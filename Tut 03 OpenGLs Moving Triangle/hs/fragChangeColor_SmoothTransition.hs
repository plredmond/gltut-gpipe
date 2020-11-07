{-# LANGUAGE LambdaCase, NegativeLiterals, NamedFieldPuns #-} -- syntax niceties
{-# LANGUAGE TypeFamilies #-} -- gpipe requirements

import Control.Monad.IO.Class (liftIO)
import System.Environment (getProgName)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Lens as Lens

import Graphics.GPipe -- unqualified
import Graphics.GPipe.Context.GLFW (Handle)
import qualified Graphics.GPipe.Context.GLFW as GLFW

main :: IO ()
main = runContextT GLFW.defaultHandleConfig $ do
    -- make a window
    win <- newWindow (WindowFormatColor RGBA8) . GLFW.defaultWindowConfig =<< liftIO getProgName
    -- hook up to receive ESC key and window-close events
    close <- liftIO $ MVar.newEmptyMVar
    _ <- GLFW.setWindowCloseCallback win . Just $
        MVar.tryPutMVar close "window closed" >> return ()
    _ <- GLFW.setKeyCallback win . Just $ \k _ ks _ -> case (k, ks) of
        (GLFW.Key'Escape, GLFW.KeyState'Pressed) -> MVar.tryPutMVar close "escape key" >> return ()
        _ -> return ()
    -- initializeProgram
    prog <- compileShader shaderCode
    buff <- initializeVertexBuffer
    unif <- newBuffer 3 -- If this is too small you might see "fragChangeColor: writeBuffer, offset out of bounds"
    writeBuffer unif loopDurUnifOffset [5]
    writeBuffer unif fragLoopDurUnifOffset [10]
    -- framework
    loop close win unif buff prog
  where
    loop close win unif buff prog = (liftIO $ MVar.tryReadMVar close) >>= \case
        Just msg -> liftIO . putStrLn $ "stopping because: " ++ msg
        Nothing -> display win unif buff prog >> loop close win unif buff prog

loopDurUnifOffset :: BufferStartPos
loopDurUnifOffset = 0

secondsUnifOffset :: BufferStartPos
secondsUnifOffset = 1

fragLoopDurUnifOffset :: BufferStartPos
fragLoopDurUnifOffset = 2

initializeVertexBuffer :: ContextT Handle os IO (Buffer os (B4 Float))
initializeVertexBuffer = do
    positionBufferObject <- newBuffer $ length vertexPositions
    writeBuffer positionBufferObject 0 vertexPositions
    return positionBufferObject

vertexPositions :: [V4 Float]
vertexPositions =
    [ V4  0.25  0.25 0 1
    , V4  0.25 -0.25 0 1
    , V4 -0.25 -0.25 0 1
    ]

data ShaderEnv os = ShaderEnv
    { getBUnif :: Buffer os (Uniform (B Float)) -- All the singleton uniforms can share the same getter here.
    , getPrimArr :: PrimitiveArray Triangles (B4 Float)
    , getRastOpt :: (Side, ViewPort, DepthRange)
    , getDrawOpt :: (Window os RGBAFloat (), ContextColorOption RGBAFloat)
    }
shaderCode :: Shader os (ShaderEnv os) ()
shaderCode = do
    -- Get the loop duration and the frag-loop duration. These gets are sorted
    -- out and actually end up happening in separate shaders.
    loopDur     <- getUniform $ \ShaderEnv{getBUnif} -> (getBUnif,     loopDurUnifOffset)
    fragLoopDur <- getUniform $ \ShaderEnv{getBUnif} -> (getBUnif, fragLoopDurUnifOffset)
    -- Get the elapsed-seconds twice. The first is inferred to `S V Float` and
    -- the second to `S F Float`, as they're used in the vertex and fragment
    -- shader respectively.
    seconds     <- getUniform $ \ShaderEnv{getBUnif} -> (getBUnif,     secondsUnifOffset)
    fragSeconds <- getUniform $ \ShaderEnv{getBUnif} -> (getBUnif,     secondsUnifOffset)
    let offsets = computePositionOffsets loopDur seconds
        vertShader pos = (Lens.over _xy (+offsets) pos, ()) -- no input to be interpolated by fragment shader
        fragShader () = computeColor fragLoopDur fragSeconds
    primStream <- toPrimitiveStream getPrimArr
    fragStream <- rasterize getRastOpt $ fmap vertShader primStream
    drawWindowColor getDrawOpt $ fmap fragShader fragStream

display
    :: Window os RGBAFloat ()
    -> Buffer os (Uniform (B Float))
    -> Buffer os (B4 Float)
    -> CompiledShader os (ShaderEnv os)
    -> ContextT Handle os IO ()
display win singletonUnif vertexBuffer shaderProg = do
    adjustSecondsUniform
    Just (x, y) <- GLFW.getWindowSize win -- whereas gltut uses a reshape callback
    render $ do
        clearWindowColor win (V4 0 0 0 0)
        vertexArray <- newVertexArray vertexBuffer
        shaderProg ShaderEnv
            { getBUnif = singletonUnif
            , getPrimArr = toPrimitiveArray TriangleList vertexArray
            , getRastOpt = (FrontAndBack, ViewPort (V2 0 0) (V2 x y), DepthRange 0 1)
            , getDrawOpt = (win, ContextColorOption NoBlending (V4 True True True True))
            }
    swapWindowBuffers win
  where
    adjustSecondsUniform = do
        seconds <- liftIO GLFW.getTime
        writeBuffer singletonUnif secondsUnifOffset [maybe 0 realToFrac seconds]

computePositionOffsets :: Real' a => a -> a -> V2 a
computePositionOffsets loopDuration elapsedTime = V2
    (0.5 * cos (currTimeThroughLoop * scale))
    (0.5 * sin (currTimeThroughLoop * scale))
  where
    scale = pi * 2 / loopDuration
    currTimeThroughLoop = mod'' elapsedTime loopDuration

computeColor :: Real' a => a -> a -> V4 a
computeColor fragLoopDuration elapsedTime =
    mix 1 (V4 0 1 0 1) (pure currLerp)
  where
    currLerp = - cos (elapsedTime * 2 * pi / fragLoopDuration) + 1

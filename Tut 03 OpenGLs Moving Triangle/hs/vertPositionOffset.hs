{-# LANGUAGE LambdaCase, NegativeLiterals #-} -- syntax niceties
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
    unif <- newBuffer 1
    -- framework
    loop close win unif buff prog
  where
    loop close win unif buff prog = (liftIO $ MVar.tryReadMVar close) >>= \case
        Just msg -> liftIO . putStrLn $ "stopping because: " ++ msg
        Nothing -> display win unif buff prog >> loop close win unif buff prog

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

-- | This shader env became a bit bigger in this tutoral. It's starting to be
-- ungainly. In the next tutorial it'll be converted to a datatype.
type ShaderEnv os =
    ( Window os RGBAFloat ()
    , (Buffer os (Uniform (B2 Float)), Int) -- Offsets uniform
    , PrimitiveArray Triangles (B4 Float) -- Vertex buffer
    , V2 Int -- Viewport size
    )
shaderCode :: Shader os (ShaderEnv os) ()
shaderCode = do
    offsets <- getUniform getOffsets
    let vertShader pos = (Lens.over _xy (+offsets) pos, ()) -- no input to be interpolated by fragment shader
        fragShader () = 1
    primStream <- toPrimitiveStream getPrimArr
    fragStream <- rasterize getRastOpt $ fmap vertShader primStream
    drawWindowColor getDrawOpt $ fmap fragShader fragStream
  where
    -- In the next tutorial, these functions will become fields in the new
    -- shader env datatype.
    getPrimArr (_, _, arr, _) = arr
    getRastOpt (_, _, _, siz) = (FrontAndBack, ViewPort (V2 0 0) siz, DepthRange 0 1)
    getDrawOpt (win, _, _, _) = (win, ContextColorOption NoBlending (V4 True True True True))
    getOffsets (_, off, _, _) = off

display
    :: Window os RGBAFloat ()
    -> Buffer os (Uniform (B2 Float))
    -> Buffer os (B4 Float)
    -> CompiledShader os (ShaderEnv os)
    -> ContextT Handle os IO ()
display win offsetUnif vertexBuffer shaderProg = do
    adjustOffsetUniform
    Just (x, y) <- GLFW.getWindowSize win -- whereas gltut uses a reshape callback
    render $ do
        clearWindowColor win (V4 0 0 0 0)
        vertexArray <- newVertexArray vertexBuffer
        shaderProg
            ( win
            , (offsetUnif, 0)
            , toPrimitiveArray TriangleList vertexArray
            , V2 x y
            )
    swapWindowBuffers win
  where
    adjustOffsetUniform = do
        seconds <- liftIO GLFW.getTime
        let offsets = computePositionOffsets $ maybe 0 realToFrac seconds
        writeBuffer offsetUnif 0 [offsets]

computePositionOffsets :: Real' a => a -> V2 a
computePositionOffsets elapsedTime = V2
    (0.5 * cos (currTimeThroughLoop * scale))
    (0.5 * sin (currTimeThroughLoop * scale))
  where
    loopDuration = 5
    scale = pi * 2 / loopDuration
    currTimeThroughLoop = mod'' elapsedTime loopDuration

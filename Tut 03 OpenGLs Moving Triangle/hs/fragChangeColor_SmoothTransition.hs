{-# LANGUAGE LambdaCase, NegativeLiterals, NamedFieldPuns #-} -- syntax niceties
{-# LANGUAGE TypeFamilies #-} -- gpipe requirements

import Control.Monad.IO.Class (liftIO)
import qualified Control.Lens as Lens

import Graphics.GPipe -- unqualified
import Graphics.GPipe.Context.GLFW (Handle)
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Graphics.GLTut.Framework as FW

main :: IO ()
main = runContextT GLFW.defaultHandleConfig $ do
    _ <- FW.main (WindowFormatColor RGBA8) initialize display keyboard reshape
    return ()

data Unifs os = Unifs
    { elapsedSecUniform :: (Buffer os (Uniform (B Float)), BufferStartPos)
    , loopDurationUnf :: (Buffer os (Uniform (B Float)), BufferStartPos)
    , fragLoopDurUnf :: (Buffer os (Uniform (B Float)), BufferStartPos)
    }

data ShaderEnv os = ShaderEnv
    { getUnifs :: Unifs os
    , getPrimArr :: PrimitiveArray Triangles (B4 Float)
    , getRastOpt :: (Side, ViewPort, DepthRange)
    , getDrawOpt :: (Window os RGBAFloat (), ContextColorOption RGBAFloat)
    }

data Env os = Env
    { vertexBuff :: Buffer os (B4 Float)
    , shaderProg :: CompiledShader os (ShaderEnv os)
    , viewport :: ViewPort
    , uniforms :: Unifs os
    }

initialize :: Window os RGBAFloat () -> [String] -> ContextT Handle os IO (Env os)
initialize _win _args = do
    theProgram <- compileShader shaderCode
    positionBufferObject <- newBuffer $ length vertexPositions
    writeBuffer positionBufferObject 0 vertexPositions
    unifs <- do
        -- When a buffer is too small, you'll get a runtime crash with an error
        -- like "fragChangeColor: writeBuffer, offset out of bounds".
        unifBF <- newBuffer 3
        writeBuffer unifBF loopDurationUnfOfs [5]
        writeBuffer unifBF fragLoopDurUnfOfs [10]
        -- The elapsed-seconds uniform will be written later.
        return Unifs
            { elapsedSecUniform = (unifBF, elapsedSecUniformOfs)
            , loopDurationUnf = (unifBF, loopDurationUnfOfs)
            , fragLoopDurUnf = (unifBF, fragLoopDurUnfOfs)
            }
    return $ Env positionBufferObject theProgram (ViewPort 0 0) unifs
  where
    elapsedSecUniformOfs = 0
    loopDurationUnfOfs = 1
    fragLoopDurUnfOfs = 2

display :: Window os RGBAFloat () -> Env os -> ContextT Handle os IO (Env os)
display win env = do
    adjustSecondsUniform $ uniforms env
    render $ do
        clearWindowColor win 0
        vertexArray <- newVertexArray $ vertexBuff env
        (shaderProg env) ShaderEnv
            { getUnifs = uniforms env
            , getPrimArr = toPrimitiveArray TriangleList vertexArray
            , getRastOpt = (FrontAndBack, viewport env, DepthRange 0 1)
            , getDrawOpt = (win, ContextColorOption NoBlending (pure True))
            }
    swapWindowBuffers win
    return env
  where
    adjustSecondsUniform Unifs{elapsedSecUniform=(buff, offset)} = do
        seconds <- liftIO GLFW.getTime
        writeBuffer buff offset [maybe 0 realToFrac seconds]

shaderCode :: Shader os (ShaderEnv os) ()
shaderCode = do
    -- Get the loop duration and the frag-loop duration. These gets are sorted
    -- out and actually end up happening in separate shaders.
    loopDur     <- getUniform $ loopDurationUnf . getUnifs
    fragLoopDur <- getUniform $ fragLoopDurUnf . getUnifs
    -- Get the elapsed-seconds twice. The first is inferred to `S V Float` and
    -- the second to `S F Float`, as they're used in the vertex and fragment
    -- shader respectively.
    seconds     <- getUniform $ elapsedSecUniform . getUnifs
    fragSeconds <- getUniform $ elapsedSecUniform . getUnifs
    let offsets = computePositionOffsets loopDur seconds
        vertShader pos = (Lens.over _xy (+ offsets) pos, ()) -- no input to be interpolated by fragment shader
        fragShader () = computeColor fragLoopDur fragSeconds
    primStream <- toPrimitiveStream getPrimArr
    fragStream <- rasterize getRastOpt $ fmap vertShader primStream
    drawWindowColor getDrawOpt $ fmap fragShader fragStream

keyboard :: Window os RGBAFloat () -> Env os -> GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> ContextT Handle os IO (Env os)
keyboard _win env _key _keyState _modKeys = return env

reshape :: Window os RGBAFloat () -> Env os -> V2 Int -> ContextT Handle os IO (Env os)
reshape _win env size = return env{viewport=ViewPort 0 size}

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

vertexPositions :: [V4 Float]
vertexPositions =
    [ V4  0.25  0.25 0 1
    , V4  0.25 -0.25 0 1
    , V4 -0.25 -0.25 0 1
    ]

{-# LANGUAGE LambdaCase, NegativeLiterals #-} -- syntax niceties
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

-- | Unifs is now a datatype because we're juggling mulitple uniforms.
data Unifs os = Unifs
    { elapsedSecUniform :: (Buffer os (Uniform (B Float)), BufferStartPos)
    , loopDurationUnf :: (Buffer os (Uniform (B Float)), BufferStartPos)
    }

-- | The new shader env datatype is mostly a noop change. The fields in this
-- env datatype have the same names and types as the functions they replaced.
-- Some options previously hardcoded into the shader are instead hardcoded into
-- the display function and passed to the shader via this env datatype.
data ShaderEnv os = ShaderEnv
    { getUnifs :: Unifs os
    , getPrimArr :: PrimitiveArray Triangles (B4 Float)
    , getRastOpt :: (Side, ViewPort, DepthRange)
    , getDrawOpt :: (Window os RGBAFloat (), ContextColorOption RGBAFloat)
    }

-- | The new resources env datatype means the framework callbacks no longer
-- pack and unpack tuples. When you write a GPipe program from scratch, this
-- probably isn't necessary. Also note, the names of the fields here don't
-- match the global variables in the cpp code.
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
        -- Since they have the same type, both the loop-duration and the
        -- elapsed-seconds uniforms will be stored in the same buffer.
        unifBF <- newBuffer 2
        writeBuffer unifBF loopDurationUnfOfs [5]
        -- The elapsed-seconds uniform will be written later.
        return Unifs
            { elapsedSecUniform = (unifBF, elapsedSecUniformOfs)
            , loopDurationUnf = (unifBF, loopDurationUnfOfs)
            }
    return $ Env positionBufferObject theProgram (ViewPort 0 0) unifs
  where
    elapsedSecUniformOfs = 0
    loopDurationUnfOfs = 1

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
    loopDur <- getUniform $ loopDurationUnf . getUnifs
    seconds <- getUniform $ elapsedSecUniform . getUnifs
    let offsets = computePositionOffsets loopDur seconds
        offsets2 = computePositionOffsets loopDur (seconds + loopDur/2)
        vertShader offs pos = (Lens.over _xy (+ offs) pos, ()) -- no input to be interpolated by fragment shader
        fragShader () = 1
    primStream <- toPrimitiveStream getPrimArr
    fragStream <- rasterize getRastOpt
        $ fmap (vertShader offsets) primStream
        <> fmap (vertShader offsets2) primStream
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

vertexPositions :: [V4 Float]
vertexPositions =
    [ V4  0.25  0.25 0 1
    , V4  0.25 -0.25 0 1
    , V4 -0.25 -0.25 0 1
    ]

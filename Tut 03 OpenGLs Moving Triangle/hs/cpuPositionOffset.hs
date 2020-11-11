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

type ShaderEnv os = (PrimitiveArray Triangles (B4 Float), ViewPort, Window os RGBAFloat ())

type Env os = (Buffer os (B4 Float), CompiledShader os (ShaderEnv os), ViewPort)

initialize :: Window os RGBAFloat () -> [String] -> ContextT Handle os IO (Env os)
initialize _win _args = do
    theProgram <- compileShader shaderCode
    positionBufferObject <- newBuffer $ length vertexPositions
    writeBuffer positionBufferObject 0 vertexPositions
    return (positionBufferObject, theProgram, ViewPort 0 0)

display :: Window os RGBAFloat () -> Env os -> ContextT Handle os IO (Env os)
display win env@(positionBufferObject, theProgram, viewport) = do
    adjustVertexData
    render $ do
        clearWindowColor win 0
        vertexArray <- newVertexArray positionBufferObject
        theProgram
            (toPrimitiveArray TriangleList vertexArray, viewport, win)
    swapWindowBuffers win
    return env
  where
    adjustVertexData = do
        seconds <- liftIO GLFW.getTime
        let offsets = computePositionOffsets $ maybe 0 realToFrac seconds
        writeBuffer positionBufferObject 0 $ fmap (Lens.over _xy (+offsets)) vertexPositions

shaderCode :: Shader os (ShaderEnv os) ()
shaderCode = do
    let vertShader pos = (pos, ()) -- no input to be interpolated by fragment shader
        fragShader () = 1 -- hardcoded color & inferred color-format
    primStream <- toPrimitiveStream getPrimArr
    fragStream <- rasterize getRastOpt $ fmap vertShader primStream
    drawWindowColor getDrawOpt $ fmap fragShader fragStream
  where
    getPrimArr (arr, _, _) = arr
    getRastOpt (_, vpt, _) = (FrontAndBack, vpt, DepthRange 0 1)
    getDrawOpt (_, _, win) = (win, ContextColorOption NoBlending (pure True))

keyboard :: Window os RGBAFloat () -> Env os -> GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> ContextT Handle os IO (Env os)
keyboard _win env _key _keyState _modKeys = return env

reshape :: Window os RGBAFloat () -> Env os -> V2 Int -> ContextT Handle os IO (Env os)
reshape _win (buff, prog, _) size = return (buff, prog, ViewPort 0 size)

computePositionOffsets :: Real' a => a -> V2 a
computePositionOffsets elapsedTime = V2
    (0.5 * cos (currTimeThroughLoop * scale))
    (0.5 * sin (currTimeThroughLoop * scale))
  where
    loopDuration = 5
    scale = pi * 2 / loopDuration
    currTimeThroughLoop = mod'' elapsedTime loopDuration

vertexPositions :: [V4 Float]
vertexPositions =
    [ V4  0.25  0.25 0 1
    , V4  0.25 -0.25 0 1
    , V4 -0.25 -0.25 0 1
    ]

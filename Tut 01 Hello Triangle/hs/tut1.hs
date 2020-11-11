{-# LANGUAGE LambdaCase, NegativeLiterals #-} -- syntax niceties
{-# LANGUAGE TypeFamilies #-} -- gpipe requirements

import Graphics.GPipe -- unqualified
import Graphics.GPipe.Context.GLFW (Handle)
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Graphics.GLTut.Framework as FW

main :: IO ()
main = runContextT GLFW.defaultHandleConfig
    $ FW.main (WindowFormatColor RGBA8) initialize display keyboard reshape
    >> return ()

-- | ShaderEnv is the type of resources used by the shader code on the graphics
-- card.
type ShaderEnv os = (PrimitiveArray Triangles (B4 Float), ViewPort, Window os RGBAFloat ())

-- | Env is the type of resources used by the display function and other
-- callbacks in the application.
type Env os = (Buffer os (B4 Float), CompiledShader os (ShaderEnv os), ViewPort)

-- | Initialize the Env.
initialize :: Window os RGBAFloat () -> [String] -> ContextT Handle os IO (Env os)
initialize _win _args = do
    theProgram <- compileShader shaderCode
    positionBufferObject <- newBuffer $ length vertexPositions
    writeBuffer positionBufferObject 0 vertexPositions
    return (positionBufferObject, theProgram, ViewPort 0 0)
  where
    vertexPositions =
        [ V4  0.75  0.75 0 1
        , V4  0.75 -0.75 0 1
        , V4 -0.75 -0.75 0 1
        ]

display :: Window os RGBAFloat () -> Env os -> ContextT Handle os IO (Env os)
display win st@(positionBufferObject, theProgram, viewPort) = do
    render $ do
        clearWindowColor win (V4 0 0 0 0)
        vertexArray <- newVertexArray positionBufferObject
        theProgram
            (toPrimitiveArray TriangleList vertexArray, viewPort, win)
    swapWindowBuffers win
    return st

-- | Shader code which (1) gets the vertex-array as a primitive-stream, (2)
-- maps a noop vertex-shader over it, (3) rasterizes to a fragment-stream, (4)
-- maps the noop fragment-shader over it, and (5) draws to the specified
-- window.
shaderCode :: Shader os (ShaderEnv os) ()
shaderCode = do
    primStream <- toPrimitiveStream getPrimArr
    fragStream <- rasterize getRastOpt $ fmap vertShader primStream
    drawWindowColor getDrawOpt $ fmap fragShader fragStream
  where
    getPrimArr (arr, _, _) = arr
    getRastOpt (_, vpt, _) = (FrontAndBack, vpt, DepthRange 0 1)
    getDrawOpt (_, _, win) = (win, ContextColorOption NoBlending (pure True))
    vertShader pos = (pos, ()) -- no input to be interpolated by fragment shader
    fragShader () = V4 1 1 1 1 -- hardcoded color & color-format

keyboard :: Window os RGBAFloat () -> Env os -> GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> ContextT Handle os IO (Env os)
keyboard _win st _key _keyState _modKeys = return st

reshape :: Window os RGBAFloat () -> Env os -> V2 Int -> ContextT Handle os IO (Env os)
reshape _win (buff, prog, _) size = return (buff, prog, ViewPort 0 size)

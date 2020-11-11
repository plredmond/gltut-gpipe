{-# LANGUAGE LambdaCase, NegativeLiterals #-} -- syntax niceties
{-# LANGUAGE TypeFamilies #-} -- gpipe requirements

import Graphics.GPipe -- unqualified
import Graphics.GPipe.Context.GLFW (Handle)
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Graphics.GLTut.Framework as FW

main :: IO ()
main = runContextT GLFW.defaultHandleConfig $ do
    _ <- FW.main (WindowFormatColor RGBA8) initialize display keyboard reshape
    return ()

type ShaderEnv os = (PrimitiveArray Triangles (B4 Float, B4 Float), ViewPort, Window os RGBAFloat ())

type Env os = (Buffer os (B4 Float), CompiledShader os (ShaderEnv os), ViewPort)

initialize :: Window os RGBAFloat () -> [String] -> ContextT Handle os IO (Env os)
initialize _win _args = do
    theProgram <- compileShader shaderCode
    vertexBufferObject <- newBuffer $ length vertexData
    writeBuffer vertexBufferObject 0 vertexData
    return (vertexBufferObject, theProgram, ViewPort 0 0)
  where
    vertexData =
        -- position data
        [ V4  0.75  0.75 0 1
        , V4  0.75 -0.75 0 1
        , V4 -0.75 -0.75 0 1
        -- color data
        , V4  1.0  0.0   0 1
        , V4  0.0  1.0   0 1
        , V4  0.0  0.0   1 1
        ]

display :: Window os RGBAFloat () -> Env os -> ContextT Handle os IO (Env os)
display win env@(vertexBufferObject, theProgram, viewport) = do
    render $ do
        clearWindowColor win 0
        vertexArray <- newVertexArray vertexBufferObject
        let posColArr = zipVertices (,)
                (takeVertices half vertexArray)
                (dropVertices half vertexArray :: VertexArray () (B4 Float))
        theProgram
            (toPrimitiveArray TriangleList posColArr, viewport, win)
    swapWindowBuffers win
    return env
  where
    half = 3

shaderCode :: Shader os (ShaderEnv os) ()
shaderCode = do
    let vertShader = id
        fragShader col RasterizedInfo{rasterizedFragCoord=V4 _ y _ _} =
            let lerpValue = y / 500
                grey = mix 1 (V4 0.2 0.2 0.2 1) (pure lerpValue)
            in grey * col
    primStream <- toPrimitiveStream getPrimArr
    fragStream <- rasterize getRastOpt $ fmap vertShader primStream
    drawWindowColor getDrawOpt $ withRasterizedInfo fragShader fragStream
  where
    getPrimArr (arr, _, _) = arr
    getRastOpt (_, vpt, _) = (FrontAndBack, vpt, DepthRange 0 1)
    getDrawOpt (_, _, win) = (win, ContextColorOption NoBlending (pure True))

keyboard :: Window os RGBAFloat () -> Env os -> GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> ContextT Handle os IO (Env os)
keyboard _win env _key _keyState _modKeys = return env

reshape :: Window os RGBAFloat () -> Env os -> V2 Int -> ContextT Handle os IO (Env os)
reshape _win (buff, prog, _) size = return (buff, prog, ViewPort 0 size)

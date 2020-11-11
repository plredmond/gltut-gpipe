{-# LANGUAGE LambdaCase, NegativeLiterals, NamedFieldPuns #-} -- syntax niceties
{-# LANGUAGE TypeFamilies #-} -- gpipe requirements

import qualified Control.Lens as Lens

import Graphics.GPipe -- unqualified
import Graphics.GPipe.Context.GLFW (Handle)
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Graphics.GLTut.Framework as FW

main :: IO ()
main = runContextT GLFW.defaultHandleConfig
    $ FW.main (WindowFormatColor RGBA8) initialize display keyboard reshape
    >> return ()

data Unifs os = Unifs
    { offsetUniform :: (Buffer os (Uniform (B2 Float)), BufferStartPos)
    , perspectiveMatrixUnif :: (Buffer os (Uniform (V4 (B4 Float))), BufferStartPos)
    }

data ShaderEnv os = ShaderEnv
    { getUnifs :: Unifs os
    , getPrimArr :: PrimitiveArray Triangles (B4 Float, B4 Float)
    , getRastOpt :: (Side, ViewPort, DepthRange)
    , getDrawOpt :: (Window os RGBAFloat (), ContextColorOption RGBAFloat)
    }

data Env os = Env
    { vertexBuff :: Buffer os (B4 Float)
    , shaderProg :: CompiledShader os (ShaderEnv os)
    , uniforms :: Unifs os
    , viewport :: ViewPort
    }

initialize :: Window os RGBAFloat () -> [String] -> ContextT Handle os IO (Env os)
initialize _win _args = do
    theProgram <- compileShader shaderCode
    vertexBufferObject <- newBuffer $ length vertexData
    writeBuffer vertexBufferObject 0 vertexData
    unifs <- do
        unifV2 <- newBuffer 1
        -- The M44 uniform isn't initialized until `display` is called.
        unifM44 <- newBuffer 1
        -- This is adjusted in "AspectRatio.cpp" from the previous tutorial to
        -- put the object off the side of the screen, so that you must resize
        -- to test the constant aspect ratio.
        writeBuffer unifV2 0 [V2 1.5 0.5]
        return Unifs
            { offsetUniform = (unifV2, 0)
            , perspectiveMatrixUnif = (unifM44, 0)
            }
    return $ Env vertexBufferObject theProgram unifs (ViewPort 0 0)

display :: Window os RGBAFloat () -> Env os -> ContextT Handle os IO (Env os)
display win env@Env{vertexBuff, shaderProg, uniforms, viewport} = do
    render $ do
        clearWindowColor win 0
        vertexArray <- newVertexArray vertexBuff
        shaderProg ShaderEnv
            { getUnifs = uniforms
            , getPrimArr = toPrimitiveArray TriangleList $ zipVertices (,)
                (takeVertices (length vertexData `div` 2) vertexArray)
                (dropVertices (length vertexData `div` 2) vertexArray :: VertexArray () (B4 Float))
            , getRastOpt = (Back, viewport, DepthRange 0 1)
            , getDrawOpt = (win, ContextColorOption NoBlending $ pure True)
            }
    swapWindowBuffers win
    return env

shaderCode :: Shader os (ShaderEnv os) ()
shaderCode = do
    offset <- getUniform $ offsetUniform . getUnifs
    perspectiveMatrix <- getUniform $ perspectiveMatrixUnif . getUnifs
    let vertShader (pos, col) =
            let cameraPos = Lens.over _xy (+ offset) pos
            in (perspectiveMatrix !* cameraPos, col)
        fragShader col = col
    primStream <- toPrimitiveStream getPrimArr
    fragStream <- rasterize getRastOpt $ fmap vertShader primStream
    drawWindowColor getDrawOpt $ fmap fragShader fragStream

keyboard :: Window os RGBAFloat () -> Env os -> GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> ContextT Handle os IO (Env os)
keyboard _win env _key _keyState _modKeys = return env

reshape :: Window os RGBAFloat () -> Env os -> V2 Int -> ContextT Handle os IO (Env os)
reshape _win env@Env{uniforms} size = do
    updatePerspectiveMatrix size uniforms
    return env{viewport=ViewPort 0 size}

updatePerspectiveMatrix :: V2 Int -> Unifs os -> ContextT Handle os IO ()
updatePerspectiveMatrix (V2 width height) Unifs{perspectiveMatrixUnif=(unifM44,buffPos)} = do
    let frustrumScale = 1
        zNear = 0.5
        zFar = 3
        x = frustrumScale / (fromIntegral width / fromIntegral height)
        y = frustrumScale
        z = (zFar + zNear) / (zNear - zFar)
        w = (2 * zFar * zNear) / (zNear - zFar)
    writeBuffer unifM44 buffPos
        [V4 (V4 x 0  0 0)
            (V4 0 y  0 0)
            (V4 0 0  z w)
            (V4 0 0 -1 0)]

vertexData :: [V4 Float]
vertexData =
    -- position data
    [ V4  0.25  0.25 -1.25 1
    , V4  0.25 -0.25 -1.25 1
    , V4 -0.25  0.25 -1.25 1

    , V4  0.25 -0.25 -1.25 1
    , V4 -0.25 -0.25 -1.25 1
    , V4 -0.25  0.25 -1.25 1

    , V4  0.25  0.25 -2.75 1
    , V4 -0.25  0.25 -2.75 1
    , V4  0.25 -0.25 -2.75 1

    , V4  0.25 -0.25 -2.75 1
    , V4 -0.25  0.25 -2.75 1
    , V4 -0.25 -0.25 -2.75 1

    , V4 -0.25  0.25 -1.25 1
    , V4 -0.25 -0.25 -1.25 1
    , V4 -0.25 -0.25 -2.75 1

    , V4 -0.25  0.25 -1.25 1
    , V4 -0.25 -0.25 -2.75 1
    , V4 -0.25  0.25 -2.75 1

    , V4  0.25  0.25 -1.25 1
    , V4  0.25 -0.25 -2.75 1
    , V4  0.25 -0.25 -1.25 1

    , V4  0.25  0.25 -1.25 1
    , V4  0.25  0.25 -2.75 1
    , V4  0.25 -0.25 -2.75 1

    , V4  0.25  0.25 -2.75 1
    , V4  0.25  0.25 -1.25 1
    , V4 -0.25  0.25 -1.25 1

    , V4  0.25  0.25 -2.75 1
    , V4 -0.25  0.25 -1.25 1
    , V4 -0.25  0.25 -2.75 1

    , V4  0.25 -0.25 -2.75 1
    , V4 -0.25 -0.25 -1.25 1
    , V4  0.25 -0.25 -1.25 1

    , V4  0.25 -0.25 -2.75 1
    , V4 -0.25 -0.25 -2.75 1
    , V4 -0.25 -0.25 -1.25 1
    -- color data
    , V4 0   0   1   1
    , V4 0   0   1   1
    , V4 0   0   1   1

    , V4 0   0   1   1
    , V4 0   0   1   1
    , V4 0   0   1   1

    , V4 0.8 0.8 0.8 1
    , V4 0.8 0.8 0.8 1
    , V4 0.8 0.8 0.8 1

    , V4 0.8 0.8 0.8 1
    , V4 0.8 0.8 0.8 1
    , V4 0.8 0.8 0.8 1

    , V4 0   1   0   1
    , V4 0   1   0   1
    , V4 0   1   0   1

    , V4 0   1   0   1
    , V4 0   1   0   1
    , V4 0   1   0   1

    , V4 0.5 0.5 0   1
    , V4 0.5 0.5 0   1
    , V4 0.5 0.5 0   1

    , V4 0.5 0.5 0   1
    , V4 0.5 0.5 0   1
    , V4 0.5 0.5 0   1

    , V4 1   0   0   1
    , V4 1   0   0   1
    , V4 1   0   0   1

    , V4 1   0   0   1
    , V4 1   0   0   1
    , V4 1   0   0   1

    , V4 0   1   1   1
    , V4 0   1   1   1
    , V4 0   1   1   1

    , V4 0   1   1   1
    , V4 0   1   1   1
    , V4 0   1   1   1
    ]

{-# LANGUAGE LambdaCase, NegativeLiterals, NamedFieldPuns #-} -- syntax niceties
{-# LANGUAGE TypeFamilies #-} -- gpipe requirements

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
    , viewport :: ViewPort
    , uniforms :: Unifs os
    }

initialize :: Window os RGBAFloat () -> [String] -> ContextT Handle os IO (Env os)
initialize _win _args = do
    theProgram <- compileShader shaderCode
    vertexBufferObject <- newBuffer $ length vertexData
    writeBuffer vertexBufferObject 0 vertexData
    unifs <- do
        unifB2F <- newBuffer 1
        writeBuffer unifB2F offsetUniformOfs [V2 0.5 0.5]
        unifM44 <- newBuffer 1
        let frustrumScale = 1
            zNear = 0.5
            zFar = 3
            x = frustrumScale
            y = frustrumScale
            z = (zFar + zNear) / (zNear - zFar)
            w = (2 * zFar * zNear) / (zNear - zFar)
        -- "MatrixPerspective.cpp" uses a flat array of 16 values representing
        -- a 4x4 matrix in column-major order.  With GPipe we'll represent
        -- matrices in row-major order.
        writeBuffer unifM44 perspectiveMatrixUnifOfs
            [V4 (V4 x 0  0 0)
                (V4 0 y  0 0)
                (V4 0 0  z w)
                (V4 0 0 -1 0)]
        return Unifs
            { offsetUniform = (unifB2F, offsetUniformOfs)
            , perspectiveMatrixUnif = (unifM44, perspectiveMatrixUnifOfs)
            }
    return $ Env vertexBufferObject theProgram (ViewPort 0 0) unifs
  where
    offsetUniformOfs = 0
    perspectiveMatrixUnifOfs = 0

display :: Window os RGBAFloat () -> Env os -> ContextT Handle os IO (Env os)
display win env = do
    render $ do
        clearWindowColor win 0
        vertexArray <- newVertexArray $ vertexBuff env
        (shaderProg env) ShaderEnv
            { getUnifs = uniforms env
            , getPrimArr = toPrimitiveArray TriangleList $ zipVertices (,)
                (takeVertices (length vertexData `div` 2) vertexArray)
                (dropVertices (length vertexData `div` 2) vertexArray :: VertexArray () (B4 Float))
            , getRastOpt = (Back, viewport env, DepthRange 0 1)
            , getDrawOpt = (win, ContextColorOption NoBlending (pure True))
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
reshape _win env size = return env{viewport=ViewPort 0 size}

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

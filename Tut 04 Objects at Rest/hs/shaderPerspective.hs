{-# LANGUAGE LambdaCase, NegativeLiterals, NamedFieldPuns #-} -- syntax niceties
{-# LANGUAGE TypeFamilies #-} -- gpipe requirements

import Control.Lens ((^.))
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
    , frustrumScaleUnif :: (Buffer os (Uniform (B Float)), BufferStartPos)
    , zNearUnif         :: (Buffer os (Uniform (B Float)), BufferStartPos)
    , zFarUnif          :: (Buffer os (Uniform (B Float)), BufferStartPos)
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
        unifBF <- newBuffer 3
        writeBuffer unifBF 0 [1, 1, 3]
        return Unifs
            { offsetUniform = (unifB2F, offsetUniformOfs)
            , frustrumScaleUnif = (unifBF, frustrumScaleUnifOfs)
            , zNearUnif         = (unifBF, zNearUnifOfs)
            , zFarUnif          = (unifBF, zFarUnifOfs)
            }
    return $ Env vertexBufferObject theProgram (ViewPort 0 0) unifs
  where
    offsetUniformOfs = 0
    frustrumScaleUnifOfs = 0
    zNearUnifOfs = 1
    zFarUnifOfs = 2

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
    frustrumScale <- getUniform $ frustrumScaleUnif . getUnifs
    zNear         <- getUniform $ zNearUnif . getUnifs
    zFar          <- getUniform $ zFarUnif . getUnifs
    let vertShader = manualPerspective offset frustrumScale (zNear, zFar)
        fragShader col = col
    primStream <- toPrimitiveStream getPrimArr
    fragStream <- rasterize getRastOpt $ fmap vertShader primStream
    drawWindowColor getDrawOpt $ fmap fragShader fragStream

keyboard :: Window os RGBAFloat () -> Env os -> GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> ContextT Handle os IO (Env os)
keyboard _win env _key _keyState _modKeys = return env

reshape :: Window os RGBAFloat () -> Env os -> V2 Int -> ContextT Handle os IO (Env os)
reshape _win env size = return env{viewport=ViewPort 0 size}

manualPerspective :: (Fractional a) => V2 a -> a -> (a , a) -> (V4 a, V4 a) -> (V4 a, V4 a)
manualPerspective offset frustrumScale (zNear, zFar) (pos, col) = (clipPos, col)
  where
    cameraPos = Lens.over _xy (+ offset) pos
    -- This is more-or-less exactly how "ManualPerspective.vert" computes
    -- `clipPos`, through setting and then adjusting first the xy, then the z,
    -- and last the w components.
    clipPos
        = Lens.set  _w  (- cameraPos^._z)
        . Lens.over _z  (+ (2 * zNear * zFar / (zNear - zFar)))
        . Lens.set  _z  (cameraPos^._z * (zNear + zFar) / (zNear - zFar))
        . Lens.set  _xy (cameraPos^._xy ^* frustrumScale)
        $ 0 -- Inferred to (V4 a)
    -- This is an alternate way to compute `clipPos` which decomposes the same
    -- computation in a more declarative way.
--  pr1 = (zNear + zFar) / (zNear - zFar)
--  pr2 = 2 * zNear * zFar / (zNear - zFar)
--  clipPos = V4
--      (cameraPos^._x * frustrumScale)
--      (cameraPos^._y * frustrumScale)
--      (cameraPos^._z * pr1 + pr2)
--      (cameraPos^._z * -1)

-- | "The location of the prism has also changed. In the original tutorial, it
-- was located on the 0.75 range in Z. Because camera space has a very
-- different Z from clip space, this had to change. Now, the Z location of the
-- prism is between -1.25 and -2.75."
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

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
    -- init
    prog <- compileShader shaderCode
    buff <- newBuffer $ length vertexData
    writeBuffer buff 0 vertexData
    unifs <- do
        unifV2 <- newBuffer 1
        unifM44 <- newBuffer 1
        writeBuffer unifV2 0 [V2 0.5 0.5]
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
        writeBuffer unifM44 0
            [V4 (V4 x 0  0 0)
                (V4 0 y  0 0)
                (V4 0 0  z w)
                (V4 0 0 -1 0)]
        return Unifs
            { offsetUniform = (unifV2, 0)
            , perspectiveMatrixUnif = (unifM44, 0)
            }
    -- framework
    loop close win unifs buff prog
  where
    loop close win unif buff prog = (liftIO $ MVar.tryReadMVar close) >>= \case
        Just msg -> liftIO . putStrLn $ "stopping because: " ++ msg
        Nothing -> display win unif buff prog >> loop close win unif buff prog

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

display
    :: Window os RGBAFloat ()
    -> Unifs os
    -> Buffer os (B4 Float)
    -> CompiledShader os (ShaderEnv os)
    -> ContextT Handle os IO ()
display win unifs vertexBuffer shaderProg = do
    Just (x, y) <- GLFW.getWindowSize win -- whereas gltut uses a reshape callback
    render $ do
        clearWindowColor win (V4 0 0 0 0)
        vertexArray <- newVertexArray vertexBuffer
        shaderProg ShaderEnv
            { getUnifs = unifs
            , getPrimArr = toPrimitiveArray TriangleList $ zipVertices (,)
                (takeVertices (length vertexData `div` 2) vertexArray)
                (dropVertices (length vertexData `div` 2) vertexArray :: VertexArray () (B4 Float))
            , getRastOpt = (Back, ViewPort (V2 0 0) (V2 x y), DepthRange 0 1)
            , getDrawOpt = (win, ContextColorOption NoBlending (V4 True True True True))
            }
    swapWindowBuffers win

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

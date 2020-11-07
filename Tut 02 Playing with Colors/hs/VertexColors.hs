{-# LANGUAGE LambdaCase, NegativeLiterals #-} -- syntax niceties
{-# LANGUAGE TypeFamilies #-} -- gpipe requirements

import Control.Monad.IO.Class (liftIO)
import System.Environment (getProgName)
import qualified Control.Concurrent.MVar as MVar

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
    -- framework
    loop close win buff prog
  where
    loop close win buff prog = (liftIO $ MVar.tryReadMVar close) >>= \case
        Just msg -> liftIO . putStrLn $ "stopping because: " ++ msg
        Nothing -> display win buff prog >> loop close win buff prog

initializeVertexBuffer :: ContextT Handle os IO (Buffer os (B4 Float))
initializeVertexBuffer = do
    vertexBufferObject <- newBuffer $ length vertexData
    writeBuffer vertexBufferObject 0 vertexData
    return vertexBufferObject
  where
    vertexData =
        -- position data
        [ V4  0.0  0.5   0 1
        , V4  0.5 -0.366 0 1
        , V4 -0.5 -0.366 0 1
        -- color data
        , V4  1.0  0.0   0 1
        , V4  0.0  1.0   0 1
        , V4  0.0  0.0   1 1
        ]

type ShaderEnv os = (Window os RGBAFloat (), PrimitiveArray Triangles (B4 Float, B4 Float), V2 Int)
shaderCode :: Shader os (ShaderEnv os) ()
shaderCode = do
    primStream <- toPrimitiveStream getPrimArr
    fragStream <- rasterize getRastOpt $ fmap vertShader primStream
    drawWindowColor getDrawOpt $ fmap fragShader fragStream
  where
    getPrimArr (_, arr, _) = arr
    getRastOpt (_, _, siz) = (FrontAndBack, ViewPort (V2 0 0) siz, DepthRange 0 1)
    getDrawOpt (win, _, _) = (win, ContextColorOption NoBlending (V4 True True True True))
    vertShader (pos, col) = (pos, col) -- this could be `id`
    fragShader col = col -- this could be `id`

display
    :: Window os RGBAFloat ()
    -> Buffer os (B4 Float)
    -> CompiledShader os (ShaderEnv os)
    -> ContextT Handle os IO ()
display win vertexBuffer shaderProg = do
    Just (x, y) <- GLFW.getWindowSize win -- whereas gltut uses a reshape callback
    render $ do
        clearWindowColor win (V4 0 0 0 0)
        vertexArray <- newVertexArray vertexBuffer
        let posColArr = zipVertices (,)
                (takeVertices 3 vertexArray)
                (dropVertices 3 vertexArray :: VertexArray () (B4 Float))
        shaderProg
            (win, toPrimitiveArray TriangleList posColArr, V2 x y)
    swapWindowBuffers win

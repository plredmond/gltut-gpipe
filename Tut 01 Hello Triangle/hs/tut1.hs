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
    positionBufferObject <- newBuffer $ length vertexPositions
    writeBuffer positionBufferObject 0 vertexPositions
    return positionBufferObject
  where
    vertexPositions =
        [ V4  0.75  0.75 0 1
        , V4  0.75 -0.75 0 1
        , V4 -0.75 -0.75 0 1
        ]

-- | Somewhat minimal shader which gets the (single) vertex array as a
-- primitive stream, maps a noop vertex-shader over it, rasterizes to a
-- fragment stream, maps the noop fragment-shader over it, and draws to the
-- window specified by the environment.
type ShaderEnv os = (Window os RGBAFloat (), PrimitiveArray Triangles (B4 Float), V2 Int)
shaderCode :: Shader os (ShaderEnv os) ()
shaderCode = do
    primStream <- toPrimitiveStream getPrimArr
    fragStream <- rasterize getRastOpt $ fmap vertShader primStream
    drawWindowColor getDrawOpt $ fmap fragShader fragStream
  where
    getPrimArr (_, arr, _) = arr
    getRastOpt (_, _, siz) = (FrontAndBack, ViewPort (V2 0 0) siz, DepthRange 0 1)
    getDrawOpt (win, _, _) = (win, ContextColorOption NoBlending (V4 True True True True))
    vertShader pos = (pos, ()) -- no input to be interpolated by fragment shader
    fragShader () = V4 1 1 1 1 -- hardcoded color & color-format

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
        shaderProg
            (win, toPrimitiveArray TriangleList vertexArray, V2 x y)
    swapWindowBuffers win

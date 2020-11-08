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
    unif <- newBuffer 1
    writeBuffer unif 0 [V2 0.5 0.25]
    -- framework
    loop close win (unif, 0) buff prog
  where
    loop close win unif buff prog = (liftIO $ MVar.tryReadMVar close) >>= \case
        Just msg -> liftIO . putStrLn $ "stopping because: " ++ msg
        Nothing -> display win unif buff prog >> loop close win unif buff prog

data ShaderEnv os = ShaderEnv
    { getOffset :: (Buffer os (Uniform (B2 Float)), BufferStartPos)
    , getPrimArr :: PrimitiveArray Triangles (B4 Float, B4 Float)
    , getRastOpt :: (Side, ViewPort, DepthRange)
    , getDrawOpt :: (Window os RGBAFloat (), ContextColorOption RGBAFloat)
    }
shaderCode :: Shader os (ShaderEnv os) ()
shaderCode = do
    offset <- getUniform getOffset
    let vertShader (pos, col) = (Lens.over _xy (+ offset) pos, col)
        fragShader col = col
    primStream <- toPrimitiveStream getPrimArr
    fragStream <- rasterize getRastOpt $ fmap vertShader primStream
    drawWindowColor getDrawOpt $ fmap fragShader fragStream

display
    :: Window os RGBAFloat ()
    -> (Buffer os (Uniform (B2 Float)), BufferStartPos)
    -> Buffer os (B4 Float)
    -> CompiledShader os (ShaderEnv os)
    -> ContextT Handle os IO ()
display win offsetUnif vertexBuffer shaderProg = do
    Just (x, y) <- GLFW.getWindowSize win -- whereas gltut uses a reshape callback
    render $ do
        clearWindowColor win (V4 0 0 0 0)
        vertexArray <- newVertexArray vertexBuffer
        shaderProg ShaderEnv
            { getOffset = offsetUnif
            , getPrimArr = toPrimitiveArray TriangleList $ zipVertices (,)
                (takeVertices (length vertexData `div` 2) vertexArray)
                (dropVertices (length vertexData `div` 2) vertexArray :: VertexArray () (B4 Float))
            -- The cpp code sets `glFrontFace(GL_CW)` and
            -- `glCullFace(GL_BACK)`, meaning the polygons in `vertexData` are
            -- clockwise-winding.  The docs say "By default, counterclockwise
            -- polygons are taken to be front-facing" at
            -- <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFrontFace.xhtml>.
            -- GPipe hardcodes this default and calls counterclockwise-winding
            -- `Front::Side` and clockwise-winding `Back::Side`. To render the
            -- clockwise-winding polygons in `vertexData` here we specify
            -- `Back::Side`.
            , getRastOpt = (Back, ViewPort (V2 0 0) (V2 x y), DepthRange 0 1)
            , getDrawOpt = (win, ContextColorOption NoBlending (V4 True True True True))
            }
    swapWindowBuffers win

vertexData :: [V4 Float]
vertexData =
    -- position data
    [ V4  0.25  0.25  0.75 1
    , V4  0.25 -0.25  0.75 1
    , V4 -0.25  0.25  0.75 1

    , V4  0.25 -0.25  0.75 1
    , V4 -0.25 -0.25  0.75 1
    , V4 -0.25  0.25  0.75 1

    , V4  0.25  0.25 -0.75 1
    , V4 -0.25  0.25 -0.75 1
    , V4  0.25 -0.25 -0.75 1

    , V4  0.25 -0.25 -0.75 1
    , V4 -0.25  0.25 -0.75 1
    , V4 -0.25 -0.25 -0.75 1

    , V4 -0.25  0.25  0.75 1
    , V4 -0.25 -0.25  0.75 1
    , V4 -0.25 -0.25 -0.75 1

    , V4 -0.25  0.25  0.75 1
    , V4 -0.25 -0.25 -0.75 1
    , V4 -0.25  0.25 -0.75 1

    , V4  0.25  0.25  0.75 1
    , V4  0.25 -0.25 -0.75 1
    , V4  0.25 -0.25  0.75 1

    , V4  0.25  0.25  0.75 1
    , V4  0.25  0.25 -0.75 1
    , V4  0.25 -0.25 -0.75 1

    , V4  0.25  0.25 -0.75 1
    , V4  0.25  0.25  0.75 1
    , V4 -0.25  0.25  0.75 1

    , V4  0.25  0.25 -0.75 1
    , V4 -0.25  0.25  0.75 1
    , V4 -0.25  0.25 -0.75 1

    , V4  0.25 -0.25 -0.75 1
    , V4 -0.25 -0.25  0.75 1
    , V4  0.25 -0.25  0.75 1

    , V4  0.25 -0.25 -0.75 1
    , V4 -0.25 -0.25 -0.75 1
    , V4 -0.25 -0.25  0.75 1
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

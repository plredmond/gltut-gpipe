{-# LANGUAGE LambdaCase, NegativeLiterals, NamedFieldPuns #-} -- syntax niceties
{-# LANGUAGE TypeFamilies #-} -- gpipe requirements

import Control.Monad.IO.Class (liftIO)
import System.Environment (getProgName)
import qualified Control.Concurrent.MVar as MVar

import Control.Lens ((^.))
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
        unif <- newBuffer 3
        writeBuffer unifV2 0 [V2 0.5 0.5]
        writeBuffer unif 0 [1, 1, 3]
        return Unifs
            { offsetUniform = (unifV2, 0)
            , frustrumScaleUnif = (unif, 0)
            , zNearUnif         = (unif, 1)
            , zFarUnif          = (unif, 2)
            }
    -- framework
    loop close win unifs buff prog
  where
    loop close win unif buff prog = (liftIO $ MVar.tryReadMVar close) >>= \case
        Just msg -> liftIO . putStrLn $ "stopping because: " ++ msg
        Nothing -> display win unif buff prog >> loop close win unif buff prog

-- | There are eough uniforms in this tutorial that they probably deserve their
-- own data type.
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

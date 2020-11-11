{-# LANGUAGE LambdaCase, NegativeLiterals #-} -- syntax niceties
{-# LANGUAGE TypeFamilies #-} -- gpipe requirements

import Control.Monad.IO.Class (liftIO)
import qualified Control.Lens as Lens

import Graphics.GPipe -- unqualified
import Graphics.GPipe.Context.GLFW (Handle)
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Graphics.GLTut.Framework as FW

main :: IO ()
main = runContextT GLFW.defaultHandleConfig $ do
    _ <- FW.main (WindowFormatColor RGBA8) initialize display keyboard reshape
    return ()

-- | Unifs is the type of uniform buffers and offsets into them.
type Unifs os = (Buffer os (Uniform (B2 Float)), BufferStartPos)

-- | This shader env became a bit bigger in this tutoral. It's starting to be
-- ungainly. In the next tutorial it'll be converted to a datatype.
type ShaderEnv os =
    ( Unifs os
    , PrimitiveArray Triangles (B4 Float)
    , ViewPort
    , Window os RGBAFloat ()
    )

-- | Similarly, this resources env is getting too big and will become a
-- datatype.
type Env os =
    ( Unifs os
    , Buffer os (B4 Float)
    , CompiledShader os (ShaderEnv os)
    , ViewPort
    )

initialize :: Window os RGBAFloat () -> [String] -> ContextT Handle os IO (Env os)
initialize _win _args = do
    theProgram <- compileShader shaderCode
    positionBufferObject <- newBuffer $ length vertexPositions
    writeBuffer positionBufferObject 0 vertexPositions
    offsetLocation <- newBuffer 1
    let unifs = (offsetLocation, 0)
    return (unifs, positionBufferObject, theProgram, ViewPort 0 0)

display :: Window os RGBAFloat () -> Env os -> ContextT Handle os IO (Env os)
display win env@(unifs, positionBufferObject, theProgram, viewport) = do
    adjustOffsetUniform unifs
    render $ do
        clearWindowColor win 0
        vertexArray <- newVertexArray positionBufferObject
        theProgram
            ( unifs
            , toPrimitiveArray TriangleList vertexArray
            , viewport
            , win
            )
    swapWindowBuffers win
    return env
  where
    adjustOffsetUniform (offsetLocation, _) = do
        seconds <- liftIO GLFW.getTime
        let offsets = computePositionOffsets $ maybe 0 realToFrac seconds
        writeBuffer offsetLocation 0 [offsets]

shaderCode :: Shader os (ShaderEnv os) ()
shaderCode = do
    offsets <- getUniform getOffsets
    let vertShader pos = (Lens.over _xy (+offsets) pos, ()) -- no input to be interpolated by fragment shader
        fragShader () = 1
    primStream <- toPrimitiveStream getPrimArr
    fragStream <- rasterize getRastOpt $ fmap vertShader primStream
    drawWindowColor getDrawOpt $ fmap fragShader fragStream
  where
    -- In the next tutorial, these functions will become fields in the new
    -- shader env datatype.
    getOffsets (off, _, _, _) = off
    getPrimArr (_, arr, _, _) = arr
    getRastOpt (_, _, vpt, _) = (FrontAndBack, vpt, DepthRange 0 1)
    getDrawOpt (_, _, _, win) = (win, ContextColorOption NoBlending (pure True))

keyboard :: Window os RGBAFloat () -> Env os -> GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> ContextT Handle os IO (Env os)
keyboard _win env _key _keyState _modKeys = return env

reshape :: Window os RGBAFloat () -> Env os -> V2 Int -> ContextT Handle os IO (Env os)
reshape _win (unifs, buff, prog, _) size = return (unifs, buff, prog, ViewPort 0 size)

computePositionOffsets :: Real' a => a -> V2 a
computePositionOffsets elapsedTime = V2
    (0.5 * cos (currTimeThroughLoop * scale))
    (0.5 * sin (currTimeThroughLoop * scale))
  where
    loopDuration = 5
    scale = pi * 2 / loopDuration
    currTimeThroughLoop = mod'' elapsedTime loopDuration

vertexPositions :: [V4 Float]
vertexPositions =
    [ V4  0.25  0.25 0 1
    , V4  0.25 -0.25 0 1
    , V4 -0.25 -0.25 0 1
    ]

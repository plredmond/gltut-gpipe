import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, IOException)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.GPipe
import Data.Vec as V
import Prelude as P

import Load

main :: IO ()
main = do
    GLUT.getArgsAndInitialize
    n <- getProgName
    f <- parseArgs
    s <- readFile f
    newWindow
        n -- window title
        (300:.200:.()) -- desired window position
        (500:.500:.()) -- desired window size
        (displayIO $ readStream s)
        initWindow
    GLUT.mainLoop

parseArgs :: IO (String)
parseArgs = do
    filename <- catch extract handle
    return filename
    where
        extract :: IO (String)
        extract = do
            [filename] <- getArgs
            return filename
        handle :: IOException -> IO String
        handle e = do
            n <- getProgName
            hPutStrLn stderr $ "USAGE: " ++ n ++ " filename"
            exitFailure

initWindow :: GLUT.Window -> IO ()
initWindow w = do
    GLUT.idleCallback GLUT.$= Just (GLUT.postRedisplay $ Just w)
    GLUT.keyboardMouseCallback GLUT.$= Just onKeyMouse
    where
        onKeyMouse :: GLUT.Key -> GLUT.KeyState -> GLUT.Modifiers -> GLUT.Position -> IO ()
        onKeyMouse (GLUT.Char '\ESC') GLUT.Down _ _ = do GLUT.leaveMainLoop
        onKeyMouse _ _ _ _ = do return ()

displayIO :: PrimitiveStream Triangle (Vec4 (Vertex Float), Vec4 (Vertex Float)) -> Vec2 Int -> IO (FrameBuffer RGBAFormat () ())
displayIO stream size = do
    milliseconds <- GLUT.get GLUT.elapsedTime
    return $ display stream size (fromIntegral milliseconds / 1000)

display :: PrimitiveStream Triangle (Vec4 (Vertex Float), Vec4 (Vertex Float)) -> Vec2 Int -> Float -> FrameBuffer RGBAFormat () ()
display stream size sec = draw pp $ draw fragments cleared
    where
        -- draw -- curry blending mode and boolean color mask onto paintColor
        draw :: FragmentStream (Color RGBAFormat (Fragment Float))
                -> FrameBuffer RGBAFormat () ()
                -> FrameBuffer RGBAFormat () ()
        draw = paintColor NoBlending (RGBA (vec True) True)
        -- cleared -- a solid color framebuffer
        cleared :: FrameBuffer RGBAFormat () ()
        cleared = newFrameBufferColor $ RGBA (vec 0) 1
        -- fragment stream
        fragments :: FragmentStream (Color RGBAFormat (Fragment Float))
        fragments = fmap fs
                  $ rasterizeBack
                  $ fmap (vs (toGPU (0.5:.0.5:.(-2):.0:.())) (toGPU perspectiveMatrix))
                  -- Minor deviation from tutorial: We offset the Y of the vertex data by -2 here.
                  stream
        -- perspective matrix
        frustrumScale = 1
        zNear = 0.5
        zFar = 3
        width:.height:.() = V.map fromIntegral size
        (ppx:.ppy:.ppz:._:.(), pp) = perspectivePlane sec
        perspectiveMatrix :: Mat44 Float
        perspectiveMatrix =
            (((scx):.(  0):.(  0):.(ppx):.()):.
             ((  0):.(scy):.(  0):.(ppy):.()):.
             ((  0):.(  0):.(pr1):.(pr2):.()):.
             ((  0):.(  0):.(neg):.(  0):.()):.
             ())
            where
                scx = frustrumScale / (width / height)
                scy = frustrumScale
                pr1 = (zNear + zFar) / (zNear - zFar)
                pr2 = (2 * zNear * zFar) / (zNear - zFar)
                neg = -1 / (abs ppz)

-- Return the position of the center of the perspective plane and a FragmentStream of it rendered.
perspectivePlane :: Float -> (Vec4 Float, FragmentStream (Color RGBAFormat (Fragment Float)))
perspectivePlane sec = (pos, frags)
    where
        pos = append (computePositionOffsets sec) ((-1):.1:.())
        pos_unif = toGPU pos
        col_unif = toGPU $ vec 1
        pp_vs :: Vec4 (Vertex Float) -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
        pp_vs disp = (pos_unif + disp, col_unif)
        frags = fmap fs
              $ rasterizeFront
              $ fmap pp_vs
              $ toGPUStream LineStrip
              [ (-1):.(-1):.0:.0:.()
              , (-1):.( 1):.0:.0:.()
              , ( 1):.( 1):.0:.0:.()
              , ( 1):.(-1):.0:.0:.()
              , (-1):.(-1):.0:.0:.()
              ]

computePositionOffsets :: Float -> Vec2 Float
computePositionOffsets elapsedTime = (0.5 * cos (currTimeThroughLoop * scale)) :.
                                     (0.5 * sin (currTimeThroughLoop * scale)) :. ()
    where
        loopDuration = 5
        scale = pi * 2 / loopDuration
        currTimeThroughLoop = mod' elapsedTime loopDuration

vs :: Vec4 (Vertex Float) -> Mat44 (Vertex Float) -> (Vec4 (Vertex Float), Vec4 (Vertex Float)) -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
vs offset mat (pos, col) = (clipPos, col)
    where
        cameraPos = pos + offset
        clipPos = multmv mat cameraPos

fs :: Vec4 (Fragment Float) -> Color RGBAFormat (Fragment Float)
fs col = RGBA (V.take n3 col) (V.last col)

-- eof

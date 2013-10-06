import System.Environment (getArgs, getProgName)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.GPipe
import Data.Vec as V
import Prelude as P

import Load
import qualified Perspective

type Stream = PrimitiveStream Triangle (Vec4 (Vertex Float), Vec4 (Vertex Float))
type Object = (Stream, Vec4 (Vertex Float))

indexes =
    [ 0, 2, 1
    , 3, 2, 0
    , 4, 5, 6
    , 6, 7, 4
    , 8, 9, 10
    , 11, 13, 12
    , 14, 16, 15
    , 17, 16, 14 ]

main :: IO ()
main = do
    GLUT.getArgsAndInitialize
    n <- getProgName
    s_horiz <- readFile "obj-horiz.vec4"
    s_vert  <- readFile "obj-vert.vec4"
    newWindow
        n -- window title
        (300:.200:.()) -- desired window position
        (500:.500:.()) -- desired window size
        (displayIO [ (readIndexedStream s_horiz indexes, toGPU (vec 0))
                   , (readIndexedStream s_vert  indexes, toGPU (0:.0:.(-1):.0:.()))
                   ])
        initWindow
    GLUT.mainLoop

initWindow :: GLUT.Window -> IO ()
initWindow w = do
    GLUT.idleCallback GLUT.$= Just (GLUT.postRedisplay $ Just w)
    GLUT.keyboardMouseCallback GLUT.$= Just onKeyMouse
    where
        onKeyMouse :: GLUT.Key -> GLUT.KeyState -> GLUT.Modifiers -> GLUT.Position -> IO ()
        onKeyMouse (GLUT.Char '\ESC') GLUT.Down _ _ = do GLUT.leaveMainLoop
        onKeyMouse _ _ _ _ = do return ()

displayIO :: [Object] -> Vec2 Int -> IO (FrameBuffer RGBAFormat () ())
displayIO objs size = do
    return $ display objs size

display :: [Object] -> Vec2 Int -> FrameBuffer RGBAFormat () ()
display objs size = P.foldl (flip draw) cleared -- draw in-order onto the framebuffer
                  $ P.map (mkFragments matrix) objs
    where
        -- draw -- curry blending mode and boolean color mask onto paintColor
        draw :: FragmentStream (Color RGBAFormat (Fragment Float))
                -> FrameBuffer RGBAFormat () ()
                -> FrameBuffer RGBAFormat () ()
        draw = paintColor NoBlending (RGBA (vec True) True)
        -- cleared -- a solid color framebuffer
        cleared :: FrameBuffer RGBAFormat () ()
        cleared = newFrameBufferColor $ RGBA (vec 0) 1
        -- matrix is a uniform calculated every frame
        matrix = toGPU $ Perspective.m_ar 1 1 3 (V.map fromIntegral size)

mkFragments :: Mat44 (Vertex Float) -> Object -> FragmentStream (Color RGBAFormat (Fragment Float))
mkFragments matrix (stream, offset) = fmap fs
                                    $ rasterizeBack
                                    $ fmap (vs offset matrix)
                                    stream

-- Offset the position. Perform projection using the provided matrix.
vs :: Vec4 (Vertex Float) -> Mat44 (Vertex Float) -> (Vec4 (Vertex Float), Vec4 (Vertex Float)) -> (Vec4 (Vertex Float), Vec4 (Vertex Float))
vs offset mat (pos, col) = (clipPos, col)
    where
        cameraPos = pos + offset
        clipPos = multmv mat cameraPos

fs :: Vec4 (Fragment Float) -> Color RGBAFormat (Fragment Float)
fs col = RGBA (V.take n3 col) (V.last col)

-- eof

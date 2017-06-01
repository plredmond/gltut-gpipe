module Graphics.GLTut.Framework where

-- Provide a common main routine for all tutorials to use.

import qualified Graphics.GPipe as GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW

import Text.Printf (printf)

main
    :: (Char -> GL.Position -> IO ())
    -> (GPipe.Vec2 Int -> IO (GPipe.FrameBuffer c d s))
    -> (GLUT.Window -> IO ())
    -> IO ()
main keyboard display init = do
    -- initialize context
    (name, _) <- GLUT.getArgsAndInitialize
    -- create window
    let posn = 300:.200:.()
        size = 500:.500:.()
    GPipe.newWindow name posn size display $ \w -> do
        -- set window callbacks
        init w -- can't set displayCallback, reshapeCallback, or keyboardCallback
        GLUT.keyboardCallback $= (Just keyboard)
    -- gl version
    do  str <- GLUT.get GL.glVersion
        (maj, min) <- GLUT.get (GL.majorMinor GL.glVersion)
        printf "GL version: %i, %i (%s)\n" maj min str
    -- sl version
    do  str <- GLUT.get GL.shadingLanguageVersion
        (maj, min) <- GLUT.get (GL.majorMinor GL.shadingLanguageVersion)
        printf "SL version: %i, %i (%s)\n" maj min str
    -- enjoy looping
    GLUT.mainLoop

eg_keyboard :: Char -> GL.Position -> IO ()
eg_keyboard c p = do
    printf "eg_keyboard (%s) (%s)\n" (show c) (show p)
    return ()

eg_display :: GPipe.Vec2 Int -> IO (GPipe.FrameBuffer GPipe.RGBFormat () ())
eg_display s = do
    printf "eg_display (%s)\n" (show s)
    return . GPipe.newFrameBufferColor . GPipe.RGB $ 1:.0:.0.4:.()

eg_init :: GLUT.Window -> IO ()
eg_init w = do
    printf "eg_init (%s)\n" (show w)
    return ()

-- eof

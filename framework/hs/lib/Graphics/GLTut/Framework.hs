{-# LANGUAGE NamedFieldPuns, LambdaCase #-}
module Graphics.GLTut.Framework where
-- Provide a common main routine for all tutorials to use.

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad as Mon
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Linear as Lin
import qualified System.Environment as Env

import Control.Monad.IO.Class (MonadIO(liftIO))
import Graphics.GPipe

-- | Framework entry function which takes as the window format in addition to
-- user callbacks.
main
    :: (MonadIO m)
    => WindowFormat c ds
    -- ^ Window format of the framebuffer.
    -> (Window os c ds -> [String] -> ContextT GLFW.Handle os m a)
    -- ^ Initialization callback which receives the commandline arguments.
    -> (Window os c ds -> a -> ContextT GLFW.Handle os m a)
    -- ^ Display callback which receives the gpipe window.
    -> (Window os c ds -> a -> GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> ContextT GLFW.Handle os m a)
    -- ^ User keyboard callback which receives information about the keys pressed.
    -> (Window os c ds -> a -> Lin.V2 Int -> ContextT GLFW.Handle os m a)
    -- ^ User reshape callback which receives the window shape.
    -> ContextT GLFW.Handle os m a
main windowFormat initialize display keyboard reshape = do
    win <- newWindow windowFormat . GLFW.defaultWindowConfig =<< liftIO Env.getProgName
    -- initialize state
    state <- liftIO . MVar.newMVar =<< initialize win =<< liftIO Env.getArgs
    events <- liftIO $ STM.newTVarIO []
    -- set callbacks
    _ <- GLFW.setWindowCloseCallback win . Just $ putStrLn "window close handled"
    _ <- GLFW.setWindowSizeCallback win . Just $ \w h -> emitEv events $ \st -> reshape win st (V2 w h)
    _ <- GLFW.setKeyCallback win . Just $ \k _ ks mk -> emitEv events $ case (k, ks) of
        (GLFW.Key'Escape, GLFW.KeyState'Pressed) -> escapeKey win
        _ -> \st -> keyboard win st k ks mk
    loop win state events
  where
    escapeKey win a = liftIO (putStrLn "escape key handled") >> GLFW.setWindowShouldClose win True >> return a
    -- function to emit an event
    emitEv state ev = STM.atomically . STM.modifyTVar' state $ (ev:)
    -- function to extract events in the correct order for mapping/folding
    drainEvs state = STM.atomically . STM.stateTVar state $ \events -> (reverse events, [])
    -- mainloop
    loop win state events = do
        -- display
        liftedModifyMVar_ state $ display win
        -- tell GLFW to process WM events; normally done after buffer swapping
        _ <- GLFW.mainstep win GLFW.Poll
        -- process our events
        evs <- liftIO $ drainEvs events
        liftedModifyMVar_ state $ \st -> Mon.foldM (flip ($)) st evs
        -- decide whether to continue
        GLFW.windowShouldClose win >>= \case
            Just True -> liftIO $ putStrLn "mainloop done" >> MVar.takeMVar state
            _ -> loop win state events

-- | Lifted modifyMVar_ which can be used over any MonadIO.
liftedModifyMVar_ :: MonadIO m => MVar.MVar a -> (a -> m a) -> m ()
liftedModifyMVar_ mv action = liftIO . MVar.putMVar mv =<< action =<< liftIO (MVar.takeMVar mv)

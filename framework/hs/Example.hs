{-# LANGUAGE NegativeLiterals, TupleSections #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# OPTIONS_GHC "-Wno-missing-signatures" #-}
import Control.Monad.IO.Class
import Graphics.GPipe
import Graphics.GPipe.Context.GLFW
import qualified Graphics.GLTut.Framework as FW

main :: IO ()
main = runContextT defaultHandleConfig $ do
    _ <- FW.main (WindowFormatColor RGBA8) initialize display keyboard reshape
    return ()

type ShaderEnv os = (PrimitiveArray Triangles (B4 Float), ViewPort, Window os RGBAFloat ())

type Env os = (Buffer os (B4 Float), CompiledShader os (ShaderEnv os), ViewPort)

initialize :: Window os RGBAFloat () -> [String] -> ContextT Handle os IO (Env os)
initialize _win _args = do
    prog <- compileShader shader
    buff <- newBuffer 3
    writeBuffer buff 0
        [ V4  0.75  0.75 0 1
        , V4  0.75 -0.75 0 1
        , V4 -0.75 -0.75 0 1
        ]
    return (buff, prog, ViewPort 0 0)

display win (buff, prog, vp) = do
    render $ do
        clearWindowColor win 0
        arr <- newVertexArray buff
        prog (toPrimitiveArray TriangleList arr, vp, win)
    swapWindowBuffers win
    return (buff, prog, vp)

shader
    = drawWindowColor getDrawOpt
    . fmap (const 1) =<< rasterize getRastOpt
    . fmap (,()) =<< toPrimitiveStream getPrimArr
  where
    getPrimArr (a, _, _) = a
    getRastOpt (_, v, _) = (FrontAndBack, v, DepthRange 0 1)
    getDrawOpt (_, _, w) = (w, ContextColorOption NoBlending $ pure True)

keyboard _win st k ks mk = do
    liftIO $ print (k, ks, mk)
    return st

reshape _win (buff, prog, _) size = do
    liftIO $ print size
    return (buff, prog, ViewPort 0 size)

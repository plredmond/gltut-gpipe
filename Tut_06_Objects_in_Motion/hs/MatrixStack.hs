module MatrixStack where

import Prelude as P
import Data.Vec as Vec

data Axis = X | Y | Z deriving (Show, Eq)

axis2int :: Axis -> Int
axis2int X = 0
axis2int Y = 1
axis2int Z = 2

data Transformation a = Translate Axis a
                      | Scale     Axis a
                      | RotateRad Axis a
                      | RotateDeg Axis a
                      | TranslateAll (Vec3 a)
                      | ScaleAll     (Vec3 a)
    deriving (Show, Eq)

trans2matrix :: Floating a => Transformation a -> Mat44 a
trans2matrix (Translate d v) = translation $ setElem (axis2int d) v $ vec 0
trans2matrix (Scale     d v) = scaling     $ setElem (axis2int d) v $ vec 1
trans2matrix (RotateDeg d v) = trans2matrix (RotateRad d $ pi / 180 * v)
trans2matrix (RotateRad d v) = rotfun d    $ v
    where
        rotfun X = rotationX
        rotfun Y = rotationY
        rotfun Z = rotationZ
trans2matrix (TranslateAll v) = translation v
trans2matrix (ScaleAll     v) = scaling v

newtype MatrixStack a = MatrixStack [Mat44 a] deriving (Show, Eq)

new :: MatrixStack a
new = MatrixStack []

peek :: Num a => MatrixStack a -> Mat44 a
peek (MatrixStack []) = identity
peek (MatrixStack (m:_)) = m

pop :: MatrixStack a -> MatrixStack a
pop (MatrixStack []) = error "MatrixStack is already empty."
pop (MatrixStack (_:rest)) = MatrixStack rest

push :: Floating a => MatrixStack a -> [Transformation a] -> MatrixStack a
push (MatrixStack ms) ts =
    case ms of
        [] -> MatrixStack [c]
        h:_ -> MatrixStack $ (multmm h c):ms
    where
        cs = P.map trans2matrix ts      -- All current transformation matrices.
        c = P.foldl multmm identity cs  -- Combined current transformation matrix.

-- eof

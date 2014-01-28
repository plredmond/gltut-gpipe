module Lib.Skeleton where

import Prelude as P
import Data.Vec as V
import Data.HashMap.Lazy as H
import Data.Maybe
import Data.Fixed -- for mod'
import Control.Monad -- for join

import Lib.MatrixStack

-- A posable skeleton.
data Skeleton m a = Bone [SkelTrans a] [m]
                  | Joint [SkelTrans a] [Skeleton m a]
                  deriving (Show, Eq)

-- A skeleton frozen in one pose in preparation for rendering.
data FrozenSkel m a = FrozenBone [Transformation a] [m]
                    | FrozenJoint [Transformation a] [FrozenSkel m a]
                    deriving (Show, Eq)

-- A transformation in a posable skeleton.
data SkelTrans a = Frozen (Transformation a)
                 | Bendy String (Transformation NumOp)
                 deriving (Show, Eq)

-- Numeric operations to apply to a position from a pose.
data NumOp = Negate NumOp
           | Num
           deriving (Show, Eq)

type Pose a = HashMap String (Position a)

data Position a = Position { pCurrent :: a
                           , pIncrement :: a
                           , pExtent :: Extent a }
            deriving (Show, Eq)

-- The limiting policy for a posable transformation.
data Extent a = Mod a
              | MinMax a a
              deriving (Show, Eq)

-- Flatten a skeleton to a list of matrices paired with meshes.
flattenSkel :: Floating a => MatrixStack a -> FrozenSkel m a -> [(Mat44 a, m)]
flattenSkel mats (FrozenBone ts ms) = P.map (\m -> (mat, m)) ms
    where
        mat = peek $ push mats ts
flattenSkel mats (FrozenJoint ts sks) = join $ P.map (flattenSkel mats') sks
    where
        mats' = push mats ts

-- Freeze a skeleton's posable transformations according to the state.
freezeSkel :: Num a => Pose a -> Skeleton m a -> FrozenSkel m a
freezeSkel pose (Bone  ts ms ) = FrozenBone  (P.map (freezeSkelTrans pose) ts) ms
freezeSkel pose (Joint ts sks) = FrozenJoint (P.map (freezeSkelTrans pose) ts) (P.map (freezeSkel pose) sks)

-- Freeze a posable transformation according to the state.
freezeSkelTrans :: Num a => Pose a -> SkelTrans a -> Transformation a
freezeSkelTrans pose (Frozen t) = t
freezeSkelTrans pose (Bendy name t) = applyTransNumOp t $ pCurrent position
    where
        position = fromJust $ H.lookup name pose -- error if not found

applyTransNumOp :: Num a => Transformation NumOp -> a -> Transformation a
applyTransNumOp (Translate axis op) x = Translate axis (evalNumOp op x)
applyTransNumOp (Scale     axis op) x = Scale     axis (evalNumOp op x)
applyTransNumOp (RotateRad axis op) x = RotateRad axis (evalNumOp op x)
applyTransNumOp (RotateDeg axis op) x = RotateDeg axis (evalNumOp op x)
applyTransNumOp (TranslateAll  ops) x = TranslateAll $ V.map (\op -> evalNumOp op x) ops
applyTransNumOp (ScaleAll      ops) x = ScaleAll     $ V.map (\op -> evalNumOp op x) ops

evalNumOp :: Num a => NumOp -> a -> a
evalNumOp (Negate numop) = negate . (evalNumOp numop)
evalNumOp Num            = id

-- Adjust a posable transformation up or down.
adjustPose :: Real a => Bool -> String -> Pose a -> Pose a
adjustPose b name pose = adjust mutate name pose
    where
        updn = if b then id else negate
        mutate p = let x = pCurrent p
                       inc = updn $ pIncrement p
                       limit = enforceExtent $ pExtent p
                   in p { pCurrent = limit (x + inc) }

-- Limit a number.
enforceExtent :: Real a => Extent a -> a -> a
enforceExtent (Mod       hi) v = mod' v hi
enforceExtent (MinMax lo hi) v = max lo $ min v hi

-- eof

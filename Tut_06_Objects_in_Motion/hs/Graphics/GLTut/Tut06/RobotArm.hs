module Graphics.GLTut.Tut06.RobotArm where

import Graphics.GLTut.MatrixStack
import Graphics.GLTut.Skeleton
import Data.Vec

import qualified Data.HashMap.Lazy as HashMap

pose :: Pose Float
pose = HashMap.fromList
        [ ("baseRotY",     Position  303.75  11.25 (Mod 360)        )
        , ("upperarmRotX", Position (-56.25) 11.25 (MinMax (-90) 0) )
        , ("lowerarmRotX", Position   67.5   11.25 (MinMax 0 146.25))
        , ("wristRotZ",    Position    0     11.25 (Mod 360)        )
        , ("wristRotX",    Position   90     11.25 (MinMax 0 90)    )
        , ("fingerRotY",   Position   27      9    (MinMax 9 90)    )
        ]

base :: m -> Skeleton m Float
base m = Joint [ Frozen (TranslateAll $ (3):.(-5):.(-40):.())
               , Bendy "baseRotY" (RotateDeg Y Num) ]
               [ Bone [Frozen (Translate X 2), Frozen (Scale Z 3)] [m]
               , Bone [Frozen (Translate X $ -2), Frozen (Scale Z 3)] [m]
               , upperarm m ]

upperarm :: m -> Skeleton m Float
upperarm m = Joint [Bendy "upperarmRotX" (RotateDeg X Num)]
                   [ Bone [ Frozen (Translate Z $ size / 2 - 1)
                          , Frozen (Scale Z $ size / 2) ]
                          [m]
                   , lowerarm m ]
    where
        size = 9

lowerarm :: m -> Skeleton m Float
lowerarm m = Joint [ Frozen (Translate Z 8)
                   , Bendy "lowerarmRotX" (RotateDeg X Num) ]
                   [ Bone [ Frozen (Translate Z $ len / 2)
                          , Frozen (ScaleAll $ (width:.width:.len:.()) / 2) ]
                          [m]
                   , wrist m ]
    where
        len = 5
        width = 1.5

wrist :: m -> Skeleton m Float
wrist m = Joint [ Frozen (Translate Z 5)
                , Bendy "wristRotZ" (RotateDeg Z Num)
                , Bendy "wristRotX" (RotateDeg X Num) ]
                [ Bone [Frozen (ScaleAll $ (width:.width:.len:.()) / 2)] [m]
                , leftfinger m
                , rightfinger m ]
    where
        len = 2
        width = 2


leftfinger :: m -> Skeleton m Float
leftfinger m = Joint [ Frozen (TranslateAll $ 1:.0:.1:.())
                     , Bendy "fingerRotY" (RotateDeg Y Num) ]
                     [ Bone [ Frozen (Translate Z $ len / 2)
                            , Frozen (ScaleAll $ (width:.width:.len:.()) / 2) ]
                            [m]
                     , Bone [ Frozen (Translate Z len)
                            , Frozen (RotateDeg Y $ -45)
                            , Frozen (Translate Z $ len / 2)
                            , Frozen (ScaleAll $ (width:.width:.len:.()) / 2) ]
                            [m]
                     ]
    where
        len = 2
        width = 0.5

rightfinger :: m -> Skeleton m Float
rightfinger m = Joint[ Frozen (TranslateAll $ (-1):.0:.1:.())
                     , Bendy "fingerRotY" (RotateDeg Y $ Negate Num) ]
                     [ Bone [ Frozen (Translate Z $ len / 2)
                            , Frozen (ScaleAll $ (width:.width:.len:.()) / 2) ]
                            [m]
                     , Bone [ Frozen (Translate Z len)
                            , Frozen (RotateDeg Y 45)
                            , Frozen (Translate Z $ len / 2)
                            , Frozen (ScaleAll $ (width:.width:.len:.()) / 2) ]
                            [m]
                     ]
    where
        len = 2
        width = 0.5

-- eof

module RobotArm where

import Data.Vec
import Data.HashMap.Lazy as H
import MatrixStack
import Skeleton
import Cube

pose :: Pose Float
pose = H.fromList
        [ ("baseRotY",     Position  303.75  11.25 (Mod 360)        )
        , ("upperarmRotX", Position (-56.25) 11.25 (MinMax (-90) 0) )
        , ("lowerarmRotX", Position   67.5   11.25 (MinMax 0 146.25))
        , ("wristRotZ",    Position    0     11.25 (Mod 360)        )
        , ("wristRotX",    Position   90     11.25 (MinMax 0 90)    )
        , ("fingerRotY",   Position   27      9    (MinMax 9 90)  )
        ]

base :: Skeleton CubeStream Float
base = Joint [ Frozen (TranslateAll $ (3):.(-5):.(-40):.())
             , Bendy "baseRotY" (RotateDeg Y Num) ]
             [ Bone [Frozen (Translate X 2), Frozen (Scale Z 3)] [cube]
             , Bone [Frozen (Translate X $ -2), Frozen (Scale Z 3)] [cube]
             , upperarm ]

upperarm = Joint [Bendy "upperarmRotX" (RotateDeg X Num)]
                 [ Bone [ Frozen (Translate Z $ size / 2 - 1)
                        , Frozen (Scale Z $ size / 2) ]
                        [cube]
                 , lowerarm ]
    where
        size = 9

lowerarm = Joint [ Frozen (Translate Z 8)
                 , Bendy "lowerarmRotX" (RotateDeg X Num) ]
                 [ Bone [ Frozen (Translate Z $ len / 2)
                        , Frozen (ScaleAll $ (width:.width:.len:.()) / 2) ]
                        [cube]
                 , wrist ]
    where
        len = 5
        width = 1.5

wrist = Joint [ Frozen (Translate Z 5)
              , Bendy "wristRotZ" (RotateDeg Z Num)
              , Bendy "wristRotX" (RotateDeg X Num) ]
              [ Bone [Frozen (ScaleAll $ (width:.width:.len:.()) / 2)] [cube]
              , leftfinger
              , rightfinger ]
    where
        len = 2
        width = 2


leftfinger = Joint [ Frozen (TranslateAll $ 1:.0:.1:.())
                   , Bendy "fingerRotY" (RotateDeg Y Num) ]
                   [ Bone [ Frozen (Translate Z $ len / 2)
                          , Frozen (ScaleAll $ (width:.width:.len:.()) / 2) ]
                          [cube]
                   , Bone [ Frozen (Translate Z len)
                          , Frozen (RotateDeg Y $ -45)
                          , Frozen (Translate Z $ len / 2)
                          , Frozen (ScaleAll $ (width:.width:.len:.()) / 2) ]
                          [cube]
                   ]
    where
        len = 2
        width = 0.5

rightfinger = Joint[ Frozen (TranslateAll $ (-1):.0:.1:.())
                   , Bendy "fingerRotY" (RotateDeg Y $ Negate Num) ]
                   [ Bone [ Frozen (Translate Z $ len / 2)
                          , Frozen (ScaleAll $ (width:.width:.len:.()) / 2) ]
                          [cube]
                   , Bone [ Frozen (Translate Z len)
                          , Frozen (RotateDeg Y 45)
                          , Frozen (Translate Z $ len / 2)
                          , Frozen (ScaleAll $ (width:.width:.len:.()) / 2) ]
                          [cube]
                   ]
    where
        len = 2
        width = 0.5

-- eof

module Mesh where

import Graphics.GPipe
import Data.Vec

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Basic types

-- CPU
type Pos = Vec4 Float
type Col = Vec4 Float
type Dsp = Vec3 Float
type Mat = Mat44 Float

-- Vertex Shaders
type FloatV = Vertex Float

type PosV = VertexPosition -- Vec4 FloatV
type ColV = Vec4 FloatV
type DspV = Vec3 FloatV
type MatV = Mat44 FloatV

-- Fragment Shaders
type FloatF = Fragment Float
type BoolF = Fragment Bool

type PosF = Vec4 FloatF
type ColF = Vec4 FloatF
type DepF = FloatF

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Rendering stage types

data Winding = Clockwise | CounterClockwise | Both deriving (Show)

data MeshCfg = MeshCfg
    { mcOffset :: Dsp
    , mcBaseCol :: Col
    , mcWinding :: Winding
    } deriving (Show)

mcSetOffset (MeshCfg o b w) o' = MeshCfg o' b w

data Mesh = MeshPos    MeshCfg (PrimitiveStream Triangle PosV)
          | MeshPosCol MeshCfg (PrimitiveStream Triangle (PosV, ColV))

meConfig :: Mesh -> MeshCfg
meConfig (MeshPos    x _) = x 
meConfig (MeshPosCol x _) = x 

meSetOffset (MeshPos    mc ps) o = MeshPos    (mcSetOffset mc o) ps
meSetOffset (MeshPosCol mc ps) o = MeshPosCol (mcSetOffset mc o) ps

data Frag = FragUnit   ColF (FragmentStream (BoolF,  ()         ))
          | FragCol         (FragmentStream (BoolF,  ColF       ))
          | FragPosCol      (FragmentStream (BoolF, (PosF, ColF)))
          | FragDep    ColF (FragmentStream (BoolF,        DepF ))
          | FragColDep      (FragmentStream (BoolF, (ColF, DepF)))

data NormFrag = NFragRGBAf   (FragmentStream (Color RGBAFormat FloatF      ))
              | NFragRGBAfDf (FragmentStream (Color RGBAFormat FloatF, DepF))

--data SceneMesh = DynMesh DynMesh
--               | Mesh Mesh
--data DynMesh = DMeshPos    Winding Triangle [Pos]
--             | DMeshPosCol Winding Triangle [(Pos, Col)]

-- eof

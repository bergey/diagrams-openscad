{-# LANGUAGE FlexibleInstances
           , TypeSynonymInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , DeriveDataTypeable
  #-}

module Diagrams.Backend.OpenSCad where

import Control.Lens
import           Data.Tree

import Diagrams.Prelude hiding (fromDirection, tan)
import Diagrams.ThreeD
import           Diagrams.Core.Types
import           Diagrams.Core.Transform

import Diagrams.Backend.OpenSCad.Syntax

import Data.Typeable

import qualified Text.PrettyPrint.HughesPJ as PP

data OpenSCad = OpenSCad
              deriving (Eq,Ord,Read,Show,Typeable)

type instance V OpenSCad = V3
type instance N OpenSCad = Double

instance Monoid (Render OpenSCad V3 Double) where
    mempty = Osc []
    (Osc i1) `mappend` (Osc i2) = Osc (i1 ++ i2)

instance Backend OpenSCad V3 Double where
    data Render OpenSCad V3 Double = Osc [OTerm]
    type Result OpenSCad V3 Double = String
    data Options OpenSCad V3 Double = OscOptions

    renderRTree _ _ rt = PP.render . PP.vcat . map toOSC .unOsc . go $ rt where
      unOsc (Osc is) = is
      go :: RTree OpenSCad V3 Double a -> Render OpenSCad V3 Double
      go (Node (RPrim p) _) = render OpenSCad p
      go (Node (RStyle s) ts) = Osc . concatMap (unOsc . go) $ ts
      go (Node _ ts) = Osc . concatMap (unOsc . go) $ ts

instance Renderable (Ellipsoid Double) OpenSCad where
    render _ (Ellipsoid t) = Osc [OBlock (asMatrix t) [OPrim $ OSphere 1]]

instance Renderable (Box Double) OpenSCad where
    render _ (Box t) = Osc [OBlock (asMatrix t) [OPrim $ OCube 1 1 1]]

instance Renderable (Frustum Double) OpenSCad where
    render _ (Frustum r0 r1 t) = Osc [OBlock (asMatrix t) [OPrim $ OFrustrum 1 r0 r1]]

-- null instances so that the same Diagram can be rendered in image and geometry backends
instance Renderable (Camera l Double) OpenSCad where
    render _ _ = Osc []

instance Renderable (ParallelLight Double) OpenSCad where
        render _ _ = Osc []

instance Renderable (PointLight Double) OpenSCad where
        render _ _ = Osc []

asMatrix :: T3 Double  -> OTransform
asMatrix = OMultMatrix . matrixHomRep

-- [ [c1^._1, c2^._1, c3^._1, t^._1]
  --                         , [c1^._2, c2^._2, c3^._2, t^._2]
  --                         , [c1^._3, c2^._3, c3^._3, t^._3]
  --                         , [0,      0,      0,      1    ]
  --                         ]
  -- where
  --   ((v1,v2,v3), v4) = onBasis tr
  --   (c1, c2, c3, t) = (unr3 v1, unr3 v2, unr3 v3, unr3 v4)

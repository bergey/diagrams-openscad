{-# LANGUAGE FlexibleInstances
           , TypeSynonymInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , DeriveDataTypeable
  #-}

module Diagrams.Backend.OpenSCad where

import Control.Lens

import Diagrams.Prelude hiding (fromDirection, tan)
import Diagrams.ThreeD

import Diagrams.Backend.OpenSCad.Syntax

import Data.Typeable

import qualified Text.PrettyPrint.HughesPJ as PP

data OpenSCad = OpenSCad
              deriving (Eq,Ord,Read,Show,Typeable)

instance Monoid (Render OpenSCad R3) where
    mempty = Osc []
    (Osc i1) `mappend` (Osc i2) = Osc (i1 ++ i2)

instance Backend OpenSCad R3 where
    data Render OpenSCad R3 = Osc [OTerm]
    type Result OpenSCad R3 = String
    data Options OpenSCad R3 = OscOptions

    withStyle _ s _ (Osc is) = case getFillColor <$> getAttr s of
        Nothing ->            Osc is
        Just (SomeColor c) -> Osc [OBlock (OColor r g b a) is] where
          (r, g, b, a) = colorToSRGBA c

    doRender _ _ (Osc is) = PP.render . PP.vcat . map toOSC $ is

instance Renderable Ellipsoid OpenSCad where
    render _ (Ellipsoid t) = Osc [OBlock (asMatrix t) [OPrim $ OSphere 1]]

instance Renderable Box OpenSCad where
    render _ (Box t) = Osc [OBlock (asMatrix t) [OPrim $ OCube 1 1 1]]

instance Renderable Frustrum OpenSCad where
    render _ (Frustrum r0 r1 t) = Osc [OBlock (asMatrix t) [OPrim $ OFrustrum 1 r0 r1]]

-- null instances so that the same Diagram can be rendered in image and geometry backends
instance Renderable (Camera l) OpenSCad where
    render _ _ = Osc []

instance Renderable ParallelLight OpenSCad where
        render _ _ = Osc []

instance Renderable PointLight OpenSCad where
        render _ _ = Osc []

asMatrix :: T3 -> OTransform
asMatrix tr = OMultMatrix [ [c1^._1, c2^._1, c3^._1, t^._1]
                          , [c1^._2, c2^._2, c3^._2, t^._2]
                          , [c1^._3, c2^._3, c3^._3, t^._3]
                          , [0,      0,      0,      1    ]
                          ]
  where
    ((v1,v2,v3), v4) = onBasis tr
    (c1, c2, c3, t) = (unr3 v1, unr3 v2, unr3 v3, unr3 v4)

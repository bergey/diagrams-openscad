{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Diagrams.Backend.OpenSCad where

import           Control.Lens
import           Data.Tree

import           Diagrams.Core.Transform
import           Diagrams.Core.Types
import           Diagrams.Prelude          as D
import           Diagrams.ThreeD

import           Graphics.OpenSCAD         as O

import           Data.Typeable

#if __GLASGOW_HASKELL__ < 710
import           Data.Foldable             (foldMap)
#endif

import qualified Text.PrettyPrint.HughesPJ as PP

data OpenSCad = OpenSCad
              deriving (Eq,Ord,Read,Show,Typeable)

type instance V OpenSCad = V3
type instance N OpenSCad = Double

instance Monoid (Render OpenSCad V3 Double) where
    mempty = Osc mempty
    (Osc i1) `mappend` (Osc i2) = Osc (i1 <> i2)

instance Backend OpenSCad V3 Double where
    data Render OpenSCad V3 Double = Osc Model3d
    type Result OpenSCad V3 Double = String
    data Options OpenSCad V3 Double = OscOptions

    renderRTree _ _ rt = O.render . go $ rt where
      go :: RTree OpenSCad V3 Double a -> Model3d
      go (Node (RPrim p) _) = unOsc $ D.render OpenSCad p
      go (Node (RStyle s) ts) = setColor s $ foldMap go ts
      go (Node _ ts) = foldMap go ts

unOsc :: Render OpenSCad V3 Double -> Model3d
unOsc (Osc is) = is

model3d :: (Renderable t OpenSCad, V t ~ V3, N t ~ Double) => t -> Model3d
model3d = unOsc . D.render OpenSCad

instance Renderable (Ellipsoid Double) OpenSCad where
    render _ (Ellipsoid t) = Osc . multMatrix (asMatrix t) $ O.sphere 1 (fs 0.1)

instance Renderable (Box Double) OpenSCad where
    render _ (Box t) = Osc . multMatrix (asMatrix t) $ box 1 1 1

instance Renderable (Frustum Double) OpenSCad where
    render _ (Frustum r0 r1 t) = Osc . multMatrix (asMatrix t) $ obCylinder r0 1 r1 (fs 0.1)

instance Renderable (CSG Double) OpenSCad where
    render _ (CsgEllipsoid p) = D.render OpenSCad p
    render _ (CsgBox p) = D.render OpenSCad p
    render _ (CsgFrustum p) = D.render OpenSCad p
    render _ (CsgUnion csgs) = Osc . O.union . map model3d $ csgs
    render _ (CsgIntersection csgs) = Osc . O.intersection . map model3d $ csgs
    render _ (CsgDifference a b) = Osc $ O.difference (model3d a) (model3d b)

-- null instances so that the same Diagram can be rendered in image and geometry backends
instance Renderable (Camera l Double) OpenSCad where
    render _ _ = mempty

instance Renderable (ParallelLight Double) OpenSCad where
        render _ _ = mempty

instance Renderable (PointLight Double) OpenSCad where
        render _ _ = mempty

asMatrix :: T3 Double -> TransMatrix
asMatrix tr = ((c1^._x, c2^._x, c3^._x, t^._x),
               (c1^._y, c2^._y, c3^._y, t^._y),
               (c1^._z, c2^._z, c3^._z, t^._z),
               (0,0,0,1)
              ) where
  ([c1, c2, c3], t) = onBasis tr

setColor :: Style V3 Double -> Model3d -> Model3d
setColor sty m = case sty ^. _sc of
    Nothing -> m
    Just c -> O.color c m

module Diagrams.Backend.OpenSCad.Syntax where

import Text.PrettyPrint.HughesPJ

import Data.List

class OSC p where
    toOSC :: p -> Doc

data OTerm = OPrim OSolid
           | OBlock OTransform [OTerm]

data OSolid = OCube Double Double Double -- ^ Length along X, Y, Z axes
            | OSphere Double -- ^ Radius
              -- consider adding resolution variables
            | OFrustrum Double Double Double -- ^ Z length, Radius at Z=0, final
--            | OPolyhedron -- not yet supported

data OTransform = OScale Double Double Double -- ^ Scale factors along X, Y, Z axes
                | ORotate Double Double Double -- ^ about x, y, z axes
                | OTranslate Double Double Double -- ^ x, y, z axes
                | OMirror -- convertable
                | OMultMatrix [[Double]]
                | OColor Double Double Double Double -- ^ RGBA, all between [0,1]
                | OMinkowski
                | OHull
                | OUnion
                | ODifference
                | OIntersection


list :: [Doc] -> Doc
list = hcat . intersperse (text ", ")

vector3 :: Double -> Double -> Double -> Doc
vector3 x y z = brackets . list . map double $ [x,y,z]

-- vector4 :: V4 Double -> Doc
-- vector4 (V4 x y z w) = brackets . list . map double $ [x, y, z, w]

statement :: String -> Doc -> Doc
statement name args = text name <> parens args

instance OSC OTerm where
    toOSC (OPrim s) = toOSC s
    toOSC (OBlock t xs) = toOSC t <+> lbrace $$
                          nest 4 (vcat . map toOSC $ xs) $$ rbrace

instance OSC OSolid where
    toOSC (OCube x y z) = statement "cube" (vector3 x y z) <> semi
    toOSC (OSphere r) = statement "sphere" (double r) <> semi
    toOSC (OFrustrum h r1 r2) = statement "cylinder" (list $ map double [h, r1, r2]) <> semi

instance OSC OTransform where
    toOSC (OScale x y z) = statement "scale" $ vector3 x y z
    toOSC (ORotate x y z) = statement "rotate" $ vector3 x y z
    toOSC (OTranslate x y z) = statement "translate" $ vector3 x y z
    toOSC OMirror = undefined
    toOSC (OMultMatrix m) = statement "multmatrix" $ 
                            brackets . vcat . fmap ((<+> text ",") . brackets . list . fmap double) $ m
    toOSC OMinkowski = undefined
    toOSC (OColor r g b a) = statement "color" . brackets . list . map double $ [r, g, b, a]
    toOSC OUnion = statement "union" empty
    toOSC ODifference = statement "difference" empty
    toOSC OIntersection = statement "intersection" empty

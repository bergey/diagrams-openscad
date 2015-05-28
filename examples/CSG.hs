-- | A test of (some of) the CSG functionality

module CSG where

import           Diagrams.Prelude

import           Diagrams.Backend.OpenSCad
import           Diagrams.Backend.OpenSCad.CmdLine

main = defaultMain ex

ex :: Diagram OpenSCad
ex = scale 20 . skin $ difference cube sphere

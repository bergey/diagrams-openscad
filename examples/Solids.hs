-- | A very simple test of diagrams-openscad

module Main where

import Diagrams.Prelude

import Diagrams.Backend.OpenSCad
import Diagrams.Backend.OpenSCad.CmdLine

main = defaultMain ex

ex :: Diagram OpenSCad
ex = scale 20 $ cube <> sphere # sc blue

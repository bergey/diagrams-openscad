-- |

module Diagrams.Backend.OpenSCad.CmdLine where

import           Diagrams.Backend.OpenSCad
import           Diagrams.Prelude

defaultMain :: QDiagram OpenSCad V3 Double Any -> IO ()
defaultMain = putStrLn . renderDia OpenSCad OscOptions

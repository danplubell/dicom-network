module Main where

import Dicom.Network.TestHarness.TestAssociationSCU

main::IO()
main = client "127.0.0.1" 3000
          

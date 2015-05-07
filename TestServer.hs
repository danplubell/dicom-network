module Main where

import Dicom.StorageSOP.SCPDispatcher

main::IO()
main = startDispatcher "127.0.0.1" 3000
          

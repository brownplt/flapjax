-- |Uses the Flapjax contract specification to determine which functions and
-- methods are not lifted.
module BrownPLT.Flapjax.Interface
  ( getFlapjaxNames
  , getFlapjaxInterfacePath
  , getInstalledFlapjaxPath
  ) where

import System.FilePath
import BrownPLT.JavaScript.Contracts
import BrownPLT.JavaScript.Contracts.Interface

import Paths_Flapjax

getFlapjaxInterfacePath :: IO FilePath
getFlapjaxInterfacePath = do
  dataDir <- getDataDir
  return $ dataDir </> "flapjax.jsi"

getInstalledFlapjaxPath :: IO FilePath
getInstalledFlapjaxPath = do
  dataDir <- getDataDir
  return $ dataDir </> "flapjax.js"

interfaceNames :: [InterfaceItem] -> [String]
interfaceNames names  = map unExport (filter isInterfaceExport names) where
  unExport (InterfaceExport n _ _) = n
  unExport x = error $ "interfaceNames: got " ++ show x

interfaceMethods :: [InterfaceItem] -> [String]
interfaceMethods names = concatMap unInstance names where
  unInstance (InterfaceInstance _ _ (ObjectContract _ methods)) = 
    map fst methods
  unInstance (InterfaceInstance _ _ contract) =
    error $ "interfaceMethods: contract on instance is " ++ show contract
  unInstance _ = []

-- |Returns a list of Flapjax function names and a list of Flapjax method
-- names.
getFlapjaxNames :: IO ([String],[String])
getFlapjaxNames = do
  interfacePath <- getFlapjaxInterfacePath
  interface <- parseInterface interfacePath
  return (interfaceNames interface, interfaceMethods interface)
  



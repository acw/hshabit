module Compiler.FrontEnd.ModuleImport(
         ImportedModule(..)
       , openModule
       )
 where

import Compiler.Monad

data ImportedModule = ImportedModule {
    imodTypeNames :: [String]
  , imodValNames  :: [String]
  }

openModule :: String -> FilePath -> CompilerM IO (Maybe ImportedModule)
openModule = undefined

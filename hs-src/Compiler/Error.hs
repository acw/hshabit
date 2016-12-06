module Compiler.Error(Error(..))
 where

import Compiler.Warning
import Syntax.Posn

data Error =
    AsNameHasPrefix        Posn String
  | ConstructorHasPrefix   Posn String
  | DiffArgCounts          Posn String
  | EmptyWhere             Posn
  | ErrorWarning           Warning
  | HiddenHasPrefix        Posn String
  | ImportNotFound         Posn String String
  | IncludeHasPrefix       Posn String
  | InvalidConstructorType Posn
  | NoEqnForTypeSig        Posn String
  | NoSuchImport           Posn String String
  | TooManyTypeSigs        Posn String
  | TopLevelHasPrefix      Posn String

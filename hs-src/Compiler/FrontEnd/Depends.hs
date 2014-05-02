module Compiler.FrontEnd.Depends(
         getDepends
       , buildMakeDepends
       )
 where

import Data.ByteString.Lazy(ByteString)
import Data.ByteString.Lazy.Char8(pack)
import Data.List(intercalate)
import Data.Maybe(catMaybes)
import Misc.Output
import Syntax.ParseAST
import Syntax.Parser(parse)
import System.FilePath(joinPath, (</>), (<.>))

data MakeDepend = MD FilePath FilePath

instance Output [MakeDepend] where
  toByteString ls = pack (intercalate "\n" (map show ls))

instance Show MakeDepend where
  show (MD a b) = a ++ ": " ++ b

getDepends :: HabitModule -> [FilePath]
getDepends (HabitModule _ decls) = catMaybes (map gdDecl decls)

buildMakeDepends :: Maybe FilePath -> ByteString -> [MakeDepend]
buildMakeDepends Nothing bstr = buildMakeDepends (Just "Main.hbt") bstr
buildMakeDepends f@(Just srcfile) bstr = dependClauses
 where
  hbtmod = parse f bstr
  dependClauses = map (MD srcfile) depends
  depends = getDepends hbtmod

gdDecl :: Decl -> Maybe FilePath
gdDecl (ImportDecl _ _ n _ _) = Just (convertName n)
gdDecl _                      = Nothing

convertName :: Name -> FilePath
convertName (Name _ prefixes name) = joinPath prefixes </> name <.> "o"

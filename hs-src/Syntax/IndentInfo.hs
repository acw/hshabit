module Syntax.IndentInfo(IndentedLexeme(..), addIndentInfo)
  where

import Debug.Trace
import Syntax.Lexeme
import Syntax.Posn

data IndentedLexeme = Normal Lexeme
                    | IndentMarkA Int -- <n>
                    | IndentMarkB Int -- {n}

instance Show IndentedLexeme where
  show (Normal x)      = show x
  show (IndentMarkA x) = "<" ++ show x ++ ">"
  show (IndentMarkB x) = "{" ++ show x ++ "}"

isOpenBrace :: Lexeme -> Bool
isOpenBrace (ReservedSym _ "{") = True
isOpenBrace _                   = False

isBraceOrModule :: Lexeme -> Bool
isBraceOrModule (ReservedId  _ "module") = True
isBraceOrModule (ReservedSym _ "{")    = True
isBraceOrModule _                        = False

tokenWantsOpenBrace :: Lexeme -> Bool
tokenWantsOpenBrace (ReservedId _ x) = x `elem` ["let", "where", "do", "of"]
tokenWantsOpenBrace _                = False

addIndentInfo :: [Lexeme] -> [IndentedLexeme]
addIndentInfo xs = go xs True False False 1
 where
  col  = columnNum . getPosn
  line = lineNum . getPosn
  isNewLine t l = line t > l
  --
  go [] _ _ _ _ = []
  go cur@(f:rest) isFirst wantOpen justInjected lastLine
    | wantOpen && not (isOpenBrace f) =
        IndentMarkB (col f) : go cur False False True (line f)
    | isFirst && not (isBraceOrModule f) =
        IndentMarkB (col f) : go cur False False True (line f)
    | (not justInjected) && isNewLine f lastLine =
        IndentMarkA (col f) : go cur False False True (line f)
    | tokenWantsOpenBrace f =
        Normal f : go rest False True False lastLine
    | otherwise =
        Normal f : go rest False False False (line f)


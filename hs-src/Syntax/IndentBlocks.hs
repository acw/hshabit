module Syntax.IndentBlocks(lex)
 where

import Data.ByteString.Lazy(ByteString)
import Prelude hiding (lex)
import Syntax.IndentInfo
import Syntax.Lexeme
import Syntax.Posn
import Syntax.Scanner

lex :: Maybe FilePath -> ByteString -> [Lexeme]
lex source bytes = indentBlocks (addIndentInfo (scan source bytes))

indentBlocks :: [IndentedLexeme] -> [Lexeme]
indentBlocks ils = ell ils []
 where
  ell :: [IndentedLexeme] -> [Int] -> [Lexeme]
  ell (IndentMarkA n : ts) (m : ms)
    | (m == n) && forbidSemi ts = ell ts (m : ms)
    | (m == n)                  = injectedSemi : ell ts (m : ms)
    | n < m                     = injectedClose : ell (IndentMarkA n : ts) ms
  ell (IndentMarkA _ : ts) ms   = ell ts ms
  --
  ell (IndentMarkB n : ts) (m : ms)
    | n > m = injectedOpen : ell ts (n : (m : ms))
  ell (IndentMarkB n : ts) []
    | n > 0 = injectedOpen : ell ts [n]
  ell (IndentMarkB n : ts) ms
    = injectedOpen : (injectedClose : (ell (IndentMarkA n : ts) ms))
  --
  ell (Normal x@(ReservedSym _ "}") : ts) (0 : ms)
    = x : ell ts ms
  ell (Normal (ReservedSym _ "}") : _) _
    = error "INTERNAL ERROR: Close in weird L state"
  --
  ell (Normal x@(ReservedSym _ "{") : ts) ms
    = x : ell ts (0 : ms)
  --
  ell ts@[Normal (EOF _)] (_ : ms) = injectedClose : ell ts ms
  ell [Normal (EOF _)] [] = []
  --
  ell (Normal t : ts) ms
    = t : ell ts ms
  --
  ell [] _ = error "INTERNAL ERROR: Reached end of list!"

injectedOpen, injectedClose, injectedSemi :: Lexeme
injectedOpen   = ReservedSym EmptyPosn "{"
injectedClose  = ReservedSym EmptyPosn "}"
injectedSemi   = ReservedSym EmptyPosn ";"

forbidSemi :: [IndentedLexeme] -> Bool
forbidSemi (Normal (ReservedId _ x) : _) = x `elem` ["then","else","of","in"]
forbidSemi _                             = False

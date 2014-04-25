module Syntax.Layout ( scanWithLayout )
 where

import Data.ByteString.Lazy(ByteString)
import Syntax.Lexeme
import Syntax.Posn
import Syntax.Scanner

data IndentedLexeme = Normal Lexeme
                    | IndentA Int -- <n>
                    | IndentB Int -- {n}

instance Show IndentedLexeme where
  show (Normal x)      = show x
  show (IndentA x) = "<" ++ show x ++ ">"
  show (IndentB x) = "{" ++ show x ++ "}"

-- |Scan the given source file and data, inserting layout tokens as
-- necessary.
scanWithLayout :: Maybe FilePath -> ByteString -> [Lexeme]
scanWithLayout source bstr = ell tokens [] False
 where tokens = addIndentInfo (scan source bstr)

-- Start by adding indent information to the Lexeme stream. (See
-- the Haskell 2010 Report, page 132.
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
        IndentB (col f) : go cur False False True (line f)
    | isFirst && not (isBraceOrModule f) =
        IndentB (col f) : go cur False False True (line f)
    | (not justInjected) && isNewLine f lastLine =
        IndentA (col f) : go cur False False True (line f)
    | tokenWantsOpenBrace f =
        Normal f : go rest False True False lastLine
    | isEOF f = []
    | otherwise =
        Normal f : go rest False False False (line f)
  --
  isOpenBrace :: Lexeme -> Bool
  isOpenBrace (ReservedSym _ "{") = True
  isOpenBrace _                   = False
  --
  isBraceOrModule :: Lexeme -> Bool
  isBraceOrModule (ReservedId  _ "module") = True
  isBraceOrModule (ReservedSym _ "{")    = True
  isBraceOrModule _                        = False
  --
  tokenWantsOpenBrace :: Lexeme -> Bool
  tokenWantsOpenBrace (ReservedId _ x) = x `elem` ["let", "where", "do", "of"]
  tokenWantsOpenBrace _                = False
  --
  isEOF :: Lexeme -> Bool
  isEOF (EOF _) = True
  isEOF _       = False


-- See the Haskell 2010 Report, page 133
ell :: [IndentedLexeme] -> [Int] -> Bool -> [Lexeme]
ell (IndentA n : ts) (m : ms) _
  | m == n && noInject ts    = ell ts (m : ms) False
  | m == n                   = sym ";" : ell ts (m : ms) False
  | n < m                    = sym "}" : ell (IndentA n : ts) ms True
ell (IndentA _ : ts) ms jc   = ell ts ms jc
ell (IndentB n : ts) (m : ms) _
  | n > m                    = sym "{" : ell ts (n : (m : ms)) False
ell (IndentB n : ts) [] _
  | n > 0                    = sym "{" : ell ts [n] False
ell (IndentB n : ts) ms _
                             = sym "{" : sym "}" : ell (IndentA n : ts) ms False
ell (Normal t@(ReservedSym _ "}") : ts) (0 : ms) _
                             = t : (ell ts ms True)
ell (Normal   (ReservedSym _ "}") : _ ) _ _
                             = error "Layout rule failed on close brace."
ell (Normal t@(ReservedSym _ "{") : ts) ms _
                             = t : (ell ts (0 : ms) False)
ell (Normal t : ts) (m : ms) justClosed
  | (m /= 0) &&
    not justClosed &&
    needsClose t             = sym "}" : ell (Normal t : ts) ms True
ell (Normal t : ts) ms _
                             = t : ell ts ms False
ell [] []  _                 = []
ell [] (_ : ms) _            = sym "}" : ell [] ms True

noInject :: [IndentedLexeme] -> Bool
noInject (Normal (ReservedId _ "then") : _) = True
noInject (Normal (ReservedId _ "else") : _) = True
noInject (Normal (ReservedId _ "of")   : _) = True
noInject (Normal (ReservedId _ "in")   : _) = True
noInject _                                  = False

needsClose :: Lexeme -> Bool
needsClose (ReservedId _ "in") = True
needsClose _                   = False

sym :: String -> Lexeme
sym = ReservedSym EmptyPosn

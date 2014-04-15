{
{-# OPTIONS_GHC -w #-}
--  vim:set filetype=haskell:
module Syntax.Scanner(scan)
 where

import Data.ByteString.Lazy(ByteString,unpack)
import qualified Data.ByteString.Lazy as BS
import Data.Char(chr,ord,isDigit,digitToInt)
import Data.Int(Int64)
import Debug.Trace
import Syntax.Lexeme
import Syntax.Posn

}

%wrapper "posn-bytestring"

$bindigit     = [01]
$octdigit     = [01234567]
$decdigit     = [0123456789]
$hexdigit     = [0123456789aAbBcCdDeEfF]

$small        = [abcdefghijklmnopqrstuvwxyz]
$large        = [ABCDEFGHIJKLMNOPQRSTUVWXYZ]
$digit        = [0123456789]
$idchar       = [$small $large $digit \']
$sym          = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$nocsym       = $sym # [:]

$whitechar    = [ \t\n\r\f\v]

@varid        = $small $idchar*
@conid        = $large $idchar*

@qual         = (@conid \.)+

habit :-

<0> $white+                                                     { skip }
<0> "--"\-*[^$sym].*                                            { skip }

"{-"                                                            { modifyCommentState succ }
"-}"                                                            { modifyCommentState pred }

<0> "area"     | "as"       | "bitdata"  | "case"     | "class"    |
    "data"     | "deriving" | "do"       | "else"     | "fails"    |
    "hiding"   | "if"       | "import"   | "in"       | "infix"    |
    "infixl"   | "infixr"   | "instance" | "lab"      | "let"      |
    "module"   | "nat"      | "of"       | "qualified"| "struct"   |
    "then"     | "type"     | "where"                           { mkReservedId }

<0> "("        | ")"        | "|"        | "="        | ","        |
    "`"        | "{"        | ";"        | "}"        | "["        |
    "]"        | \\         | "<-"       | "->"       | "=>"       |
    "::"       | "#."       | "@"        | "_"        | "."        |
    "*"        | "/"        | ":#"                              { mkReservedSym }

<0> "0"[bB][$bindigit \_]+[KMGT]?                               { mkBinConst }
<0> "0"[oO][$octdigit \_]+[KMGT]?                               { mkOctConst }
<0> "0"[dD][$decdigit \_]+[KMGT]?                               { mkDecConst }
<0> "0"[xX][$hexdigit \_]+[KMGT]?                               { mkHexConst }
<0> [$decdigit \_]+[KMGT]?                                      { mkDecConst }

<0> "B"[$bindigit \_]+                                          { mkBinVecConst }
<0> "O"[$octdigit \_]+                                          { mkOctVecConst }
<0> "X"[$hexdigit \_]+                                          { mkHexVecConst }

<0> $decdigit+ \. $decdigit+ ([eE] [\-\+]? $decdigit+)? [fFdD]? { mkFloatConst }
<0> $decdigit+               ([eE] [\-\+]? $decdigit+)? [fFdD]? { mkFloatConst }

<0> @varid                                                      { mkVarId }
<0> @conid                                                      { mkConId }
<0> @qual @varid                                                { mkQVarId }
<0> @qual @conid                                                { mkQConId }
<0> $nocsym $sym*                                               { mkVarSymId }
<0> ":" $sym*                                                   { mkConSymId }
<0> @qual $nocsym $sym*                                         { mkQVarSymId }
<0> @qual ":" $sym*                                             { mkQConSymId }

{

type PosnBuilder = Int -> Int -> Posn

data LexerState = LS {
    lsPosnBuilder  :: PosnBuilder
  , lsCurStartCode :: Int
  , lsCurState     :: AlexInput
  }

newtype Lexer a = Lexer { runLexer :: LexerState -> (a, LexerState) }

instance Monad Lexer where
  return a = Lexer $ \ s -> (a, s)
  a >>= k  = Lexer $ \ s ->
               let (v, s') = runLexer a s
               in runLexer (k v) s'

instance Functor Lexer where
  fmap f a = Lexer $ \ s ->
               let (v, s') = runLexer a s
               in (f v, s')

getPosnBuilder :: Lexer PosnBuilder
getPosnBuilder = Lexer $ \ s -> (lsPosnBuilder s, s)

getStartCode :: Lexer Int
getStartCode = Lexer $ \ s -> (lsCurStartCode s, s)

setStartCode :: Int -> Lexer ()
setStartCode st = Lexer $ \ s -> ((), s{ lsCurStartCode = st })

getInput :: Lexer AlexInput
getInput = Lexer $ \ s -> (lsCurState s, s)

setInput :: AlexInput -> Lexer ()
setInput i = Lexer $ \ s -> ((), s{ lsCurState = i })

lexError :: String -> Lexer a
lexError errstr =
  do (AlexPn _ l c, _, input) <- getInput
     builder                  <- getPosnBuilder
     let myp                   = builder l c
     fail (show myp ++ ": " ++ errstr)

scan :: Maybe FilePath -> ByteString -> [Lexeme]
scan source bytes = fst (runLexer go state0)
 where
  state0  = LS builder 0 (AlexPn 0 1 1, undefined, bytes)
  builder :: Int -> Int -> Posn
  builder = maybe ConsolePosn FilePosn source
  --
  go =
    do inp@(p,_,bstr) <- getInput
       sc <- getStartCode
       case alexScan inp sc of
         AlexToken inp' len action ->
           do setInput inp'
              mb <- action p (BS.take (fromIntegral len) bstr)
              rest <- go
              case mb of
                Just lex -> return (lex:rest)
                Nothing  -> return rest

         AlexSkip inp' len ->
           do setInput inp'
              go

         AlexEOF ->
           do let AlexPn _ l c = p
              return [EOF (builder l c)]

         AlexError inp' ->
           fail "Internal error in parse!"

-- ----------------------------------------------------------------------------

type AlexAction = AlexPosn -> ByteString -> Lexer (Maybe Lexeme)

skip :: AlexAction
skip _ _ = return Nothing

modifyCommentState :: (Int -> Int) -> AlexAction
modifyCommentState f _ _ =
  do sc <- getStartCode
     setStartCode (f sc)
     return Nothing

mkReservedId, mkReservedSym :: AlexAction
mkReservedId  = yankAndFill ReservedId  (map (chr . fromIntegral) . unpack)
mkReservedSym = yankAndFill ReservedSym (map (chr . fromIntegral) . unpack)

mkBinConst, mkOctConst, mkDecConst, mkHexConst :: AlexAction
mkBinConst = yankAndFill (\ a b -> IntConst a b 2)  (parseInt 2)
mkOctConst = yankAndFill (\ a b -> IntConst a b 8)  (parseInt 8)
mkDecConst = yankAndFill (\ a b -> IntConst a b 10) (parseInt 10)
mkHexConst = yankAndFill (\ a b -> IntConst a b 16) (parseInt 16)

mkBinVecConst, mkOctVecConst, mkDecVecConst, mkHexVecConst :: AlexAction
mkBinVecConst = yankAndFill (\ a (v,l) -> VecConst a v 2  l) (parseVec 2)
mkOctVecConst = yankAndFill (\ a (v,l) -> VecConst a v 8  l) (parseVec 8)
mkDecVecConst = yankAndFill (\ a (v,l) -> VecConst a v 10 l) (parseVec 10)
mkHexVecConst = yankAndFill (\ a (v,l) -> VecConst a v 16 l) (parseVec 16)

mkVarId, mkConId, mkQVarId, mkQConId :: AlexAction
mkVarId    = yankAndFill VarId    id
mkConId    = yankAndFill ConId    id
mkQVarId   = yankAndFill QVarId   id
mkQConId   = yankAndFill QConId   id

mkVarSymId, mkConSymId, mkQVarSymId, mkQConSymId :: AlexAction
mkVarSymId = yankAndFill  VarSymId  id
mkConSymId = yankAndFill  ConSymId  id
mkQVarSymId = yankAndFill QVarSymId id
mkQConSymId = yankAndFill QConSymId id

mkFloatConst :: AlexAction
mkFloatConst (AlexPn _ l c) bstr = do
  builder <- getPosnBuilder
  let posn = builder l c
  case ByteString.unsnoc bstr of
    Nothing -> lexError "Empty float constant is weird."
    Just (start, lastb)
      | chr (fromIntegral lastb) == 'f' -> makeFloat  posn start
      | chr (fromIntegral lastb) == 'F' -> makeFloat  posn start
      | chr (fromIntegral lastb) == 'd' -> makeDouble posn start
      | chr (fromIntegral lastb) == 'D' -> makeDouble posn start
      | otherwise                       -> makeFloat  posn bstr
 where
  makeFloat posn str =
    let cstr = map (chr . fromIntegral) (unpack str)
    in case reads cstr of
         [(x, "")] -> return (Just (FloatConst posn (FVal x)))
         _         -> lexError ("Error parsing " ++ cstr ++ " as Float")

  makeDouble posn str =
    let cstr = map (chr . fromIntegral) (unpack str)
    in case reads cstr of
         [(x, "")] -> return (Just (FloatConst posn (DVal x)))
         _         -> lexError ("Error parsing " ++ cstr ++ " as Double")

-- ----------------------------------------------------------------------------

yankAndFill :: (Posn -> a -> Lexeme) ->
               (ByteString -> a) ->
               AlexPosn -> ByteString ->
               Lexer (Maybe Lexeme)
yankAndFill builder converter (AlexPn _ l c) bstr =
  do posnBuilder <- getPosnBuilder
     return (Just (builder (posnBuilder l c) (converter bstr)))

parseInt :: Int -> ByteString -> Integer
parseInt base bstr
  | BS.length bstr < 2           = buildIntConst bstr
  | secondChar `elem` "bBoOdDxX" = buildIntConst (BS.drop 2 bstr)
  | otherwise                    = buildIntConst bstr
 where
  secondChar = chr (fromIntegral (BS.index bstr 1))
  baseI = fromIntegral base
  --
  buildIntConst bstr =
    case BS.unsnoc bstr of
      Nothing -> error ("Internal error: empty integer constant: " ++ show bstr)
      Just (start, u)
        | chr (fromIntegral u) == 'K' -> (2^10) * bs2int (unpack start) baseI 0
        | chr (fromIntegral u) == 'M' -> (2^20) * bs2int (unpack start) baseI 0
        | chr (fromIntegral u) == 'G' -> (2^30) * bs2int (unpack start) baseI 0
        | chr (fromIntegral u) == 'T' -> (2^40) * bs2int (unpack start) baseI 0
        | otherwise                   -> 1      * bs2int (unpack bstr ) baseI 0

parseVec :: Int -> ByteString -> (Integer, Int)
parseVec base bstr = (bs2int (BS.unpack cleanbstr) baseI 0, bpc * cleanlen)
 where
  cleanbstr = BS.drop 1 (BS.filter (/= (fromIntegral (ord '_'))) bstr)
  cleanlen  = fromIntegral (BS.length cleanbstr)
  baseI     = fromIntegral base
  bpc | base == 2  = 1
      | base == 8  = 3
      | base == 16 = 4
      | otherwise  = error "Internal error: unacceptable vector base."

bs2int :: [Word8] -> Integer -> Integer -> Integer
bs2int []       _    acc = acc
bs2int (95:rst) base acc = bs2int rst base acc -- 95 == '_'
bs2int (f :rst) base acc = bs2int rst base ((acc * base) + digitToInt' f)
 where  digitToInt' = fromIntegral . digitToInt . chr . fromIntegral

}

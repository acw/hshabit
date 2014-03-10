{
module Syntax.Tokens( Lexeme(..)
                    , FloatVal(..)
                    , Alex
                    , alexEOF
                    , tokenize
                    , runParser
                    )
 where

import Data.ByteString.Lazy(ByteString,unpack)
import qualified Data.ByteString.Lazy as ByteString
import Data.Char(chr,ord,isDigit,digitToInt)
import Data.Int(Int64)

import Debug.Trace

}

%wrapper "monadUserState-bytestring"

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

habit :-

<0> $white+                                                    { skip }
<0> "--"\-*[^$sym].*                                           { skip }

"{-"                                                           { run_comment }

<0> "area"     | "bitdata"  | "case"     | "class"    | "data"     |
    "deriving" | "do"       | "else"     | "fails"    | "if"       |
    "in"       | "infix"    | "infixl"   | "infixr"   | "instance" |
    "lab"      | "let"      | "of"       | "struct"   | "then"     |
    "type"     | "where"                                       { mkReservedId }

<0> "("        | ")"        | "|"        | "="        | ","        |
    "`"        | "{"        | ";"        | "}"        | "["        |
    "]"        | "\\"       | "<-"       | "->"       | "=>"       |
    "::"       | "#."       | "@"        | "_"        | "."        |
    "*"        | "/"                                           { mkReservedSym }

<0> "0"[bB][$bindigit \_]*[KMGT]?                              { mkBinConst }
<0> "0"[oO][$octdigit \_]*[KMGT]?                              { mkOctConst }
<0> "0"[dD][$decdigit \_]*[KMGT]?                              { mkDecConst }
<0> "0"[xX][$hexdigit \_]*[KMGT]?                              { mkHexConst }
<0> [$decdigit \_]+[KMGT]?                                     { mkDecConst }

<0> "B"[$bindigit \_]+                                         { mkBinVecConst }
<0> "O"[$octdigit \_]+                                         { mkOctVecConst }
<0> "X"[$hexdigit \_]+                                         { mkHexVecConst }

<0> $decdigit+ \. $decdigit+ ([eE] [\-\+] $decdigit+)? [fFgG]? { mkFloatConst }
<0> $decdigit+               ([eE] [\-\+] $decdigit+)? [fFgG]? { mkFloatConst }

<0> $small $idchar*                                            { mkVarId }
<0> $large $idchar*                                            { mkConId }
<0> ":" $sym+                                                  { mkVarSymId }
<0> $nocsym $sym*                                              { mkConSymId }

{

-- ----------------------------------------------------------------------------
--
-- Lexemes, Functions over Lexemes, and Accessors
--
-- ----------------------------------------------------------------------------

data Lexeme = ReservedId  AlexPosn String
            | ReservedSym AlexPosn String     Bool
            | VarId       AlexPosn ByteString
            | ConId       AlexPosn ByteString
            | VarSymId    AlexPosn ByteString
            | ConSymId    AlexPosn ByteString
            | IntConst    AlexPosn Integer    Int
            | VecConst    AlexPosn Integer    Int Int
            | FloatConst  AlexPosn FloatVal
            | IndentMarkB Int -- {n}
            | IndentMarkA Int -- <n>
            | EOF

data FloatVal = FVal Float | DVal Double

instance Show Lexeme where
  show (ReservedId  p st)     = "lexeme:" ++ show p ++ ":" ++ "RId:" ++ st
  show (ReservedSym p st _)   = "lexeme:" ++ show p ++ ":" ++ "RSym:" ++ st
  show (VarId       p bs)     = "lexeme:" ++ show p ++ ":" ++ "VarId:|" ++
                                map (chr . fromIntegral) (unpack bs) ++ "|"
  show (ConId       p bs)     = "lexeme:" ++ show p ++ ":" ++ "ConId:" ++
                                map (chr . fromIntegral) (unpack bs)
  show (VarSymId    p bs)     = "lexeme:" ++ show p ++ ":" ++ "VarSymId:" ++
                                map (chr . fromIntegral) (unpack bs)
  show (ConSymId    p bs)     = "lexeme:" ++ show p ++ ":" ++ "ConSymId:" ++
                                map (chr . fromIntegral) (unpack bs)
  show (IntConst    p vl b)   = "lexeme:" ++ show p ++ ":Int:base" ++ show b ++
                                ":" ++ show vl
  show (VecConst    p vl b l) = "lexeme:" ++ show p ++ ":Vec:base" ++ show b ++
                                "len" ++ show l ++ ":" ++ show vl
  show (FloatConst  p vl)     = "lexeme:" ++ show p ++ ":Float:" ++ show vl
  show (IndentMarkB x)        = "lexeme:{" ++ show x ++ "}"
  show (IndentMarkA x)        = "lexeme:<" ++ show x ++ ">"
  show EOF                    = "lexeme:EOF"

instance Show FloatVal where
  show (FVal f) = show f ++ "f"
  show (DVal d) = show d ++ "d"

getPosn :: Lexeme -> AlexPosn
getPosn (ReservedId  p _)     = p
getPosn (ReservedSym p _ _)   = p
getPosn (VarId       p _)     = p
getPosn (ConId       p _)     = p
getPosn (VarSymId    p _)     = p
getPosn (ConSymId    p _)     = p
getPosn (IntConst    p _ _)   = p
getPosn (VecConst    p _ _ _) = p
getPosn (FloatConst  p _)     = p
getPosn (EOF)                 = AlexPn maxBound maxBound maxBound
getPosn _                     = error "Tried to get line of indent mark"

alexEOF :: Alex Lexeme
alexEOF = return EOF

isOpenBrace :: Lexeme -> Bool
isOpenBrace (ReservedSym _ "{" _) = True
isOpenBrace _                     = False

isBraceOrModule :: Lexeme -> Bool
isBraceOrModule (ReservedId  _ "module") = True
isBraceOrModule (ReservedSym _ "{" _)    = True
isBraceOrModule _                        = False

lineNum :: Lexeme -> Int
lineNum tok = lineno
  where (AlexPn _ lineno _) = getPosn tok

columnNum :: Lexeme -> Int
columnNum tok = colno
  where (AlexPn _ _ colno) = getPosn tok

newIndentToken :: (Int -> Lexeme) -> Lexeme -> Lexeme
newIndentToken builder tok = builder (columnNum tok)

isNewLine :: AlexUserState -> Lexeme -> Bool
isNewLine st tok = lineNum tok > curLine st

tokenWantsOpenBrace :: Lexeme -> Bool
tokenWantsOpenBrace (ReservedId _ x) = x `elem` ["let", "where", "do", "of"]
tokenWantsOpenBrace _                = False

injectedCloseBrace :: Lexeme
injectedCloseBrace = ReservedSym nopos "}" True

nonInjectedClose :: Lexeme -> Bool
nonInjectedClose (ReservedSym _ "}" False) = True
nonInjectedClose _                         = False

injectedOpenBrace :: Lexeme
injectedOpenBrace = ReservedSym nopos "{" True

nonInjectedOpen :: Lexeme -> Bool
nonInjectedOpen (ReservedSym _ "{" False) = True
nonInjectedOpen _                         = False

injectedSemi :: Lexeme
injectedSemi  = ReservedSym nopos ";" True

nopos :: AlexPosn
nopos = AlexPn minBound minBound minBound

-- ----------------------------------------------------------------------------
--
-- Nested Comment Handline
--
-- ----------------------------------------------------------------------------

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar input =
  case alexGetByte input of
    Nothing          -> Nothing
    Just (x, input') -> Just (chr (fromIntegral x), input')

run_comment :: AlexInput -> Int64 -> Alex Lexeme
run_comment _ _ = go 1 =<< alexGetInput
 where
  go 0 input = alexSetInput input >> alexMonadScan
  go n input = do
   case alexGetChar input of
     Nothing            ->
       err input
     Just ('{', input') ->
       case alexGetChar input of
         Nothing            -> err input
         Just ('-',input'') -> go (n + 1) input''
         Just (_, input'')  -> go n input''
     Just ('-', input') ->
       case alexGetChar input' of
         Nothing            -> err input
         Just ('}',input'') -> go (n - 1) input''
         Just (_, input'')  -> go n input''
     Just (x, input')   ->
       go n input'
  --
  err input = alexSetInput input >> lexError "File ended in nested comment!"

-- ----------------------------------------------------------------------------
--
-- Lexeme Completion Actions
--
-- ----------------------------------------------------------------------------

standardMk :: (AlexPosn -> ByteString -> Lexeme) ->
              AlexInput -> Int64 ->
              Alex Lexeme
standardMk f (p, _, str) len = return (f p (ByteString.take len str))

mkIntegerConst :: Int -> Char -> AlexInput -> Int64 -> Alex Lexeme
mkIntegerConst base letter (p, _, str) len =
  do let basebytestr = ByteString.take len str
         secondByte  = ByteString.index basebytestr 1
         secondChar  = chr (fromIntegral secondByte)
     case () of
       () | ByteString.length basebytestr < 2 ->
              buildIntConst basebytestr
          | secondChar `elem` "bBoOdDxX"      ->
              buildIntConst (ByteString.drop 2 basebytestr)
          | otherwise   ->
              buildIntConst basebytestr
 where
  buildIntConst :: ByteString -> Alex Lexeme
  buildIntConst bstr =
    case ByteString.unsnoc bstr of
      Nothing -> lexError "Empty integer constant is weird."
      Just (start, lastb)
        | chr (fromIntegral lastb) == 'K' -> return (build (2 ^ 10) start)
        | chr (fromIntegral lastb) == 'M' -> return (build (2 ^ 20) start)
        | chr (fromIntegral lastb) == 'G' -> return (build (2 ^ 30) start)
        | chr (fromIntegral lastb) == 'T' -> return (build (2 ^ 40) start)
        | otherwise                       -> return (build 1        bstr)
  --
  build mult v = buildConstant (\ x -> IntConst p (mult * x) base) base v

mkVectorConst :: Int -> Int -> AlexInput -> Int64 -> Alex Lexeme
mkVectorConst base bitsPerChar (p, _, str) len =
  do let cleanbs = ByteString.drop 1
                    (ByteString.filter (/= under)
                      (ByteString.take len str))
         bitlen  = bitsPerChar * (fromIntegral (ByteString.length cleanbs))
     return (buildConstant (\ x -> VecConst p x base bitlen) base cleanbs)
 where under = fromIntegral (ord '_')

buildConstant :: (Integer -> Lexeme) -> Int -> ByteString -> Lexeme
buildConstant f base bstr = f (go (unpack bstr) 0)
 where
  go :: [Word8] -> Integer -> Integer
  go [] acc       = acc
  go (95:rst) acc = go rst acc -- 95 == '_'
  go (f :rst) acc = go rst ((acc * baseI) + digitToInt' (chr (fromIntegral f)))
  --
  baseI :: Integer
  baseI = fromIntegral base
  --
  digitToInt' :: Char -> Integer
  digitToInt' = fromIntegral . digitToInt

fromBS :: ByteString -> String
fromBS = map (chr . fromIntegral) . unpack

mkReservedId :: AlexInput -> Int64 -> Alex Lexeme
mkReservedId = standardMk (\ a b -> ReservedId a (fromBS b))

mkReservedSym :: AlexInput -> Int64 -> Alex Lexeme
mkReservedSym = standardMk (\ a b -> ReservedSym a (fromBS b) False)

mkVarId :: AlexInput -> Int64 -> Alex Lexeme
mkVarId = standardMk VarId

mkConId :: AlexInput -> Int64 -> Alex Lexeme
mkConId = standardMk ConId

mkVarSymId :: AlexInput -> Int64 -> Alex Lexeme
mkVarSymId = standardMk VarSymId

mkConSymId :: AlexInput -> Int64 -> Alex Lexeme
mkConSymId = standardMk ConSymId

mkBinConst :: AlexInput -> Int64 -> Alex Lexeme
mkBinConst = mkIntegerConst 2 'b'

mkOctConst :: AlexInput -> Int64 -> Alex Lexeme
mkOctConst = mkIntegerConst 8 'o'

mkDecConst :: AlexInput -> Int64 -> Alex Lexeme
mkDecConst = mkIntegerConst 10 'd'

mkHexConst :: AlexInput -> Int64 -> Alex Lexeme
mkHexConst = mkIntegerConst 16 'x'

mkBinVecConst :: AlexInput -> Int64 -> Alex Lexeme
mkBinVecConst = mkVectorConst 2 1

mkOctVecConst :: AlexInput -> Int64 -> Alex Lexeme
mkOctVecConst = mkVectorConst 8 3

mkHexVecConst :: AlexInput -> Int64 -> Alex Lexeme
mkHexVecConst = mkVectorConst 16 4

mkFloatConst :: AlexInput -> Int64 -> Alex Lexeme
mkFloatConst (p, _, bstr) _ =
  case ByteString.unsnoc bstr of
    Nothing -> lexError "Empty float constant is weird."
    Just (start, lastb)
      | chr (fromIntegral lastb) == 'f' -> makeFloat start
      | chr (fromIntegral lastb) == 'F' -> makeFloat start
      | chr (fromIntegral lastb) == 'g' -> makeDouble start
      | chr (fromIntegral lastb) == 'G' -> makeDouble start
      | otherwise                       -> makeFloat bstr
 where
  makeFloat str =
    let cstr = map (chr . fromIntegral) (unpack str)
    in case reads cstr of
         [(x, "")] -> return (FloatConst p (FVal x))
         _         -> lexError ("Error parsing " ++ cstr ++ " as Float")

  makeDouble str =
    let cstr = map (chr . fromIntegral) (unpack str)
    in case reads cstr of
         [(x, "")] -> return (FloatConst p (DVal x))
         _         -> lexError ("Error parsing " ++ cstr ++ " as Double")

lexError :: String -> Alex a
lexError str =
  do (p, _, input) <- alexGetInput
     alexError (showPosn p ++ ": " ++ str ++
       (if (not (ByteString.null input))
          then " before " ++ show (chr (fromIntegral (ByteString.head input)))
          else " at end of file"))

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ":" ++ show col

-- ----------------------------------------------------------------------------
--
-- Local User State Definition / Access Functions
--
-- ----------------------------------------------------------------------------

data AlexUserState = AlexUserState {
       -- Phase 1 state
       bufferedToken    :: Maybe Lexeme
     , isFirstLexeme    :: Bool
     , wantOpenBrace    :: Bool
     , justManufactured :: Bool
     , curLine          :: Int
       -- Phase 2 state
     , curStack         :: [Int]
     , savedTokens      :: [Lexeme]
     }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
    bufferedToken    = Nothing
  , isFirstLexeme    = True
  , wantOpenBrace    = False
  , justManufactured = False
  , curLine          = minBound
  , curStack         = []
  , savedTokens      = []
  }

-- ----------------------------------------------------------------------------
--
-- Phase 1 Layout Handling (Generates "the input to L")
--
-- ----------------------------------------------------------------------------

getUserData :: Alex AlexUserState
getUserData  = Alex $ \s@AlexState{alex_ust=ud} -> Right (s, ud)

setUserData :: AlexUserState -> Alex ()
setUserData ust = Alex $ \s -> Right (s{alex_ust=ust}, ())

nextWithIndentInfo :: Alex Lexeme
nextWithIndentInfo = do
  state <- getUserData
  nexttok <- case bufferedToken state of
               Nothing -> alexMonadScan
               Just x  -> return x
  case () of
    () | wantOpenBrace state && not (isOpenBrace nexttok) ->
            do setUserData state{ bufferedToken    = Just nexttok
                                , isFirstLexeme    = False
                                , wantOpenBrace    = False
                                , justManufactured = True
                                , curLine          = lineNum nexttok }
               return (newIndentToken IndentMarkB nexttok)
       | isFirstLexeme state && not (isBraceOrModule nexttok) ->
            do setUserData state{ bufferedToken    = Just nexttok
                                , isFirstLexeme    = False
                                , wantOpenBrace    = False
                                , justManufactured = True
                                , curLine          = lineNum nexttok }
               return (newIndentToken IndentMarkB nexttok)
       | not (justManufactured state) && isNewLine state nexttok ->
            do setUserData state{ bufferedToken    = Just nexttok
                                , isFirstLexeme    = False
                                , wantOpenBrace    = False
                                , justManufactured = True
                                , curLine          = lineNum nexttok }
               return (newIndentToken IndentMarkA nexttok)
       | tokenWantsOpenBrace nexttok ->
            do setUserData state{ bufferedToken    = Nothing
                                , isFirstLexeme    = False
                                , wantOpenBrace    = True
                                , justManufactured = False
                                , curLine          = lineNum nexttok }
               return nexttok
       | otherwise ->
            do setUserData state{ bufferedToken    = Nothing
                                , isFirstLexeme    = False
                                , wantOpenBrace    = False
                                , justManufactured = False
                                , curLine          = lineNum nexttok }
               return nexttok

-- ----------------------------------------------------------------------------
--
-- Phase 2 Layout Handling (the "L" function)
--
-- ----------------------------------------------------------------------------

setIndentStack :: [Int] -> Alex ()
setIndentStack stack = do
  st <- getUserData
  setUserData st{ curStack = stack }

saveToken :: Lexeme -> Alex ()
saveToken tok = do
  st <- getUserData
  setUserData st{ savedTokens = tok : savedTokens st }

nextFunctionLState :: Alex (Lexeme, [Int])
nextFunctionLState  = do
  state <- getUserData
  tok <- case savedTokens state of
           []       ->    nextWithIndentInfo
           (x:rest) -> do setUserData state{ savedTokens = rest }
                          return x
  return (tok, curStack state)

nextToken :: Alex Lexeme
nextToken = do
  (head, stack) <- nextFunctionLState
  case (head, stack) of
    (IndentMarkA n, (m : ms)) | m == n ->    return injectedSemi
                              | n <  m -> do saveToken head
                                             setIndentStack ms
                                             return injectedCloseBrace
    (IndentMarkA n, ms)                ->    nextToken
    --
    (IndentMarkB n, (m : ms)) | n >  m -> do setIndentStack (n : (m : ms))
                                             return injectedOpenBrace
    (IndentMarkB n, [])                -> do setIndentStack [n]
                                             return injectedOpenBrace
    (IndentMarkB n, ms)                -> do setIndentStack ms
                                             saveToken (IndentMarkA n)
                                             saveToken injectedCloseBrace
                                             return injectedOpenBrace
    (x, (0 : ms)) | nonInjectedClose x -> do setIndentStack ms
                                             return x
    (x, ms) | nonInjectedClose x       -> lexError "Bad case in indent handler!"
    (x, ms) | nonInjectedOpen x        -> do setIndentStack (0 : ms)
                                             return x
    (EOF, [])                          ->    return EOF
    (EOF, (m : ms))                    -> do setIndentStack ms
                                             saveToken EOF
                                             return injectedCloseBrace
    (t, ms)                            ->    return t

-- ----------------------------------------------------------------------------
--
-- External Interfaces
--
-- ----------------------------------------------------------------------------

tokenize :: (Lexeme -> Alex a) -> Alex a
tokenize f = nextToken >>= f

runParser :: FilePath -> ByteString -> Alex a -> Either String a
runParser _ bstr = runAlex bstr

}

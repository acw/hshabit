import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Debug.Trace
import Syntax.Parser
import Syntax.Tokens
import System.Console.GetOpt
import System.Environment
import System.IO

data CompilerMode = Normal | LexOnly | ParseOnly | Help | Version
 deriving (Show, Eq)

data Options = Options {
    optMode       :: CompilerMode
  , optVerbose    :: Bool
  , optOutputFile :: Maybe FilePath
  }
 deriving (Show)

defaultOptions :: Options
defaultOptions = Options {
    optMode       = Normal
  , optVerbose    = False
  , optOutputFile = Nothing
  }

setMode :: CompilerMode -> Options -> Options
setMode c o = o{ optMode = c }

setOutputFile :: FilePath -> Options -> Options
setOutputFile f o = o{ optOutputFile = Just f }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['v']     ["verbose"]   (NoArg (\ o -> o{ optVerbose = True }))
                     "Print out quite a lot of status information."
  , Option ['V']     ["version"]   (NoArg (setMode Version))
                     "Print out version information and then exit."
  , Option ['h','?'] ["help"]      (NoArg (setMode Help))
                     "Print out help information."
  , Option ['p']     ["parserOnly"] (NoArg (setMode ParseOnly))
                     "Run only the lexer, printing its output."
  , Option ['l']     ["lexerOnly"] (NoArg (setMode LexOnly))
                     "Run only the lexer, printing its output."
  , Option ['o']     ["output"]    (ReqArg setOutputFile "FILE")
                     "Write the output to the given file."
  ]

commandStr :: String
commandStr = "\nUSAGE: hshabit [flags] file ...\n"

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    (o, n, []  ) -> dispatchModes (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> putStrLn ("ERROR: " ++ show errs)

dispatchModes :: (Options, [String]) -> IO ()
dispatchModes (opts, filenames)
  | optMode opts == Version = putStrLn "Bang Compiler Version 1.0.0"
  | optMode opts == Help    = putStrLn (usageInfo commandStr options)
  | null filenames          = putStrLn "No input files."
  | length filenames > 1    = putStrLn "Too many input files."
  | otherwise               = runCompiler opts (head filenames)

runCompiler :: Options -> FilePath -> IO ()
runCompiler opts file = do
  withFile file ReadMode $ \ ihndl -> do
    withOutputFile (optOutputFile opts) $ \ ohndl -> do
      let compiler = buildCompiler opts
      input <- BS.hGetContents ihndl
      BS.hPut ohndl (compiler file input)

withOutputFile :: Maybe FilePath -> (Handle -> IO r) -> IO r
withOutputFile Nothing   action = action stdout
withOutputFile (Just fp) action = withFile fp WriteMode action

buildCompiler :: Options -> FilePath -> ByteString -> ByteString
buildCompiler opts =
  case optMode opts of
    Version   -> error "Internal error: building compiler from Version?"
    Help      -> error "Internal error: building compiler from Help?"
    LexOnly   -> \ file bstr ->
                  case runParser file bstr runLexer of
                    Left err -> toByteString ("ERROR: " ++ show err ++ "\n")
                    Right x  -> toByteString (x ++ "\n")
    ParseOnly -> \ file bstr ->
                     case runParser file bstr parseHabit of
                       Left err -> toByteString ("ERROR: " ++ show err ++ "\n")
                       Right x  -> toByteString (show x ++ "\n")
    Normal    -> error "No parser yet" -- toByteString . show . parse . lexer
 where
  runLexer :: Alex String
  runLexer = tokenize (\ l ->
                        case (trace (show l) l) of
                          EOF -> return "\n"
                          x   -> do rest <- runLexer
                                    return (show x ++ "\n" ++ rest))

toByteString :: String -> ByteString
toByteString ls = BS.pack (map (fromIntegral . ord) ls)

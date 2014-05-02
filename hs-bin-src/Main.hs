import Compiler.FrontEnd.Depends(buildMakeDepends)
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS
import Misc.Output(Output(..))
import Syntax.Parser(parse)
import System.FilePath(takeFileName)
import System.IO
import System.ParseArgs
import Text.PrettyPrint.ANSI.Leijen(displayIO)

main :: IO ()
main = parseCommandLine >>= \ c ->
  case c of
    HELP       -> displayIO stderr habitHelp
    VERSION    -> hPutStrLn stderr "The High Speed Habit Compiler, Version 0.1"
    LEX{}      -> runCompiler c (scanner c)
    PARSE{}    -> runCompiler c parse
    DEPENDS{}  -> runCompiler c buildMakeDepends

runCompiler :: Output a =>
               Command ->
               (Maybe FilePath -> ByteString -> a) ->
               IO ()
runCompiler cmd compiler =
  withFile (getInputFile cmd) ReadMode $ \ ihndl ->
    withOutputFile (getOutputFile cmd) $ \ ohndl ->
      do input <- BS.hGetContents ihndl
         let fname     = takeFileName (getInputFile cmd)
             output    = compiler (Just fname) input
         BS.hPut ohndl (toByteString output)

withOutputFile :: FilePath -> (Handle -> IO ()) -> IO ()
withOutputFile "" action = action stdout
withOutputFile f  action = withFile f WriteMode action

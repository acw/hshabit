import Data.Char(toUpper)
import System.Directory
import System.FilePath
import Test.Framework(Test, defaultMain, testGroup)
import Test.Framework.Providers.Program


main :: IO ()
main =
  do ltests <- buildLexerTests
     ptests <- buildParserTests
     defaultMain [ltests,ptests]

buildLexerTests :: IO Test
buildLexerTests  =
  do stests <- buildGoldTests "scanner"    ["lex"]
     itests <- buildGoldTests "indentinfo" ["lex","-i"]
     etests <- buildGoldTests "lexer"      ["lex","-e"]
     return (testGroup "Lexer Tests" [stests, itests, etests])

buildParserTests :: IO Test
buildParserTests  =
  do ptests <- buildGoldTests "parser"     ["parse"]
     return (testGroup "Parser Tests" [ptests])

buildGoldTests :: String -> [String] -> IO Test
buildGoldTests name args =
  do everything <- getDirectoryContents ("tests/" ++ name)
     let golds = filter (\ x -> takeExtension x == ".gld") everything
         tests = map (\ x -> replaceExtension x ".hbt") golds
     testResults <- mapM (\ x -> readFile ("tests/" ++ name ++ "/" ++ x)) golds
     return (testGroup (capWord name ++ " Tests")
                       (zipWith buildGold tests testResults))
 where
  buildGold t g =
    testProgramOutput
      (name ++ "/" ++ t)
      "./dist/build/hshabit/hshabit"
      (args ++ ["tests/" ++ name ++ "/" ++ t])
      (Just (== g))
      Nothing
  capWord "" = ""
  capWord (x:rest) = toUpper x : rest


import Data.Char(toUpper)
import System.Directory
import System.FilePath
import Test.Framework(Test, defaultMain, testGroup)
import Test.Framework.Providers.Program


main :: IO ()
main =
  do ltests <- buildGoldTests "lexer" "-l"
     ptests <- buildGoldTests "parser" "-p"
     defaultMain [ltests,ptests]

buildGoldTests :: String -> String -> IO Test
buildGoldTests name arg =
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
      [arg, "tests/" ++ name ++ "/" ++ t]
      (Just (== g))
      Nothing
  capWord "" = ""
  capWord (x:rest) = toUpper x : rest


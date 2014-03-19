import System.Directory
import System.FilePath
import Test.Framework(Test, defaultMain, testGroup)
import Test.Framework.Providers.Program


main :: IO ()
main =
  do ptests <- buildParserTests
     defaultMain [ptests]

buildParserTests :: IO Test
buildParserTests =
  do everything <- getDirectoryContents "tests/parser"
     let golds = filter (\ x -> takeExtension x == ".gld") everything
         tests = map (\ x -> replaceExtension x ".hbt") golds
     testResults <- mapM (\ x -> readFile ("tests/parser/" ++ x)) golds
     return (testGroup "Parser Tests" (zipWith buildParserGold tests testResults))
 where
  buildParserGold t g =
    testProgramOutput
      ("parser/" ++ t)
      "./dist/build/hshabit/hshabit"
      ["-p", "tests/parser/" ++ t]
      (Just (== g))
      Nothing

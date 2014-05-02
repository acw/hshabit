module System.ParseArgs( Command(..)
                       , habitHelp
                       , parseCommandLine
                       )
 where

import Data.ByteString.Lazy(ByteString)
import Data.Monoid
import Options.Applicative
import Options.Applicative.Help.Core
import Syntax.Layout(scanWithLayout)
import Syntax.Lexeme(Lexeme)
import Syntax.Scanner(scan)
import Text.PrettyPrint.ANSI.Leijen(SimpleDoc,renderPretty)

data Command =
    HELP
  | VERSION
  | LEX { getOutputFile       :: FilePath
        , getInputFile        :: FilePath
        , scanner             :: Maybe FilePath -> ByteString ->[Lexeme]
        }
  | PARSE { getOutputFile     :: FilePath
          , getInputFile      :: FilePath
          }
  | DEPENDS { getOutputFile     :: FilePath
            , getInputFile      :: FilePath
            }

habitOptions :: Parser Command
habitOptions = subparser (helpCmd <> versionCmd <>
                          lexCmd <> parseCmd <> dependCmd)
 where
  helpCmd     = command "help"
                  (info (pure HELP)
                  (progDesc "Show the help."))
  versionCmd  = command "version"
                  (info (pure VERSION)
                  (progDesc "Show the version."))
  lexCmd      = command "lex"
                  (info lexeropts
                  (progDesc "Lex an input file."))
  parseCmd    = command "parse"
                  (info parseropts
                  (progDesc "Parse an input file."))
  dependCmd   = command "depends"
                  (info dependopts
                  (progDesc "Generate dependency info for a file."))
  --
  lexeropts   = LEX <$> outputFlag <*> inputFile <*> emitBlFlag
  emitBlFlag  = flag scan scanWithLayout (long "emitBlocks" <> short 'e'
                                   <> help "Emit inferred block tokens.")
  --
  parseropts  = PARSE <$> outputFlag <*> inputFile
  dependopts  = DEPENDS <$> outputFlag <*> inputFile
  --
  outputFlag  = strOption (long "output" <> short 'o' <> metavar "FILE" <> value ""
                                         <> help "Output to the given file.")
  inputFile   = argument str (metavar "FILE")


habitHelp :: SimpleDoc
habitHelp = renderPretty 0.9 80 (helpText (parserHelp undefined habitOptions))

parseCommandLine :: IO Command
parseCommandLine  = execParser (info (habitOptions <**> helper) idm)

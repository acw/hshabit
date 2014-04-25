module Syntax.Posn
 where

data Posn = EmptyPosn
          | ConsolePosn !Int !Int
          | FilePosn String !Int !Int
 deriving (Eq)

instance Show Posn where
  show EmptyPosn = "<->"
  show (ConsolePosn l c) = "<console:" ++ show l ++ ":" ++ show c ++ ">"
  show (FilePosn f l c) = "<" ++ f ++ ":" ++ show l ++ ":" ++ show c ++ ">"

lineNum :: Posn -> Int
lineNum EmptyPosn         = error "ERROR: lineNum called on manufactured posn!"
lineNum (ConsolePosn l _) = l
lineNum (FilePosn _ l _)  = l

columnNum :: Posn -> Int
columnNum EmptyPosn         = error "ERROR: lineNum called on manufactured posn!"
columnNum (ConsolePosn _ c) = c
columnNum (FilePosn _ _ c)  = c

isEmptyPosn :: Posn -> Bool
isEmptyPosn EmptyPosn = True
isEmptyPosn _         = False

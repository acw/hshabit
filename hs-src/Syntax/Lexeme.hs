module Syntax.Lexeme
 where

import Data.ByteString.Lazy(ByteString, unpack)
import Data.Char(chr)
import Syntax.Posn

data Lexeme = ReservedId  Posn String
            | ReservedSym Posn String
            | VarId       Posn ByteString
            | ConId       Posn ByteString
            | QVarId      Posn ByteString
            | QConId      Posn ByteString
            | VarSymId    Posn ByteString
            | ConSymId    Posn ByteString
            | QVarSymId   Posn ByteString
            | QConSymId   Posn ByteString
            | IntConst    Posn Integer    Int
            | VecConst    Posn Integer    Int Int
            | FloatConst  Posn FloatVal
            | EOF         Posn
 deriving (Eq)

data FloatVal = FVal Float | DVal Double
 deriving (Eq)

instance Show Lexeme where
  show (ReservedId  p st)     = "lexeme:" ++ show p ++ ":" ++ "RId:" ++ st
  show (ReservedSym p st)     = "lexeme:" ++ show p ++ ":" ++ "RSym:" ++ st
  show (VarId       p bs)     = "lexeme:" ++ show p ++ ":" ++ "VarId:" ++
                                map (chr . fromIntegral) (unpack bs)
  show (ConId       p bs)     = "lexeme:" ++ show p ++ ":" ++ "ConId:" ++
                                map (chr . fromIntegral) (unpack bs)
  show (QVarId      p bs)     = "lexeme:" ++ show p ++ ":" ++ "QVarId:" ++
                                map (chr . fromIntegral) (unpack bs)
  show (QConId      p bs)     = "lexeme:" ++ show p ++ ":" ++ "QConId:" ++
                                map (chr . fromIntegral) (unpack bs)
  show (VarSymId    p bs)     = "lexeme:" ++ show p ++ ":" ++ "VarSymId:" ++
                                map (chr . fromIntegral) (unpack bs)
  show (ConSymId    p bs)     = "lexeme:" ++ show p ++ ":" ++ "ConSymId:" ++
                                map (chr . fromIntegral) (unpack bs)
  show (QVarSymId    p bs)    = "lexeme:" ++ show p ++ ":" ++ "QVarSymId:" ++
                                map (chr . fromIntegral) (unpack bs)
  show (QConSymId    p bs)    = "lexeme:" ++ show p ++ ":" ++ "QConSymId:" ++
                                map (chr . fromIntegral) (unpack bs)
  show (IntConst    p vl b)   = "lexeme:" ++ show p ++ ":Int:base" ++ show b ++
                                ":" ++ show vl
  show (VecConst    p vl b l) = "lexeme:" ++ show p ++ ":Vec:base" ++ show b ++
                                "len" ++ show l ++ ":" ++ show vl
  show (FloatConst  p vl)     = "lexeme:" ++ show p ++ ":Float:" ++ show vl
  show (EOF         _)        = "lexeme:EOF"

instance Show FloatVal where
  show (FVal f) = show f ++ "f"
  show (DVal d) = show d ++ "d"

getPosn :: Lexeme -> Posn
getPosn (ReservedId  p _  )   = p
getPosn (ReservedSym p _  )   = p
getPosn (VarId       p _  )   = p
getPosn (ConId       p _  )   = p
getPosn (QVarId      p _  )   = p
getPosn (QConId      p _  )   = p
getPosn (VarSymId    p _  )   = p
getPosn (ConSymId    p _  )   = p
getPosn (QVarSymId   p _  )   = p
getPosn (QConSymId   p _  )   = p
getPosn (IntConst    p _ _)   = p
getPosn (VecConst    p _ _ _) = p
getPosn (FloatConst  p _  )   = p
getPosn (EOF         p)       = p

getToken :: Lexeme -> String
getToken (ReservedId  _ t  )   = t
getToken (ReservedSym _ t  )   = t
getToken (VarId       _ b  )   = show b
getToken (ConId       _ b  )   = show b
getToken (QVarId      _ b  )   = show b
getToken (QConId      _ b  )   = show b
getToken (VarSymId    _ b  )   = show b
getToken (ConSymId    _ b  )   = show b
getToken (QVarSymId   _ b  )   = show b
getToken (QConSymId   _ b  )   = show b
getToken (IntConst    _ v _)   = show v
getToken (VecConst    _ v _ _) = show v
getToken (FloatConst  _ v  )   = show v
getToken (EOF         _)       = "EOF"


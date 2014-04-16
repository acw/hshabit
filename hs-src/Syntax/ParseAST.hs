module Syntax.ParseAST(
                        HabitModule(..)
                      , Decl(..), ImportMods(..), Constraint(..), Instance(..), FixityType(..)
                      , StructField, BitdataField
                      , Expr(..), Statement(..)
                      , ExprCase(..), BlockCase(..)
                      , Pattern(..)
                      , Type(..), Kind(..), ConstVal(..), Predicate(..), Name(..)
                      , startName
                      , translateConst
                      , defaultMod
                      )
 where

import Data.ByteString.Lazy(ByteString,unpack)
import Data.Char(chr)
import Syntax.Lexeme
import Syntax.Posn

data HabitModule = HabitModule Name [Decl]
 deriving (Show)

data Decl        = ImportDecl Posn Bool Name (Maybe Name) ImportMods
                 | FixityDecl Posn FixityType Bool (Maybe Integer) [Name]
                 | TypeSigDecl Posn Name Type
                 | TypeDecl Posn Type Type
                 | StructDecl Posn Name (Maybe Type) [StructField] [Name]
                 | BitdataDecl Posn Name (Maybe Type) [BitdataField] [Name]
                 | AreaDecl Posn Name (Maybe Expr) Type
                 | ClassDecl Posn Type (Maybe Type) [Constraint] [Decl]
                 | InstanceDecl Posn [Instance]
                 | DataDecl Posn Type [Type] [Name] [Constraint]
                 | EquationDecl Posn Name [Pattern] (Maybe Expr) Expr
                 | LocalDecl [Decl] [Decl]
 deriving (Show)

data Expr = ExprConst  ConstVal
          | ExprRef    Name
          | ExprTuple  Posn [Expr]
          | ExprLet    Posn [Decl] Expr
          | ExprIf     Posn Expr Expr Expr
          | ExprIfM    Posn Expr [Statement] [Statement]
          | ExprCase   Posn Expr [ExprCase]
          | ExprCaseM  Posn Expr [BlockCase]
          | ExprDo     Posn [Statement]
          | ExprLambda Posn [Pattern] Expr
          | ExprType   Posn Expr Type
          | ExprApply  Expr [Expr]
          | ExprInfix  Expr [(Name, Expr)]
          | ExprFldRef Posn Expr Name
          | ExprUpdate Posn Expr [(Name, Maybe Expr)]
          | ExprBuild  Posn Expr [(Name, Expr)]
  deriving (Show)

data Statement = StmtExpr Expr
               | StmtBind Posn Name Statement
               | StmtLet  Posn [Decl] [Statement]
               | StmtIf   Posn Expr [Statement] [Statement]
               | StmtCase Posn Expr [BlockCase]
  deriving (Show)


data ImportMods = IncludeOnly [Name]
                | HidingNames [Name]
                | NoMods
 deriving (Show)

data Constraint = FunDep [Name] [Name]
                | Superclass Name [Type]
 deriving (Show)

data Instance = Instance [Predicate] [Predicate] [Decl]
 deriving (Show)

data FixityType = FixityLeft | FixityRight | FixityBoth
  deriving (Show)

type StructField = (Maybe Name, Maybe Expr, Type)
type BitdataField = (Name, [Either Expr (Name, Maybe Expr, Type)])

data ExprCase = ECase Pattern (Maybe Expr) Expr
              | EWhereCase [Decl] [ExprCase]
  deriving (Show)

data BlockCase = BCase Pattern (Maybe Expr) [Statement]
               | BWhereCase [Decl] [BlockCase]
  deriving (Show)

data Pattern = PatBlank
             | PatConst ConstVal
             | PatRef Name
             | PatTyped Pattern Type
             | PatTuple [Pattern]
             | PatStruct Name [(Name, Maybe Pattern)]
             | PatNamed Name Pattern
             | PatApply Pattern [Pattern]
             | PatInfix Pattern [(Name, Pattern)]
  deriving (Show)

data Type = WithPredicates [Predicate] Type
          | TypeRef Name
          | TypeUnit
          | TypeInt Integer
          | TypeLabel Name
          | TypeKind Type Kind
          | TypeApp Type [Type]
          | TypeTuple [Type]
          | TypeInfix Type [(Name, Type)]
  deriving (Show)

data Kind = KindStar | KindType | KindNat | KindArea | KindLabel
          | KindFun Kind Kind
  deriving (Show)

data ConstVal = ConstInt Posn Integer Int
              | ConstVec Posn Integer Int Int
              | ConstFloat Posn Float
              | ConstDouble Posn Double
              | ConstUnit Posn
              | ConstLabel Posn Name
  deriving (Show)

translateConst :: Lexeme -> ConstVal
translateConst (IntConst a b c) = ConstInt a b c
translateConst (VecConst a b c d) = ConstVec a b c d
translateConst (FloatConst a (FVal f)) = ConstFloat a f
translateConst (FloatConst a (DVal d)) = ConstDouble a d
translateConst _ = error "Incorrect lexeme to translateConst"

data Predicate = Predicate (Maybe Type) Type
               | SelectPredicate Type Name Type
               | FailPredicate Predicate
  deriving (Show)

data Name       = Name Posn [String] String
 deriving (Show)

defaultMod :: Name
defaultMod = Name EmptyPosn [] "Main"

startName :: Lexeme -> Name
startName (ReservedId p x)  = Name p []              x
startName (ReservedSym p x) = Name p []              x
startName (VarId p bs)      = Name p []              (fromBS bs)
startName (ConId p bs)      = Name p []              (fromBS bs)
startName (VarSymId p bs)   = Name p []              (fromBS bs)
startName (ConSymId p bs)   = Name p []              (fromBS bs)
startName (QVarId p bs)     = Name p (moduleName bs) (symbolName bs)
startName (QConId p bs)     = Name p (moduleName bs) (symbolName bs)
startName (QVarSymId p bs)  = Name p (moduleName bs) (symbolName bs)
startName (QConSymId p bs)  = Name p (moduleName bs) (symbolName bs)
startName x                 = error ("Bad token for startName: " ++ show x)

moduleName :: ByteString -> [String]
moduleName bstr = go (fromBS bstr)
 where
  go str =
    case span (/= '.') str of
      (_, "")   -> []
      (x, rest) -> x : go (dropWhile (== '.') rest)

symbolName :: ByteString -> String
symbolName bstr = go (fromBS bstr)
 where
  go str =
    case span (/= '.') str of
      (x, "")   -> x
      (_, rest) -> go (dropWhile (== '.') rest)

fromBS :: ByteString -> String
fromBS = map (chr . fromIntegral) . unpack



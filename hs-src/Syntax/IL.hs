module Syntax.IL where

import Syntax.Posn

data Name = Name

data HabitModule = HabitModule

data Declaration = DEquation 

data Statement = SExpr { stmtPosn :: Posn,
                         stmtExpr :: Expression }
               | SBind { stmtPosn :: Posn,
                         stmtName :: Name,
                         stmtVal  :: Expression }

data Expression = EConst  { exprPosn :: Posn, exprType :: Type,
                            constVal :: Constant }
                | ERef    { exprPosn :: Posn, exprType :: Type,
                            refName :: Name }
                | ETuple  { exprPosn :: Posn, exprType :: Type,
                            tupleExprs :: [Expression] }
                | ELet    { exprPosn :: Posn, exprType :: Type,
                            letDecls :: [Declaration],
                            letBody :: Expression }
                | EIf     { exprPosn :: Posn, exprType :: Type,
                            ifTest :: Expression,
                            ifCons :: Expression,
                            ifAlt  :: Expression }
                | ECase   { exprPosn :: Posn, exprType :: Type,
                            caseExpr :: Expression,
                            caseArms :: [CaseArm] }
                | EDo     { exprPosn :: Posn, exprType :: Type,
                            doStmts :: [Statement] }
                | ELambda { exprPosn :: Posn, exprType :: Type,
                            lambdaArgs :: [Pattern],
                            lambdaBody :: Expression }
                | ECall   { exprPosn  :: Posn, exprType :: Type,
                            callFun   :: Expression,
                            callArgs  :: [Expression] }
                | EFldRef { exprPosn  :: Posn, exprType :: Type,
                            refExpr   :: Expression,
                            refField  :: Name }
                | EUpdate { exprPosn  :: Posn, exprType :: Type,
                            updExpr   :: Expression,
                            updFields :: [(Name, Maybe Expression)] }
                | EBuild  { exprPosn  :: Posn, exprType :: Type,
                            bldExpr   :: Expression,
                            bldFields :: [(Name, Expression)] }

data CaseArm = CaseArm

data Pattern = Pattern

data Kind = Kind

data Type = TUnit       { typePosn :: Posn }
          | TRef        { typePosn :: Posn,
                          trefName  :: Name }
          | TLabel      { typePosn :: Posn,
                          tlabName  :: Name }
          | TInt        { typePosn :: Posn,
                          tintValue :: Integer }
          | TTuple      { typePosn :: Posn,
                          ttupValues :: [Type] }
          | TFun        { typePosn  :: Posn,
                          funArgs   :: [Type],
                          funResult :: Type }
          | TApp        { typePosn :: Posn,
                          tappFun :: Type,
                          tappArgs :: [Type] }
          | TPredicated { typePosn :: Posn,
                          tPreds :: [Predicate],
                          tPredType :: Type }
          | TypeKind    { typePosn :: Posn,
                          tkindKind :: Kind,
                          tkindType :: Type }

data Predicate = Predicate Name [Name]
               | PredEquals Predicate Type
               | PredFails Predicate

data Constant = CInteger { intValue :: Integer, intBase :: Int }
              | CFloat Float
              | CDouble Double
              | CUnit
              | CLabel Name

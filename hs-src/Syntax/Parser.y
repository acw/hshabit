--  vim:set filetype=haskell:
{
module Syntax.Parser(parse, HabitModule) where

import Data.ByteString.Lazy(ByteString,unpack)
import Data.Char(chr)
import Syntax.Layout
import Syntax.Lexeme
import Syntax.ParseAST
import Syntax.Posn

}

%name parseHabit

%tokentype { Lexeme }
%token
  "area"        { ReservedId $$ "area" }
  "as"          { ReservedId $$ "as" }
  "bitdata"     { ReservedId $$ "bitdata" }
  "case"        { ReservedId $$ "case" }
  "class"       { ReservedId $$ "class" }
  "data"        { ReservedId $$ "data" }
  "deriving"    { ReservedId $$ "deriving" }
  "do"          { ReservedId $$ "do" }
  "else"        { ReservedId $$ "else" }
  "fails"       { ReservedId $$ "fails" }
  "hiding"      { ReservedId $$ "hiding" }
  "if"          { ReservedId $$ "if" }
  "import"      { ReservedId $$ "import" }
  "in"          { ReservedId $$ "in" }
  "infix"       { ReservedId $$ "infix" }
  "infixl"      { ReservedId $$ "infixl" }
  "infixr"      { ReservedId $$ "infixr" }
  "instance"    { ReservedId $$ "instance" }
  "lab"         { ReservedId $$ "lab" }
  "let"         { ReservedId $$ "let" }
  "module"      { ReservedId $$ "module" }
  "nat"         { ReservedId $$ "nat" }
  "of"          { ReservedId $$ "of" }
  "qualified"   { ReservedId $$ "qualified" }
  "struct"      { ReservedId $$ "struct" }
  "then"        { ReservedId $$ "then" }
  "type"        { ReservedId $$ "type" }
  "where"       { ReservedId $$ "where" }

  "("           { ReservedSym $$ "("  }
  ")"           { ReservedSym $$ ")"  }
  "|"           { ReservedSym $$ "|"  }
  "="           { ReservedSym $$ "="  }
  ","           { ReservedSym $$ ","  }
  "`"           { ReservedSym $$ "`"  }
  "{"           { ReservedSym $$ "{"  }
  ";"           { ReservedSym $$ ";"  }
  "}"           { ReservedSym $$ "}"  }
  "["           { ReservedSym $$ "["  }
  "]"           { ReservedSym $$ "]"  }
  "\\"          { ReservedSym $$ "\\" }
  "<-"          { ReservedSym $$ "<-" }
  "->"          { ReservedSym $$ "->" }
  "=>"          { ReservedSym $$ "=>" }
  "::"          { ReservedSym $$ "::" }
  "#."          { ReservedSym $$ "#." }
  "@"           { ReservedSym $$ "@"  }
  "_"           { ReservedSym $$ "_"  }
  "."           { ReservedSym $$ "."  }
  "*"           { ReservedSym $$ "*"  }
  "/"           { ReservedSym $$ "/"  }
  ":#"          { ReservedSym $$ ":#" }

  varid         { VarId _ _ }
  conid         { ConId _ _ }
  varsymid      { VarSymId _ _ }
  consymid      { ConSymId _ _ }
  qvarid        { QVarId _ _ }
  qconid        { QConId _ _ }
  qvarsymid     { QVarSymId _ _ }
  qconsymid     { QConSymId _ _ }
  int           { IntConst _ _ _ }
  vec           { VecConst _ _ _ _ }
  float         { FloatConst _ _ }

%%

-- These are defined in the Nov10 Habit report, page 28
Habit :: { HabitModule }
  : "{" Prog "}"
  { HabitModule defaultMod $2 }
  | "{" Prog ";" "}"
  { HabitModule defaultMod $2 }
  | "module" ModName "where" "{" Prog "}"
  { HabitModule $2 $5 }
  | "module" ModName "where" "{" Prog ";" "}"
  { HabitModule $2 $5 }

Prog :: { [Decl] }
  :
  { [] }
  | TopDecl
  { $1 }
  | Prog ";" TopDecl
  { $1 ++ $3 }

-- Declarations

TopDecl :: { [Decl] }
  : ImportDecl
  { [$1] }
  | FixityDecl
  { [$1] }
  | TypeDecl
  { [$1] }
  | StructDecl
  { [$1] }
  | BitdataDecl
  { [$1] }
  | AreaDecl
  { $1 }
  | ClassDecl
  { [$1] }
  | InstanceDecl
  { [$1] }
  | DataDecl
  { [$1] }
  | StandardDecl
  { $1 }

StandardDecl :: { [Decl] }
  : TypeSigDecl
  { $1 }
  | Equation
  { $1 }

DeclBlock :: { [Decl] }
  : "{" "}"
  { [] }
  | "{" StandardDecls "}"
  { $2 }
  | StandardDecl
  { $1 }

StandardDecls :: { [Decl] }
  : StandardDecl
  { $1 }
  | StandardDecls ";" StandardDecl
  { $1 ++ $3 }

-- Import Statements
ImportDecl :: { Decl }
  : "import" ModName
  { ImportDecl $1 False $2 Nothing NoMods }
  | "import" ModName ImportNameList
  { ImportDecl $1 False $2 Nothing (IncludeOnly $3) }
  | "import" "qualified" ModName
  { ImportDecl $1 True $3 Nothing NoMods }
  | "import" "qualified" ModName ImportNameList
  { ImportDecl $1 True $3 Nothing (IncludeOnly $4) }
  | "import" "qualified" ModName "as" ModName
  { ImportDecl $1 True $3 (Just $5) NoMods }
  | "import" "qualified" ModName ImportNameList "as" ModName
  { ImportDecl $1 True $3 (Just $6) (IncludeOnly $4) }
  | "import" ModName "hiding" ImportNameList
  { ImportDecl $1 False $2 Nothing (HidingNames $4) }
  | "import" "qualified" ModName "hiding" ImportNameList
  { ImportDecl $1 True $3 Nothing (HidingNames $5) }
  | "import" ModName "hiding" ImportNameList "as" ModName
  { ImportDecl $1 False $2 (Just $6) (HidingNames $4) }
  | "import" "qualified" ModName "hiding" ImportNameList "as" ModName
  { ImportDecl $1 True $3 (Just $7) (HidingNames $5) }

ImportNameList :: { [Name] }
  : "(" ")"
  { [] }
  | "(" NameList ")"
  { $2 }

NameList :: { [Name] }
  : Id
  { [$1] }
  | NameList "," Id
  { $1 ++ [$3] }

-- Fixity Declarations Statements

FixityDecl :: { Decl }
  : Assoc Prec CommaOpList
  { $1 False $2 $3 }
  | Assoc "type" Prec CommaTyopList
  { $1 True $3 $4 }

Assoc :: { Bool -> Maybe Integer -> [Name] -> Decl }
  : "infixl"
  { FixityDecl $1 FixityLeft }
  | "infixr"
  { FixityDecl $1 FixityRight }
  | "infix"
  { FixityDecl $1 FixityBoth }

Prec :: { Maybe Integer }
  :
  { Nothing }
  | int
  { let IntConst _ v _ = $1 in Just v }

CommaOpList :: { [Name] }
  : QVarSymName
  { [$1] }
  | CommaOpList "," QVarSymName
  { $1 ++ [$3] }

CommaTyopList :: { [Name] }
  : TyOp
  { [$1] }
  | CommaTyopList "," TyOp
  { $1 ++ [$3] }

-- Type Signatures

TypeSigDecl :: { [Decl] }
  : TypeSigNames "::" Type
  { map (\ n -> TypeSigDecl $2 n $3) $1 }
  | TypeSigNames "::" Predicate "=>" Type
  { map (\ n -> TypeSigDecl $2 n (WithPredicates [$3] $5)) $1 }

TypeSigNames :: { [Name] }
  : VarName
  { [$1] }
  | TypeSigNames "," VarName
  { $1 ++ [$3] }

Predicate :: { Predicate }
  : Type
  { Predicate Nothing $1 }
  | Type "=" Type
  { Predicate (Just $1) $3 }
  | Predicate "fails"
  { FailPredicate $1 }
  | "(" Type "=" Type ")"
  { Predicate (Just $2) $4 }
  | "(" Predicate "fails" ")"
  { FailPredicate $2 }

-- Type Declarations

TypeDecl :: { Decl }
  : "type" TypeLhs "=" Type
  { TypeDecl $1 $2 $4 }

TypeLhs :: { Type }
  : TypeParam ConSymName TypeParam
  { TypeApp (TypeRef $2) [$1, $3] }
  | PreTypeLhs
  { $1 }

PreTypeLhs :: { Type }
  : ConName
  { TypeRef $1 }
  | PreTypeLhs TypeParam
  { TypeApp $1 [$2] }
  | "(" TypeLhs ")"
  { $2 }

TypeParam :: { Type }
  : VarId
  { TypeRef $1 }
  | "(" TypeParam ")"
  { $2 }
  | "(" TypeParam "::" Kind ")"
  { TypeKind $2 $4 }

-- Struct Decl

StructDecl :: { Decl }
  : "struct" ConName "[" "]"
  { StructDecl $1 $2 Nothing [] [] }
  | "struct" ConName "[" "]" "deriving" DeriveList
  { StructDecl $1 $2 Nothing [] $6 }
  | "struct" ConName "[" StructRegions "]"
  { StructDecl $1 $2 Nothing $4 [] }
  | "struct" ConName "[" StructRegions "]" "deriving" DeriveList
  { StructDecl $1 $2 Nothing $4 $7 }
  | "struct" ConName "/" Type "[" "]"
  { StructDecl $1 $2 (Just $4) [] [] }
  | "struct" ConName "/" Type "[" "]" "deriving" DeriveList
  { StructDecl $1 $2 (Just $4) [] $8 }
  | "struct" ConName "/" Type "[" StructRegions "]"
  { StructDecl $1 $2 (Just $4) $6 [] }
  | "struct" ConName "/" Type "[" StructRegions "]" "deriving" DeriveList
  { StructDecl $1 $2 (Just $4) $6 $9 }

StructRegions :: { [StructField] }
  : StructRegion
  { $1 }
  | StructRegions "|" StructRegion
  { $1 ++ $3 }

StructRegion :: { [StructField] }
  : Type
  { [(Nothing, Nothing, $1)] }
  | FieldNames "::" Type
  { map (\ (a,b) -> (Just a, b, $3)) $1 }

FieldNames :: { [(Name, Maybe Expr)] }
  : FieldName
  { [$1] }
  | FieldNames "," FieldName
  { $1 ++ [$3] }

FieldName :: { (Name, Maybe Expr) }
  : VarId
  { ($1, Nothing) }
  | VarId "<-" InfExpr
  { ($1, Just $3) }

DeriveList :: { [Name] }
  : ConName
  { [$1] }
  | "(" ConNameList ")"
  { $2 }

ConNameList :: { [Name] }
  : ConName
  { [$1] }
  | ConNameList "," ConName
  { $1 ++ [$3] }

-- Bitdata Declarations
BitdataDecl :: { Decl }
  : "bitdata" ConName
  { BitdataDecl $1 $2 Nothing [] [] }
  | "bitdata" ConName "deriving" DeriveList
  { BitdataDecl $1 $2 Nothing [] $4 }
  | "bitdata" ConName BitdataCons
  { BitdataDecl $1 $2 Nothing $3 [] }
  | "bitdata" ConName BitdataCons "deriving" DeriveList
  { BitdataDecl $1 $2 Nothing $3 $5 }
  | "bitdata" ConName "/" Type
  { BitdataDecl $1 $2 (Just $4) [] [] }
  | "bitdata" ConName "/" Type "deriving" DeriveList
  { BitdataDecl $1 $2 (Just $4) [] $6 }
  | "bitdata" ConName "/" Type BitdataCons
  { BitdataDecl $1 $2 (Just $4) $5 [] }
  | "bitdata" ConName "/" Type BitdataCons "deriving" DeriveList
  { BitdataDecl $1 $2 (Just $4) $5 $7 }

BitdataCons :: { [(Name, [Either Expr (Name, Maybe Expr, Type)])] }
  : "=" BitdataCon
  { [$2] }
  | BitdataCons "|" BitdataCon
  { $1 ++ [$3] }

BitdataCon :: { (Name, [Either Expr (Name, Maybe Expr, Type)]) }
  : ConName "[" BitdataFields "]"
  { ($1, $3) }

BitdataFields :: { [Either Expr (Name, Maybe Expr, Type)] }
  : BitdataField
  { [$1] }
  | BitdataFields "|" BitdataField
  { $1 ++ [$3] }

BitdataField :: { Either Expr (Name, Maybe Expr, Type) }
  : VarName "::" Type
  { Right ($1, Nothing, $3) }
  | VarName "=" InfExpr "::" Type
  { Right ($1, Just $3, $5) }
  | AtomicExpr
  { Left $1 }

-- Area Declarations

AreaDecl :: { [Decl] }
  : "area" AreaVars "::" Type
  { map (\ (n,v) -> AreaDecl $1 n v $4) $2 }
  | "area" AreaVars "::" Type "where" DeclBlock
  { [LocalDecl $5 $6 (map (\ (n,v) -> AreaDecl $1 n v $4) $2)] }

AreaVars :: { [(Name, Maybe Expr)] }
  : AreaVar
  { [$1] }
  | AreaVars "," AreaVar
  { $1 ++ [$3] }

AreaVar :: { (Name, Maybe Expr) }
  : VarName
  { ($1, Nothing) }
  | VarName "<-" InfExpr
  { ($1, Just $3) }

-- Class Declarations

ClassDecl :: { Decl }
  : "class" ClassLhs
  { let (a,b) = $2 in ClassDecl $1 a b [] [] }
  | "class" ClassLhs "|" CommaConstList
  { let (a,b) = $2 in ClassDecl $1 a b $4 [] }
  | "class" ClassLhs "where" DeclBlock
  { let (a,b) = $2 in ClassDecl $1 a b [] $4 }
  | "class" ClassLhs "|" CommaConstList "where" DeclBlock
  { let (a,b) = $2 in ClassDecl $1 a b $4 $6 }

ClassLhs :: { (Type, Maybe Type) }
  : TypeLhs
  { ($1, Nothing) }
  | TypeLhs "=" TypeParam
  { ($1, Just $3) }

CommaConstList :: { [Constraint] }
  : Constraint
  { [$1] }
  | CommaConstList "," Constraint
  { $1 ++ [$3] }

Constraint :: { Constraint }
  : FunDep
  { $1 }
  | ConName TypeList
  { Superclass $1 $2 }

TypeList :: { [Type] }
  : VarId
  { [TypeRef $1] }
  | "(" Type ")"
  { [$2] }
  | TypeList VarId
  { $1 ++ [TypeRef $2] }
  | TypeList "(" Type ")"
  { $1 ++ [$3] }

FunDep :: { Constraint }
  : "->" ListVar
  { FunDep [] $2 }
  | ListVar "->" ListVar
  { FunDep $1 $3 }

ListVar :: { [Name] }
  : VarId
  { [$1] }
  | ListVar VarId
  { $1 ++ [$2] }

-- Instance Declarations

InstanceDecl :: { Decl }
  : "instance" Instance
  { InstanceDecl $1 [$2] }
  | InstanceDecl "else" Instance
  { let InstanceDecl src xs = $1
    in InstanceDecl src (xs ++ [$3]) }

Instance :: { Instance }
  : Predicate
  { Instance [$1] [] [] }
  | Predicate "if" Predicate
  { Instance [$1] [$3] [] }
  | Predicate "where" DeclBlock
  { Instance [$1] [] $3 }
  | Predicate "if" Predicate "where" DeclBlock
  { Instance [$1] [$3] $5 }

-- Data Declarations

DataDecl :: { Decl }
  : "data" TypeLhs
  { DataDecl $1 $2 [] [] [] }
  | "data" TypeLhs "=" DataCons
  { DataDecl $1 $2 $4 [] [] }
  | "data" TypeLhs "|" CommaConstList "=" DataCons
  { DataDecl $1 $2 $6 [] $4 }
  | "data" TypeLhs "deriving" DeriveList
  { DataDecl $1 $2 [] $4 [] }
  | "data" TypeLhs "=" DataCons "deriving" DeriveList
  { DataDecl $1 $2 $4 $6 [] }
  | "data" TypeLhs "|" CommaConstList "=" DataCons "deriving" DeriveList
  { DataDecl $1 $2 $6 $8 $4 }

DataCons :: { [Type] }
  : Type
  { [$1] }
  | DataCons "|" Type
  { $1 ++ [$3] }

-- Equations

Equation :: { [Decl] }
  : VarName "=" Expr
  { [EquationDecl $2 $1 [] Nothing $3] }
  | VarName GuardEqRhs
  { map (\f -> f $1 []) $2 }
  | VarName PatList "=" Expr
  { [EquationDecl $3 $1 $2 Nothing $4] }
  | VarName PatList GuardEqRhs
  { map (\f -> f $1 $2) $3 }
  | VarName "=" Expr "where" DeclBlock
  { [LocalDecl $4 $5 [EquationDecl $2 $1 [] Nothing $3]] }
  | VarName GuardEqRhs "where" DeclBlock
  { [LocalDecl $3 $4 (map (\f -> f $1 []) $2)] }
  | VarName PatList "=" Expr "where" DeclBlock
  { [LocalDecl $5 $6 [EquationDecl $3 $1 $2 Nothing $4]] }
  | VarName PatList GuardEqRhs "where" DeclBlock
  { [LocalDecl $4 $5 (map (\f -> f $1 $2) $3)] }

GuardEqRhs :: { [Name -> [Pattern] -> Decl] }
  : "|" Expr "=" Expr
  { [\ n p -> EquationDecl $1 n p (Just $2) $4] }
  | GuardEqRhs "|" Expr "=" Expr
  { $1 ++ [\ n p -> EquationDecl $2 n p (Just $3) $5] }

PatList :: { [Pattern] }
  : APat
  { [$1] }
  | PatList APat
  { $1 ++ [$2] }

-- Patterns
Pat :: { Pattern }
  : AppPat
  { $1 }
  | Pat VarSymId AppPat
  { case $1 of
      PatInfix f others -> PatInfix f (others ++ [($2, $3)])
      _                 -> PatInfix $1 [($2,$3)] }

AppPat :: { Pattern }
  : APat
  { $1 }
  | AppPat APat
  { PatApply $1 [$2] }

APat :: { Pattern }
  : VarId
  { PatRef $1 }
  | "_"
  { PatBlank }
  | VarId "@" APat
  { PatNamed $1 $3 }
  | ConName
  { PatRef $1 }
  | ConName "[" "]"
  { PatStruct $1 [] }
  | ConName "[" PatFields "]"
  { PatStruct $1 $3 }
  | "(" TuplePat ")"
  { PatTuple $2 }
  | "(" Pat "::" Type ")"
  { PatTyped $2 $4 }
  | "(" Pat ")"
  { $2 }
  | Literal
  { PatConst $1 }

TuplePat :: { [Pattern] }
  : Pat "," Pat
  { [$1, $3] }
  | TuplePat "," Pat
  { $1 ++ [$3] }

PatFields :: { [(Name, Maybe Pattern)] }
  : Id
  { [($1, Nothing)] }
  | Id "=" Pat
  { [($1, Just $3)] }
  | PatFields "," Id
  { $1 ++ [($3, Nothing)] }
  | PatFields "," Id "=" Pat
  { $1 ++ [($3, Just $5)] }

-- Expressions

Expr :: { Expr }
  : ApplicExpr
  { $1 }
  | LetExpr
  { $1 }
  | IfExpr
  { $1 }
  | CaseExpr
  { $1 }

ApplicExpr :: { Expr }
  : "\\" PatList "->" Expr
  { ExprLambda $1 $2 $4 }
  | "do" Block
  { ExprDo $1 $2 }
  | InfExpr
  { $1 }
  | InfExpr "::" AppliedType
  { ExprType $2 $1 $3 }

InfExpr :: { Expr }
  : AppExpr
  { $1 }
  | InfExpr Op AppExpr
  { case $1 of
      ExprInfix l others -> ExprInfix l (others ++ [($2, $3)])
      _                  -> ExprInfix $1 [($2, $3)] }

AppExpr :: { Expr }
  : AtomicExpr
  { $1 }
  | AppExpr AtomicExpr
  { ExprApply $1 [$2] }

LetExpr :: { Expr }
  : "let" DeclBlock "in" Expr
  { ExprLet $1 $2 $4 }

IfExpr :: { Expr }
  : "if" Expr "then" Expr "else" Expr
  { ExprIf $1 $2 $4 $6 }
  | "if" "<-" Expr "then" Expr "else" Expr
  { ExprIfM $1 $3 [StmtExpr $5] [StmtExpr $7] }
  | "if" "<-" Expr "then" Block "else" Block
  { ExprIfM $1 $3 $5 $7 }

CaseExpr :: { Expr }
  : "case" Expr "of" ExprAlts
  { ExprCase $1 $2 $4 }
  | "case" "<-" Expr "of" BlockAlts
  { ExprCaseM $1 $3 $5 }

AtomicExpr :: { Expr }
  : QVarName
  { ExprRef $1 }
  | QConName
  { ExprRef $1 }
  | Literal
  { ExprConst $1 }
  | AtomicExpr "." Id
  { ExprFldRef $2 $1 $3 }
  | "(" Expr ")"
  { $2 }
  | "(" InfExpr Op ")"
  { ExprApply (ExprRef $3) [$2] }
  | "(" Op Expr ")"
  { ExprApply (ExprRef $2) [$3] } -- FIXME
  | "(" TupleExprCommas ")"
  { ExprTuple $1 $2 }
  | AtomicExpr "[" FieldInits "]"
  { $3 $2 $1 }

FieldInits :: { Posn -> Expr -> Expr }
  : Fields
  { \ s x -> ExprUpdate s x $1 }
  | StructFldInit
  { \ s x -> ExprBuild s x $1 }

Fields :: { [(Name, Maybe Expr)] }
  : VarName
  { [($1, Nothing)] }
  | VarName "=" Expr
  { [($1, Just $3)] }
  | Fields "|" VarName
  { $1 ++ [($3, Nothing)] }
  | Fields "|" VarName "=" Expr
  { $1 ++ [($3, Just $5)] }

TupleExprCommas :: { [Expr] }
  : Expr "," Expr
  { [$1, $3] }
  | TupleExprCommas "," Expr
  { $1 ++ [$3] }

StructFldInit :: { [(Name, Expr)] }
  : VarName "<-" Expr
  { [($1, $3)] }
  | StructFldInit "|" VarName "<-" Expr
  { $1 ++ [($3, $5)] }

Literal :: { ConstVal }
  : int
  { translateConst $1 }
  | float
  { translateConst $1 }
  | vec
  { translateConst $1 }
  | "(" ")"
  { ConstUnit $1 }
  | "#." Id
  { ConstLabel $1 $2 }

-- case exp/stmt stuff

ExprAlts :: { [ExprCase] }
  : "{" SemiSepAltExprs "}"
  { $2 }

SemiSepAltExprs :: { [ExprCase] }
  : AltExpr
  { $1 }
  | SemiSepAltExprs ";" AltExpr
  { $1 ++ $3 }

AltExpr :: { [ExprCase] }
  : Pat ExprRhs1
  { map (\ f -> f $1) $2 }
  | Pat ExprRhs1 "where" DeclBlock
  { [EWhereCase $4 (map (\ f -> f $1) $2)] }

ExprRhs1 :: { [Pattern -> ExprCase] }
  : "->" Expr
  { [\ p -> ECase p Nothing $2] }
  | GuardListExpr
  { $1 }

GuardListExpr :: { [Pattern -> ExprCase] }
  : "|" Expr "->" Expr
  { [\ p -> ECase p (Just $2) $4]  }
  | GuardListExpr "|" Expr "->" Expr
  { $1 ++ [\ p -> ECase p (Just $3) $5] }

BlockAlts :: { [BlockCase] }
  : "{" SemiSepAltBlocks "}"
  { $2 }

SemiSepAltBlocks :: { [BlockCase] }
  : AltBlock
  { $1 }
  | SemiSepAltBlocks ";" AltBlock
  { $1 ++ $3 }

AltBlock :: { [BlockCase] }
  : Pat BlockRhs1
  { map (\ f -> f $1) $2 }
  | Pat BlockRhs1 "where" DeclBlock
  { [BWhereCase $4 (map (\ f -> f $1) $2)] }

BlockRhs1 :: { [Pattern -> BlockCase] }
  : "->" Block
  { [\ p -> BCase p Nothing $2] }
  | GuardListBlock
  { $1 }

GuardListBlock :: { [Pattern -> BlockCase] }
  : "|" Expr "->" Block
  { [\ p -> BCase p (Just $2) $4]  }
  | GuardListBlock "|" Expr "->" Block
  { $1 ++ [\ p -> BCase p (Just $3) $5] }

-- Statements

Block :: { [Statement] }
  : "{" Statements "}"
  { $2 }

Statements :: { [Statement] }
  : Statement ";" Statements
  { $1 : $3 }
  | VarId "<-" Statement ";" Statements
  { StmtBind $2 $1 $3 : $5 }
  | "let" DeclBlock ";" Statements
  { StmtLet $1 $2 [] : $4 }
  | Statement
  { [$1] }

Statement :: { Statement }
  : Expr
  { StmtExpr $1 }
  | "let" DeclBlock "in" Block
  { StmtLet $1 $2 $4 }
  | "if" Expr "then" Block
  { StmtIf $1 $2 $4 [] }
  | "if" Expr "then" Block "else" Block
  { StmtIf $1 $2 $4 $6 }
  | "case" Expr "of" BlockAlts
  { StmtCase $1 $2 $4 }
  -- The case<- item should be covered by Expr, above

-- Types

AtomicType :: { Type }
  : TyCon
  { TypeRef $1 }
  | VarId
  { TypeRef $1 }
  | "(" ")"
  { TypeUnit }
  | int
  { let IntConst _ v _ = $1 in TypeInt v }
  | "#." Id
  { TypeLabel $2 }
  | "(" Type ")"
  { $2 }
  | "(" Type "::" Kind ")"
  { TypeKind $2 $4 }
  | "(" TupleTypeCommas ")"
  { TypeTuple $2 }

AppliedType :: { Type }
  : AtomicType
  { $1 }
  | AppliedType AtomicType
  { TypeApp $1 [$2] }

Type :: { Type }
  : AppliedType
  { $1 }
  | Type TyOp AppliedType
  { case $1 of
      TypeInfix f others -> TypeInfix f (others ++ [($2, $3)])
      _                  -> TypeInfix $1 [($2, $3)] }

TupleTypeCommas :: { [Type] }
  : Type "," Type
  { [$1, $3] }
  | TupleTypeCommas "," Type
  { $1 ++ [$3] }

-- Kinds

Kind :: { Kind }
  : AtomicKind "->" Kind
  { KindFun $1 $3 }
  | AtomicKind
  { $1 }

AtomicKind :: { Kind }
  : "*"
  { KindStar }
  | "type"
  { KindType }
  | "nat"
  { KindNat }
  | "area"
  { KindArea }
  | "lab"
  { KindLabel }
  | "(" Kind ")"
  { $2 }

--

VarId :: { Name }
  : varid
  { startName $1 }
  | "as"
  { Name $1 [] "as" }
  | "hiding"
  { Name $1 [] "hiding" }
  | "lab"
  { Name $1 [] "lab" }
  | "module"
  { Name $1 [] "module" }
  | "qualified"
  { Name $1 [] "qualified" }

QVarId :: { Name }
  : VarId
  { $1 }
  | qvarid
  { startName $1 }

VarSymId :: { Name }
  : varsymid
  { startName $1 }
  | "*"
  { Name $1 [] "*" }
  | "/"
  { Name $1 [] "/" }

QVarSymId :: { Name }
  : VarSymId
  { $1 }
  | qvarsymid
  { startName $1 }

ConId :: { Name }
  : conid
  { startName $1 }

ConSymId :: { Name }
  : consymid
  { startName $1 }
  | ":#"
  { Name $1 [] ":#" }

VarName :: { Name }
  : VarId
  { $1 }
  | "(" VarSymId ")"
  { $2 }

QVarName :: { Name }
  : VarName
  { $1 }
  | qvarid
  { startName $1 }

ConName :: { Name }
  : ConId
  { $1 }
  | "(" ConSymId ")"
  { $2 }

QConName :: { Name }
  : ConName
  { $1 }
  | qconid
  { startName $1 }

VarSymName :: { Name }
  : VarSymId
  { $1 }
  | "`" VarName "`"
  { $2 }

QVarSymName :: { Name }
  : QVarSymId
  { $1 }
  | "`" QVarName "`"
  { $2 }

ConSymName :: { Name }
  : ConSymId
  { $1 }
  | "`" ConName "`"
  { $2 }

QConSymName :: { Name }
  : ConSymName
  { $1 }
  | qconsymid
  { startName $1 }

Id :: { Name }
  : VarId
  { $1 }
  | ConId
  { $1 }

Op :: { Name }
  : VarSymName
  { $1 }
  | ConSymName
  { $1 }

TyCon :: { Name }
  : QConName
  { $1 }
  | "(" QVarSymId ")"
  { $2 }
  | "(" "->" ")"
  { Name $2 [] "->" }

TyConOp :: { Name }
  : QConSymName
  { $1 }
  | QVarSymId
  { $1 }
  | "->"
  { Name $1 [] "->" }

TyOp :: { Name }
  : TyConOp
  { $1 }
  | "`" QVarId "`"
  { $2 }

ModName :: { Name }
  : ConId
  { $1 }
  | qconid
  { startName $1 }


{

happyError :: [Lexeme] -> a
happyError []    = error "Parse error at end of file!"
happyError (x:_) =
  error (show (getPosn x) ++ ": Parse error around " ++ show (getToken x))

parse :: Maybe FilePath -> ByteString -> HabitModule
parse source bytes = parseHabit (scanWithLayout source bytes)

}


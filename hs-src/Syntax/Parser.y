--  vim:set filetype=haskell:
{
module Syntax.Parser(parseHabit) where

import Data.ByteString.Lazy(ByteString,unpack)
import Data.Char(chr)
import Syntax.Tokens

}

%name parseHabit
%monad { Alex }
%lexer { tokenize } { EOF }

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

  "("           { ReservedSym $$ "(" _ }
  ")"           { ReservedSym $$ ")" _ }
  "|"           { ReservedSym $$ "|" _ }
  "="           { ReservedSym $$ "=" _ }
  ","           { ReservedSym $$ "," _ }
  "`"           { ReservedSym $$ "`" _ }
  "{"           { ReservedSym $$ "{" _ }
  ";"           { ReservedSym $$ ";" _ }
  "}"           { ReservedSym $$ "}" _ }
  "["           { ReservedSym $$ "[" _ }
  "]"           { ReservedSym $$ "]" _ }
  "\\"          { ReservedSym $$ "\\" _ }
  "<-"          { ReservedSym $$ "<-" _ }
  "->"          { ReservedSym $$ "->" _ }
  "=>"          { ReservedSym $$ "=>" _ }
  "::"          { ReservedSym $$ "::" _ }
  "#."          { ReservedSym $$ "#." _ }
  "@"           { ReservedSym $$ "@" _ }
  "_"           { ReservedSym $$ "_" _ }
  "."           { ReservedSym $$ "." _ }
  "*"           { ReservedSym $$ "*" _ }
  "/"           { ReservedSym $$ "/" _ }
  ":#"          { ReservedSym $$ ":#" _ }

  varid         { VarId _ _ }
  conid         { ConId _ _ }
  varsymid      { VarSymId _ _ }
  consymid      { ConSymId _ _ }
  int           { IntConst _ _ _ }
  vec           { VecConst _ _ _ _ }
  float         { FloatConst _ _ }

%%

-- These are defined in the Nov10 Habit report, page 28
Habit :: { HabitModule }
  : "{" Prog "}"
  { HabitModule defaultMod $2 }
  | "module" ModName "where" "{" Prog "}"
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
  : VarSymId
  { [$1] }
  | CommaOpList "," VarSymId
  { $1 ++ [$3] }

CommaTyopList :: { [Name] }
  : ConSymId
  { [$1] }
  | CommaTyopList "," ConSymId
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
  | Type "fails"
  { FailPredicate (Predicate Nothing $1) }
  | Type "=" Type "fails"
  { FailPredicate (Predicate (Just $1) $3) }

-- Type Declarations

TypeDecl :: { Decl }
  : "type" TypeLhs "=" Type
  { TypeDecl $1 $2 $4 }

TypeLhs :: { Type }
  : TypeParam ConSymName TypeParam
  { TypeApp (TypeRef (startName $2)) [$1, $3] }
  | PreTypeLhs
  { $1 }

PreTypeLhs :: { Type }
  : ConName
  { TypeRef (startName $1) }
  | PreTypeLhs TypeParam
  { TypeApp $1 [$2] }
  | "(" TypeLhs ")"
  { $2 }

TypeParam :: { Type }
  : VarId
  { TypeRef (startName $1) }
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
  | "(" ModNameList ")"
  { $2 }

ModNameList :: { [Name] }
  : ConName
  { [$1] }
  | ModNameList "," ConName
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
  { [LocalDecl $6 (map (\ (n,v) -> AreaDecl $1 n v $4) $2)] }

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
  | InstanceDecl "else" Predicate
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
  { [LocalDecl $5 [EquationDecl $2 $1 [] Nothing $3]] }
  | VarName GuardEqRhs "where" DeclBlock
  { [LocalDecl $4 (map (\f -> f $1 []) $2)] }
  | VarName PatList "=" Expr "where" DeclBlock
  { [LocalDecl $6 [EquationDecl $3 $1 $2 Nothing $4]] }
  | VarName PatList GuardEqRhs "where" DeclBlock
  { [LocalDecl $5 (map (\f -> f $1 $2) $3)] }

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
  { PatApply (PatRef $2) [$1, $3] }

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
  { ExprLambda $2 $4 }
  | "do" Block
  { ExprDo $2 }
  | InfExpr
  { $1 }
  | InfExpr "::" AppliedType
  { ExprType $1 $3 }

InfExpr :: { Expr }
  : AppExpr
  { $1 }
  | InfExpr Op AppExpr
  { ExprApply (ExprRef $2) [$1, $2] }

AppExpr :: { Expr }
  : AtomicExpr
  { $1 }
  | AppExpr AtomicExpr
  { ExprApply $1 [$2] }

LetExpr :: { Expr }
  : "let" DeclBlock "in" Expr
  { ExprLet $2 $4 }

IfExpr :: { Expr }
  : "if" Expr "then" Expr "else" Expr
  { ExprIf $2 $4 $6 }
  | "if" "<-" Expr "then" Block "else" Block
  { ExprIfM $3 $5 $7 }

CaseExpr :: { Expr }
  : "case" Expr "of" ExprAlts
  { ExprCase $2 $4 }
  | "case" "<-" Expr "of" BlockAlts
  { ExprCaseM $3 $5 }

AtomicExpr :: { Expr }
  : VarName
  { ExprRef $1 }
  | ConName
  { ExprRef $1 }
  | Literal
  { ExprConst $1 }
  | AtomicExpr "." Id
  { ExprFldRef $1 $3 }
  | "(" Expr ")"
  { $2 }
  | "(" InfExpr Op ")"
  { ExprApply (ExprRef $3) [$2] }
  | "(" Op Expr ")"
  { ExprApply (ExprRef $2) [$3] } -- FIXME
  | "(" TupleExprCommas ")"
  { ExprTuple $2 }
  | AtomicExpr "[" FieldInits "]"
  { $3 $1 }

FieldInits :: { Expr -> Expr }
  : Fields
  { \ x -> ExprUpdate x $1 }
  | StructFldInit
  { \ x -> ExprBuild x $1 }

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
  { StmtBind $1 $3 : $5 }
  | "let" DeclBlock ";" Statements
  { StmtLet $2 [] : $4 }
  | Statement
  { [$1] }

Statement :: { Statement }
  : Expr
  { StmtExpr $1 }
  | "let" DeclBlock "in" Block
  { StmtLet $2 $4 }
  | "if" Expr "then" Block
  { StmtIf $2 $4 [] }
  | "if" Expr "then" Block "else" Block
  { StmtIf $2 $4 $6 }
  | "case" Expr "of" BlockAlts
  { StmtCase $2 $4 }
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
  { TypeApp (TypeRef $2) [$1, $3] }

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
  { Name $1 False [] "as" }
  | "hiding"
  { Name $1 False [] "hiding" }
  | "lab"
  { Name $1 False [] "lab" }
  | "module"
  { Name $1 False [] "module" }
  | "qualified"
  { Name $1 False [] "qualified" }

VarSymId :: { Name }
  : varsymid
  { startName $1 }
  | "*"
  { Name $1 False [] "*" }
  | "/"
  { Name $1 False [] "/" }

ConId :: { Name }
  : conid
  { startName $1 }

ConSymId :: { Name }
  : consymid
  { startName $1 }
  | ":#"
  { Name $1 False [] ":#" }

VarName :: { Name }
  : VarId
  { $1 }
  | "(" VarSymId ")"
  { $2 }
--  | ModName "." VarId
--  { addName' $1 $3 }
--  | ModName "." "(" VarSymId ")"
--  { addName' $1 $4 }
--  | ModName "." "(" ConSymId ")"
--  { addName' $1 $4 }

ConName :: { Name }
  : ConId
  { $1 }
  | "(" ConSymId ")"
  { $2 }
--  : ModName
--  { $1 }
--  | ModName "." "(" ConSymId ")"
--  { addName' $1 $4 }

VarSymName :: { Name }
  : VarSymId
  { $1 }
  | "`" VarName "`"
  { $2 }

ConSymName :: { Name }
  : ConSymId
  { $1 }
  | "`" ConName "`"
  { $2 }

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
  : ConName
  { $1 }
  | "(" VarSymId ")"
  { $2 }
  | "(" "->" ")"
  { startName $2 }

TyConOp :: { Name }
  : ConSymName
  { $1 }
  | VarSymId
  { $1 }
  | "->"
  { startName $1 }

TyOp :: { Name }
  : TyConOp
  { $1 }
  | "`" VarId "`"
  { $2 }

ModName :: { Name }
  : ConId
  { $1 }
  | ModName "." ConId
  { addName' $1 $3 }


{

data HabitModule = HabitModule Name [Decl]
 deriving (Show)

data Decl        = ImportDecl AlexPosn Bool Name (Maybe Name) ImportMods
                 | FixityDecl AlexPosn FixityType Bool (Maybe Integer) [Name]
                 | TypeSigDecl AlexPosn Name Type
                 | TypeDecl AlexPosn Type Type
                 | StructDecl AlexPosn Name (Maybe Type) [StructField] [Name]
                 | BitdataDecl AlexPosn Name (Maybe Type) [BitdataField] [Name]
                 | AreaDecl AlexPosn Name (Maybe Expr) Type
                 | ClassDecl AlexPosn Type (Maybe Type) [Constraint] [Decl]
                 | InstanceDecl AlexPosn [Instance]
                 | DataDecl AlexPosn Type [Type] [Name] [Constraint]
                 | EquationDecl AlexPosn Name [Pattern] (Maybe Expr) Expr
                 | LocalDecl [Decl] [Decl]
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

data Expr = ExprConst ConstVal
          | ExprRef Name
          | ExprLet [Decl] Expr
          | ExprIf Expr Expr Expr
          | ExprIfM Expr [Statement] [Statement]
          | ExprCase Expr [ExprCase]
          | ExprCaseM Expr [BlockCase]
          | ExprDo [Statement]
          | ExprLambda [Pattern] Expr
          | ExprType Expr Type
          | ExprInfix Expr Name Expr
          | ExprApply Expr Expr
          | ExprFldRef Expr Name
          | ExprUpdate Expr [(Name, Maybe Expr)]
          | ExprBuild Expr [(Name, Expr)]
  deriving (Show)

data Statement = StmtBind Name Statement
               | StmtLet [Decl] [Statement]
               | StmtExpr Expr
               | StmtIf Expr [Statement] [Statement]
               | StmtCase Expr [BlockCase]
  deriving (Show)

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
  deriving (Show)

data Type = WithPredicates [Predicate] Type
          | TypeRef Name
          | TypeUnit
          | TypeInt Integer
          | TypeLabel Name
          | TypeKind Type Kind
          | TypeApp Type [Type]
          | TypeTuple [Type]
  deriving (Show)

data Kind = KindStar | KindType | KindNat | KindArea | KindLabel
          | KindFun Kind Kind
  deriving (Show)

data ConstVal = ConstInt AlexPosn Integer Int
              | ConstVec AlexPosn Integer Int Int
              | ConstFloat AlexPosn Float
              | ConstDouble AlexPosn Double
              | ConstUnit AlexPosn
              | ConstLabel AlexPosn Name
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

data Name       = Name AlexPosn Bool [String] String
 deriving (Show)

defaultMod :: Name
defaultMod = Name nopos True [] "Main"

startName :: Lexeme -> Name
startName (ReservedId p x)    = Name p False [] x
startName (ReservedSym p x m) = Name p m [] x
startName (VarId p bs)        = Name p False [] (fromBS bs)
startName (ConId p bs)        = Name p False [] (fromBS bs)
startName (VarSymId p bs)     = Name p False [] (fromBS bs)
startName (ConSymId p bs)     = Name p False [] (fromBS bs)
startName x                   = error ("Bad token for startName: " ++ show x)

addName :: Name -> Lexeme -> Name
addName (Name p m ls x) t = Name p m (ls ++ [x]) y
 where Name _ _ _ y = startName t

addName' :: Name -> Name -> Name
addName' (Name p1 m1 ls1 x1) (Name _ m2 ls2 x2) =
  Name p1 (m1 && m2) (ls1 ++ [x1] ++ ls2) x2

happyError :: Alex a
happyError = fail "Parse Failed"

fromBS :: ByteString -> String
fromBS = map (chr . fromIntegral) . unpack

}


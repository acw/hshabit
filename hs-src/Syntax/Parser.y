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
  "extends"     { ReservedId $$ "else" }
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
  | Decl
  { $1 }
  | Prog ";" Decl
  { $1 ++ $3 }

-- Declarations

Decl :: { [Decl] }
  : ImportDecl
  { [$1] }
  | FixityDecl
  { [$1] }
  | TypeSigDecl
  { $1 }
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
  | Equation
  { $1 }

DeclBlock :: { [Decl] }
  : "{" "}"
  { [] }
  | "{" Decls "}"
  { $2 }

Decls :: { [Decl] }
  : Decl
  { $1 }
  | Decls ";" Decl
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
  : VarName
  { [$1] }
  | NameList "," VarName
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
  : Oper
  { [$1] }
  | CommaOpList "," Oper
  { $1 ++ [$3] }

CommaTyopList :: { [Name] }
  : TyOper
  { [$1] }
  | CommaTyopList "," TyOper
  { $1 ++ [$3] }

Oper :: { Name }
  : VarSymId
  { $1 }
  | "`" VarId "`"
  { $2 }
  | "`" ModName "." VarId "`"
  { addName' $2 $4 }
  | consymid
  { startName $1 }
  | "`" ModName "`"
  { $2 }

TyOper :: { Name }
  : "`" VarId "`"
  { $2 }
  | "`" ModName VarId "`"
  { addName' $2 $3 }
  | consymid
  { startName $1 }
  | "`" ModName "`"
  { $2 }
  | VarSymId
  { $1 }
  | "->"
  { Name $1 False [] "->" }

-- Type Signatures

TypeSigDecl :: { [Decl] }
  : TypeSigNames "::" Type
  { map (\ n -> TypeSigDecl $2 n $3) $1 }
  | TypeSigNames "::" "(" Predicate ")" "=>" Type
  { map (\ n -> TypeSigDecl $2 n (WithPredicates $4 $7)) $1 }
  | TypeSigNames "::" Predicate "=>" Type
  { map (\ n -> TypeSigDecl $2 n (WithPredicates $3 $5)) $1 }

TypeSigNames :: { [Name] }
  : TypeSigName
  { [$1] }
  | TypeSigNames "," TypeSigName
  { $1 ++ [$3] }

TypeSigName :: { Name }
  : VarId
  { $1 }
  | "(" VarSymId ")"
  { $2 }

Predicate :: { [Predicate] }
  : Type
  { buildPredicate $1 Nothing False }
  | Type "=" Type
  { buildPredicate $1 (Just $3) False }
  | Type "fails"
  { buildPredicate $1 Nothing True }
  | Type "=" Type "fails"
  { buildPredicate $1 (Just $3) True }

-- Type Declarations

TypeDecl :: { Decl }
  : "type" TypeLhs "=" Type
  { TypeDecl $1 $2 $4 }

TypeLhs :: { Type }
  : TypeParam consymid TypeParam
  { TypeApp (TypeRef (startName $2)) [$1, $3] }
  | TypeParam "`" conid "`" TypeParam
  { TypeApp (TypeRef (startName $3)) [$1, $5] }
  | PreTypeLhs
  { $1 }

PreTypeLhs :: { Type }
  : conid
  { TypeRef (startName $1) }
  | PreTypeLhs TypeParam
  { TypeApp $1 [$2] }
  | "(" TypeLhs ")"
  { $2 }

TypeParam :: { Type }
  : varid
  { TypeRef (startName $1) }
  | "(" TypeParam ")"
  { $2 }
  | "(" TypeParam "::" Kind ")"
  { TypeKind $2 $4 }

-- Struct Decl

StructDecl :: { Decl }
  : "struct" conid "[" "]"
  { StructDecl $1 (startName $2) Nothing [] [] }
  | "struct" conid "[" "]" "deriving" DeriveList
  { StructDecl $1 (startName $2) Nothing [] $6 }
  | "struct" conid "[" StructRegions "]"
  { StructDecl $1 (startName $2) Nothing $4 [] }
  | "struct" conid "[" StructRegions "]" "deriving" DeriveList
  { StructDecl $1 (startName $2) Nothing $4 $7 }
  | "struct" conid "/" Type "[" "]"
  { StructDecl $1 (startName $2) (Just $4) [] [] }
  | "struct" conid "/" Type "[" "]" "deriving" DeriveList
  { StructDecl $1 (startName $2) (Just $4) [] $8 }
  | "struct" conid "/" Type "[" StructRegions "]"
  { StructDecl $1 (startName $2) (Just $4) $6 [] }
  | "struct" conid "/" Type "[" StructRegions "]" "deriving" DeriveList
  { StructDecl $1 (startName $2) (Just $4) $6 $9 }

StructRegions :: { [StructField] }
  : StructRegion
  { $1 }
  | StructRegions "|" StructRegion
  { $1 ++ $3 }

StructRegion :: { [StructField] }
  : UnkindedType
  { [(Nothing, Nothing, $1)] }
  | FieldNames "::" Type
  { map (\ (a,b) -> (Just a, b, $3)) $1 }

FieldNames :: { [(Name, Maybe Expr)] }
  : FieldName
  { [$1] }
  | FieldNames "," FieldName
  { $1 ++ [$3] }

FieldName :: { (Name, Maybe Expr) }
  : varid
  { (startName $1, Nothing) }
  | varid "<-" Expr
  { (startName $1, Just $3) }

DeriveList :: { [Name] }
  : ModName
  { [$1] }
  | "(" ModNameList ")"
  { $2 }

ModNameList :: { [Name] }
  : ModName
  { [$1] }
  | ModNameList "," ModName
  { $1 ++ [$3] }

-- Bitdata Declarations
BitdataDecl :: { Decl }
  : "bitdata" conid
  { BitdataDecl $1 (startName $2) Nothing [] [] }
  | "bitdata" conid "deriving" DeriveList
  { BitdataDecl $1 (startName $2) Nothing [] $4 }
  | "bitdata" conid BitdataCons
  { BitdataDecl $1 (startName $2) Nothing $3 [] }
  | "bitdata" conid BitdataCons "deriving" DeriveList
  { BitdataDecl $1 (startName $2) Nothing $3 $5 }
  | "bitdata" conid "/" Type
  { BitdataDecl $1 (startName $2) (Just $4) [] [] }
  | "bitdata" conid "/" Type "deriving" DeriveList
  { BitdataDecl $1 (startName $2) (Just $4) [] $6 }
  | "bitdata" conid "/" Type BitdataCons
  { BitdataDecl $1 (startName $2) (Just $4) $5 [] }
  | "bitdata" conid "/" Type BitdataCons "deriving" DeriveList
  { BitdataDecl $1 (startName $2) (Just $4) $5 $7 }

BitdataCons :: { [(Name, [Either Expr (Name, Maybe Expr, Type)])] }
  : "=" BitdataCon
  { [$2] }
  | BitdataCons "|" BitdataCon
  { $1 ++ [$3] }

BitdataCon :: { (Name, [Either Expr (Name, Maybe Expr, Type)]) }
  : conid "[" BitdataFields "]"
  { (startName $1, $3) }

BitdataFields :: { [Either Expr (Name, Maybe Expr, Type)] }
  : BitdataField
  { [$1] }
  | BitdataFields "|" BitdataField
  { $1 ++ [$3] }

BitdataField :: { Either Expr (Name, Maybe Expr, Type) }
  : varid "::" Type
  { Right (startName $1, Nothing, $3) }
  | varid "=" Expr "::" Type
  { Right (startName $1, Just $3, $5) }
  | Expr
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
  : varid
  { (startName $1, Nothing) }
  | varid "<-" Expr
  { (startName $1, Just $3) }
  | "(" varsymid ")"
  { (startName $2, Nothing) }
  | "(" varsymid ")" "<-" Expr
  { (startName $2, Just $5) }

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
  : "instance" Instances
  { InstanceDecl $1 $2 }

Instances :: { [Instance] }
  : Instance
  { [$1] }
  | Instances "else" Instance
  { $1 ++ [$3] }

Instance :: { Instance }
  : Predicate
  { Instance $1 [] [] }
  | Predicate "if" Predicate
  { Instance $1 $3 [] }
  | Predicate "if" "(" CommaPreds ")"
  { Instance $1 $4 [] }
  | Predicate "where" DeclBlock
  { Instance $1 [] $3 }
  | Predicate "if" Predicate "where" DeclBlock
  { Instance $1 $3 $5 }
  | Predicate "if" "(" CommaPreds ")" "where" DeclBlock
  { Instance $1 $4 $7 }

CommaPreds :: { [Predicate] }
  : Predicate
  { $1 }
  | CommaPreds "," Predicate
  { $1 ++ $3 }

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
  : VarId "=" Expr
  { [EquationDecl $2 $1 [] Nothing $3] }
  | VarId GuardEqRhs
  { map (\f -> f $1 []) $2 }
  | VarId PatList "=" Expr
  { [EquationDecl $3 $1 $2 Nothing $4] }
  | VarId PatList GuardEqRhs
  { map (\f -> f $1 $2) $3 }
  | VarId "=" Expr "where" DeclBlock
  { [LocalDecl $5 [EquationDecl $2 $1 [] Nothing $3]] }
  | VarId GuardEqRhs "where" DeclBlock
  { [LocalDecl $4 (map (\f -> f $1 []) $2)] }
  | VarId PatList "=" Expr "where" DeclBlock
  { [LocalDecl $6 [EquationDecl $3 $1 $2 Nothing $4]] }
  | VarId PatList GuardEqRhs "where" DeclBlock
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
  : AtomicExpr
  { $1 }

AtomicExpr :: { Expr }
  : Literal
  { ExprConst $1 }
  | VarName
  { ExprRef $1 }

Literal :: { ConstVal }
  : int
  { translateConst $1 }
  | float
  { translateConst $1 }
  | vec
  { translateConst $1 }

-- Types

Type :: { Type }
  : TupleType "::" Kind
  { TypeKind $1 $3 }
  | TupleType
  { $1 }

UnkindedType :: { Type }
  : TupleType
  { $1 }

TupleType :: { Type }
  : "(" TupleTypeCommas ")"
  { TypeTuple $2 }
  | InfixType
  { $1 }

TupleTypeCommas :: { [Type] }
  : TupleType "," TupleType
  { [$1, $3] }
  | TupleTypeCommas "," TupleType
  { $1 ++ [$3] }

InfixType :: { Type }
  : InfixType TyOp AppliedType
  { TypeApp (TypeRef $2) [$1, $3] }
  | AppliedType
  { $1 }

AppliedType :: { Type }
  : AppliedType AtomicType
  { TypeApp $1 [$2] }
  | AtomicType
  { $1 }

AtomicType :: { Type }
  : ConName
  { TypeRef $1 }
  | VarId
  { TypeRef $1 }
  | "(" ")"
  { TypeUnit }
  | int
  { let IntConst _ v _ = $1 in TypeInt v }
  | "#." Id
  { TypeLabel $2 }
  | "(" TyOp ")"
  { TypeRef $2 }
  | "(" Type ")"
  { $2 }

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

VarSymId :: { Name }
  : varsymid
  { startName $1 }
  | "*"
  { Name $1 False [] "*" }
  | "/"
  { Name $1 False [] "/" }
  | ":#"
  { Name $1 False [] ":#" }

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

ModName :: { Name }
  : conid
  { startName $1 }
  | ModName "." conid
  { addName $1 $3 }

VarName :: { Name }
  : VarId
  { $1 }
  | "(" VarSymId ")"
  { $2 }
  | "(" consymid ")"
  { startName $2 }
  | ModName "." VarId
  { addName' $1 $3 }
  | ModName
  { $1 }
  | ModName "." "(" VarSymId ")"
  { addName' $1 $4 }
  | ModName "." "(" consymid ")"
  { addName $1 $4 }

ConName :: { Name }
  : ModName
  { $1 }
  | ModName "." "(" consymid ")"
  { addName $1 $4 }

TyOp :: { Name }
  : consymid
  { startName $1 }
  | varsymid
  { startName $1 }
  | "`" conid "`"
  { startName $2 }
  | ModName "." consymid
  { addName $1 $3 }
  | ModName "." varsymid
  { addName $1 $3 }
  | ModName "." "`" conid "`"
  { addName $1 $4 }
  | "->"
  { Name $1 False [] "->" }

Id :: { Name }
  : varid
  { startName $1 }
  | conid
  { startName $1 }


{-

OLDRULES

Decl        | Equation                    { 1 }

TopDecl     : Decl                        { 1 }

-- These are defined in the Nov10 Habit report, page 29
Equation    : EqLhs EqRhs                 { 1 }

EqLhs       : Var OptPatList              { 1 }

OptPatList  :                             { 1 }
            | OptPatList APat             { 1 }

EqRhs       : EqRhs1 OptWhere             { 1 }

EqRhs1      : "=" Expr                    { 1 }
            | GuardEqRhs                  { 1 }

GuardEqRhs  : "|" Expr "=" Expr             { 1 }
            | GuardEqRhs "|" Expr "=" Expr  { 1 }

-- These are defined in the Nov10 Habit report, page 30
CommaOpList : Op                                { 1 }
            | CommaOpList "," Op                { 1 }

CommaTyopList : Tyop                            { 1 }
              | CommaTyopList "," Tyop          { 1 }

-- These are defined in the Nov10 Habit report, page 23. I've
-- switched the Stmts to be left-recursive instead of right, which
-- will admit incorrect programs. But those are easily checked for
-- later.
Block       : "{" Stmts "}"               { 1 }

Stmts       : Stmt                        { 1 }
--            | Stmts ";" "let" DeclBlock   { 1 }
--            | Stmts ";" OptAssign Stmt    { 1 }

OptAssign   :                             { 1 }
            | Var "<-"                    { 1 }

-- These are defined in the Nov10 Habit report, page 22
Stmt        : Applic                      { 1 }
            | LetStmt                     { 1 }
            | IfStmt                      { 1 }
            | CaseStmt                    { 1 }


-- These are defined in the Nov10 Habit report, page 22
Expr        : Applic                      { 1 }
            | LetExpr                     { 1 }
            | IfExpr                      { 1 }
            | CaseExpr                    { 1 }

-- These are defined in the Nov10 Habit report, page 22
Applic      : "\\" APatList "->" Expr     { 1 }
            | "do" Block                  { 1 }
            | InfExpr OptType             { 1 }

APatList    : APat                        { 1 }
            | APatList APat               { 1 }

OptType     :                             { 1 }
            | "::" Type                   { 1 }

InfExpr     : AppExpr InfixOpList         { 1 }

InfixOpList :                             { 1 }
            | InfixOpList Op AppExpr      { 1 }

AppExpr     : AExpr                       { 1 }
            | AppExpr AExpr               { 1 }

AExpr       : Var                         { 1 }
            | Con                         { 1 }
            | Literal                     { 1 }
            | AExpr "." Id                { 1 }
            | AExpr "[" Fields "]"        { 1 }
            | "(" ")"                     { 1 }
            | "(" Expr ")"                { 1 }
            | "(" AExpr Op ")"            { 1 }
            | "(" Op AExpr ")"            { 1 }
            | "(" TupleExprs ")"          { 1 }
            | conid "[" StructFldInit "]" { 1 }

TupleExprs  : Expr "," Expr               { 1 }
            | TupleExprs "," Expr         { 1 }

StructFldInit : Id "<-" Expr                    { 1 }
              | StructFldInit "|" Id "<-" Expr  { 1 }

Fields        : Id OptFieldInit              { 1 }
              | Fields "|" Id OptFieldInit   { 1 }

OptFieldInit  :                           { 1 }
              | "=" Expr                  { 1 }

Literal     : int                         { 1 }
            | vec                         { 1 }
            | float                       { 1 }
            | "#." Id                     { 1 }

-- These are defined in the Nov10 Habit report, page 24
LetExpr     : "let" DeclBlock "in" Expr   { 1 }

LetStmt     : "let" DeclBlock "in" Block  { 1 }

DeclBlock   : "{" OptDecls "}"            { 1 }

OptDecls    :                             { 1 }
            | SemiSepDecls                { 1 }

SemiSepDecls : Decl                       { 1 }
             | SemiSepDecls ";" Decl      { 1 }

-- These are defined in the Nov10 Habit report, page 25
IfExpr      : "if" Expr "then" Expr "else" Expr     { 1 }
            | IfFrom                                { 1 }

IfStmt      : "if" Expr "then" Block OptElseBlock   { 1 }
            | IfFrom                                { 1 }

IfFrom      : "if" "<-" Stmt "then" Block OptElseBlock { 1 }

OptElseBlock :                              { 1 }
             | "else" Block                 { 1 }

CaseExpr    : "case" Expr "of" ExprAlts     { 1 }
            | CaseFrom                      { 1 }

CaseStmt    : "case" Expr "of" BlockAlts    { 1 }
            | CaseFrom                      { 1 }

CaseFrom    : "case" "<-" Stmt "of" BlockAlts { 1 }

ExprAlts    : "{" SemiSepAltExprs "}"       { 1 }

BlockAlts   : "{" SemiSepAltBlocks "}"      { 1 }

SemiSepAltExprs : AltExpr                           { 1 }
                | SemiSepAltExprs ";" AltExpr       { 1 }

SemiSepAltBlocks : AltBlock                        { 1 }
                 | SemiSepAltBlocks ";" AltBlock   { 1 }

AltExpr : Pat ExprRhs1 OptWhere                     { 1 }

ExprRhs1 : "->" Expr                                { 1 }
         | GuardListExpr                            { 1 }

OptWhere : "where" DeclBlock                        { 1 }

GuardListExpr : "|" Expr "->" Expr                  { 1 }
              | GuardListExpr "|" Expr "->" Expr    { 1 }

AltBlock : Pat BlockRhs1 OptWhere                   { 1 }

BlockRhs1 : "->" Block                              { 1 }
          | GuardListBlock                          { 1 }

GuardListBlock : "|" Expr "->" Block                { 1 }
               | GuardListBlock "|" Expr "->" Block { 1 }

-- These are defined in the Nov10 Habit report, page 26
Pat         : AppPat                      { 1 }
            | Pat Op AppPat               { 1 }

AppPat      : APat                        { 1 }
            | AppPat APat                 { 1 }

APat        : Var                         { 1 }
            | "_"                         { 1 }
            | Var "@" APat                { 1 }
            | Con                         { 1 }
            | Con "[" PatFields "]"       { 1 }
            | "(" TuplePat ")"            { 1 }
            | "(" Pat "::" Type ")"       { 1 }
            | "(" Pat ")"                 { 1 }
            | Literal                     { 1 }

TuplePat    : Pat "," Pat                 { 1 }
            | TuplePat "," Pat            { 1 }

PatFields   :                             { 1 }
            | PatFieldsComma              { 1 }

PatFieldsComma
            : Id OptEqPat                 { 1 }
            | PatFieldsComma Id OptEqPat  { 1 }

OptEqPat    :                             { 1 }
            | "=" Pat                     { 1 }



-}

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
  deriving (Show)

translateConst :: Lexeme -> ConstVal
translateConst (IntConst a b c) = ConstInt a b c
translateConst (VecConst a b c d) = ConstVec a b c d
translateConst (FloatConst a (FVal f)) = ConstFloat a f
translateConst (FloatConst a (DVal d)) = ConstDouble a d
translateConst _ = error "Incorrect lexeme to translateConst"

data Predicate = Predicate Type (Maybe Type) Bool
  deriving (Show)

buildPredicate :: Type -> (Maybe Type) -> Bool -> [Predicate]
buildPredicate (TypeTuple ps) Nothing False =
  concatMap (\ x -> buildPredicate x Nothing False) ps
buildPredicate (TypeTuple _) _ _ =
  error "Unexpected tuple in predicate position."
buildPredicate x@(TypeApp _ _) y z = [Predicate x y z]
buildPredicate _ _ _ =
  error "Unexpected type in predicate position."

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


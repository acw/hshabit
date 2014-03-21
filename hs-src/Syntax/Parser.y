{ module Syntax.Parser(parseHabit) where

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
  | "`" OptModName VarId "`"
  { maybeAddName' $2 $3 }
  | consymid
  { startName $1 }
  | "`" ModName "`"
  { $2 }

TyOper :: { Name }
  : "`" OptModName VarId "`"
  { maybeAddName' $2 $3 }
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

-- Types

Type :: { Type }
  : TupleType "::" Kind
  { TypeKind $1 $3 }
  | TupleType
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
  : TyVar
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

VarId :: { Name }
  : varid
  { startName $1 }
  | "as"
  { Name $1 False [] "as" }
  | "hiding"
  { Name $1 False [] "hiding" }
  | "import"
  { Name $1 False [] "import" }
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

OptModName :: { Maybe Name }
  :
  { Nothing }
  | ModName "."
  { Just $1 }

VarName :: { Name }
  : OptModName VarId
  { maybeAddName' $1 $2 }
  | ModName
  { $1 }
  | OptModName "(" VarSymId ")"
  { maybeAddName' $1 $3 }
  | OptModName "(" consymid ")"
  { maybeAddName $1 $3 }

TyOp :: { Name }
  : OptModName consymid
  { maybeAddName $1 $2 }
  | OptModName "`" conid "`"
  { maybeAddName $1 $3 }
  | "->"
  { Name $1 False [] "->" }

TyVar :: { Name }
  : ModName
  { $1 }
  | varid
  { startName $1 }
  | "_"
  { Name $1 False [] "_" }

Id :: { Name }
  : varid
  { startName $1 }
  | conid
  { startName $1 }


{-

OLDRULES

Decl        | TypeSigDecl                 { 1 }
            | Equation                    { 1 }

TopDecl     : Decl                        { 1 }
            | ClassDecl                   { 1 }
            | InstanceDecl                { 1 }
            | TypeDecl                    { 1 }
            | DataDecl                    { 1 }
            | BitdataDecl                 { 1 }
            | StructDecl                  { 1 }
            | AreaDecl                    { 1 }

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

-- These are defined in the Nov10 Habit report, page 30
TypeSigDecl : CommaVarList "::" SigType         { 1 }

CommaVarList : Var                              { 1 }
             | CommaVarList "," Var             { 1 }

-- These are defined in the Nov10 Habit report, page 31
ClassDecl   : "class" ClassLhs OptConstraints OptWhere  { 1 }

OptConstraints :                            { 1 }
               | "|" CommaConstList         { 1 }

CommaConstList : Constraint                    { 1 }
               | CommaConstList "," Constraint { 1 }

ClassLhs    : TypeLhs OptParam                  { 1 }

OptParam    : "=" TypeParam                     { 1 }

TypeLhs     : TypeParam Tyconop TypeParam       { 1 }
            | PreTypeLhs                        { 1 }

PreTypeLhs  : Tycon                             { 1 }
            | PreTypeLhs TypeParam              { 1 }

TypeParam   : Var                               { 1 }
            | "(" TypeParam OptKind ")"         { 1 }

OptKind     :                                   { 1 }
            | Kind                              { 1 }

Constraint  : FunDep                            { 1 }
            | Con ConstArgs                     { 1 }
            | "(" Preds ")"                     { 1 }

ConstArgs   : Type                              { 1 }
            | ConstArgs Type                    { 1 }

FunDep      : OptListVar "->" ListVar           { 1 }

OptListVar  :                                   { 1 }
            | ListVar                           { 1 }

ListVar     : Var                               { 1 }
            | ListVar Var                       { 1 }

-- These are defined in the Nov10 Habit report, page 33
InstanceDecl
            : "instance" Instances              { 1 }

Instances   : Instance                          { 1 }
            | Instances "else" Instance         { 1 }

Instance    : Pred OptIfPreds OptWhere          { 1 }

OptIfPreds  : "if" Preds                        { 1 }


-- This is defined in the Nov10 Habit report, page 33
TypeDecl    : "type" TypeLhs "=" Type     { 1 }


-- These are defined in the Nov10 Habit report, page 38
DataDecl    : "data" TypeLhs OptConst OptDerive
                                          { 1 }

OptConst    :                             { 1 }
            | "=" DataCons                { 1 }

DataCons    : DataCon                     { 1 }
            | DataCons "|" DataCon        { 1 }

OptDerive   :                             { 1 }
            | "deriving" DeriveList       { 1 }

DataCon     : DataCon Conop Type          { 1 }
            | PreDataCon                  { 1 }

PreDataCon  : Con                         { 1 }
            | PreDataCon AType            { 1 }
            | "(" DataCon ")"             { 1 }

DeriveList  : Con                         { 1 }
            | "(" Cons ")"                { 1 }

Cons        : Con                         { 1 }
            | Cons "," Con                { 1 }

-- These are defined in the Nov10 Habit report, page 39
BitdataDecl : "bitdata" conid OptSlType OptBCons OptDerive
                                          { 1 }

OptSlType   :                             { 1 }
            | "/" Type                    { 1 }

OptBCons    :                             { 1 }
            | "=" BCons                   { 1 }

BCons       : BitdataCon                  { 1 }
            | BCons "|" BitdataCon        { 1 }

BitdataCon  : Con "[" BDataFields "]"     { 1 }

BDataFields : BitdataField                { 1 }
            | BDataFields "|" BitdataField
                                          { 1 }

BitdataField
            : varid "::" AppType          { 1 }
            | varid "=" Expr "::" AppType { 1 }
            | Expr                        { 1 }

-- These are defined in the Nov10 Habit report, page 44.
StructDecl  : "struct" conid OptSlType "[" SRegions "]" OptDerive
                                          { 1 }

SRegions    : StructRegion                { 1 }
            | SRegions "|" StructRegion   { 1 }

StructRegion
            : OptFieldNames Type          { 1 }

OptFieldNames
            :                             { 1 }
            | StrFields "::"              { 1 }

StrFields   : StructField                 { 1 }
            | StrFields "," StructField   { 1 }

StructField : Id OptInit                  { 1 }

OptInit     :                             { 1 }
            | "<-" Expr                   { 1 }

-- These are defined in the Nov10 Habit report, page 44.
AreaDecl    : "area" AreaVars "::" Type OptWhere
                                          { 1 }

AreaVars    : AreaVar                     { 1 }
            | AreaVars "," AreaVar        { 1 }

AreaVar     : Var OptInit                 { 1 }

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


-- These are defined in the Nov10 Habit report, page 17-18
SigType     : Type                        { 1 }
            | Pred "=>"                   { 1 }
            | Pred "=>" Type              { 1 }
            | "(" Preds ")" "=>"          { 1 }
            | "(" Preds ")" "=>" Type     { 1 }

Preds       : Pred                        { 1 }
            | Preds  "," Pred             { 1 }

Pred        : Type OptEqType OptFails     { 1 }

OptEqType   :                             { 1 }
            | "=" Type                    { 1 }

OptFails    :                             { 1 }
            | "fails"                     { 1 }


-- These are defined in the Nov10 Habit report, page 15
Type        : AppType                     { 1 }
            | Type Tyop AppType           { 1 }

AppType     : AType                       { 1 }
            | AppType AType               { 1 }

AType       : varid                       { 1 }
            | conid                       { 1 }
            | "(" ")"                     { 1 }
            | int                         { 1 }
            | "#." Id                     { 1 }
            | AType "." Id                { 1 }
            | "(" Tyop ")"                { 1 }
            | "(" Type OptionalKind ")"   { 1 }

OptionalKind : "::" Kind                  { 1 }

-- These are defined in the Nov10 Habit report, page 14-15
Kind        : AKind "->" Kind         { 1 }
            | AKind                   { 1 }

AKind       : "*"                     { 1 }
            | "type"                  { 1 }
            | int                     { 1 }
            | "area"                  { 1 }
            | "lab"                   { 1 }
            | "(" Kind ")"            { 1 }

-- These are defined in the Nov10 Habit report, page 11
Tyvar       : varid                   { 1 }

Tyvarop     : "`" varid "`"           { 1 }

Tycon       : Con                     { 1 }
            | "(" varsymid ")"        { 1 }

Tyconop     : Conop                   { 1 }
            | varsymid                { 1 }
            | "->"                    { 1 }

Tyop        : Tyvarop                 { 1 }
            | Tyconop                 { 1 }


-- These are defined in the Nov10 Habit report, page 10
Var         : varid                   { 1 }
            | "(" varsymid ")"        { 1 }

Varop       : varsymid                { 1 }
            | "`" varid "`"           { 1 }

Con         : conid                   { 1 }
            | "(" consymid ")"        { 1 }

Conop       : consymid                { 1 }
            | "`" conid "`"           { 1 }

Op          : Varop                   { 1 }
            | Conop                   { 1 }

Id          : varid                   { 1 }
            | conid                   { 1 }

-}

{

data HabitModule = HabitModule Name [Decl]
 deriving (Show)

data Decl        = ImportDecl AlexPosn Bool Name (Maybe Name) ImportMods
                 | FixityDecl AlexPosn FixityType Bool (Maybe Integer) [Name]
                 | TypeSigDecl AlexPosn Name Type
 deriving (Show)

data ImportMods = IncludeOnly [Name]
                | HidingNames [Name]
                | NoMods
 deriving (Show)

data FixityType = FixityLeft | FixityRight | FixityBoth
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

maybeAddName :: Maybe Name -> Lexeme -> Name
maybeAddName Nothing  x = startName x
maybeAddName (Just m) x = addName m x

maybeAddName' :: Maybe Name -> Name -> Name
maybeAddName' Nothing  x = x
maybeAddName' (Just (Name p1 m1 ls1 x1)) (Name _ m2 ls2 x2) =
  Name p1 (m1 && m2) (ls1 ++ [x1] ++ ls2) x2

happyError :: Alex a
happyError = fail "Parse Failed"

fromBS :: ByteString -> String
fromBS = map (chr . fromIntegral) . unpack

}


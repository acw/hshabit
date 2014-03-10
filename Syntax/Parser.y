{
module Syntax.Parser(parseHabit) where

import Syntax.Tokens
}

%name parseHabit
%monad { Alex }
%lexer { tokenize } { EOF }

%tokentype { Lexeme }
%token
  "area"        { ReservedId $$ "area" }
  "bitdata"     { ReservedId $$ "bitdata" }
  "case"        { ReservedId $$ "case" }
  "class"       { ReservedId $$ "class" }
  "data"        { ReservedId $$ "data" }
  "deriving"    { ReservedId $$ "deriving" }
  "do"          { ReservedId $$ "do" }
  "else"        { ReservedId $$ "else" }
  "fails"       { ReservedId $$ "fails" }
  "if"          { ReservedId $$ "if" }
  "in"          { ReservedId $$ "in" }
  "infix"       { ReservedId $$ "infix" }
  "infixl"      { ReservedId $$ "infixl" }
  "infixr"      { ReservedId $$ "infixr" }
  "instance"    { ReservedId $$ "instance" }
  "lab"         { ReservedId $$ "lab" }
  "let"         { ReservedId $$ "let" }
  "of"          { ReservedId $$ "of" }
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
  "*"           { ReservedSym $$ "." _ }
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
Prog        : TopDecl                     { 1 }
            | Prog ";" TopDecl            { 1 }

Decl        : Equation                    { 1 }
            | FixityDecl                  { 1 }
            | TypeSigDecl                 { 1 }

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
            | Pat                         { 1 }

OptPatList  :                             { 1 }
            | OptPatList APat             { 1 }

EqRhs       : EqRhs1 OptWhere             { 1 }

EqRhs1      : "=" Expr                    { 1 }
            | GuardEqRhs                  { 1 }

GuardEqRhs  : "|" Expr "=" Expr             { 1 }
            | GuardEqRhs "|" Expr "=" Expr  { 1 }

-- These are defined in the Nov10 Habit report, page 30
FixityDecl  : Assoc Prec CommaOpList            { 1 }
            | Assoc "type" Prec CommaTyopList   { 1 }

Assoc       : "infixl"                          { 1 }
            | "infixr"                          { 1 }
            | "infix"                           { 1 }

Prec        :                                   { 1 }
            | int                               { 1 }

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
            | "(" ClassLhs ")"                  { 1 }

OptParam    : "=" TypeParam                     { 1 }

TypeLhs     : TypeParam Tyconop TypeParam       { 1 }
            | PreTypeLhs                        { 1 }

PreTypeLhs  : Tycon                             { 1 }
            | PreTypeLhs TypeParam              { 1 }
            | "(" TypeLhs ")"                   { 1 }

TypeParam   : Var                               { 1 }
            | "(" TypeParam OptKind ")"         { 1 }

OptKind     :                                   { 1 }
            | Kind                              { 1 }

Constraint  : Pred                              { 1 }
            | FunDep                            { 1 }

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

DataCon     : Type Conop Type             { 1 }
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
            : varid OptEqExpr "::" AppType
                                          { 1 }
            | Expr                        { 1 }

OptEqExpr   :                             { 1 }
            | "=" Expr                    { 1 }


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
            | Stmts ";" "let" DeclBlock   { 1 }
            | Stmts ";" OptAssign Stmt    { 1 }

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
Pat         : AppPat PatInfixOps          { 1 }

PatInfixOps :                             { 1 }
            | Op AppPat                   { 1 }

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
SigType     : OptPreds Type               { 1 }

OptPreds    :                             { 1 }
            | Preds "=>"                  { 1 }

Preds       : "(" PredItems ")"           { 1 }
            | Pred                        { 1 }

PredItems   :                             { 1 }
            | PredItems2                  { 1 }

PredItems2  : Pred                        { 1 }
            | PredItems2 "," Pred         { 1 }

Pred        : AppPred OptEqType OptFails  { 1 }
            | SelPred "=" Type OptFails   { 1 }
            | "(" Pred ")"                { 1 }

OptEqType   :                             { 1 }
            | "=" Type                    { 1 }

OptFails    :                             { 1 }
            | "fails"                     { 1 }

AppPred     : Type Tyconop Type           { 1 }
            | PrePred                     { 1 }

PrePred     : Tycon                       { 1 }
            | PrePred AType               { 1 }
            | "(" AppPred ")"             { 1 }

SelPred     : AType "." Id                { 1 }
            | "(" SelPred ")"             { 1 }


-- These are defined in the Nov10 Habit report, page 15
Type        : AppType InfixTypeStuff      { 1 }

InfixTypeStuff
            :                             { 1 }
            | InfixTypeStuff Tyop AppType { 1 }

AppType     : AType                       { 1 }
            | AppType AType               { 1 }

AType       : Tyvar                       { 1 }
            | Tycon                       { 1 }
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
            | "(" "->" ")"            { 1 }

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


{

happyError :: Alex a
happyError = fail "Parse Failed"

}


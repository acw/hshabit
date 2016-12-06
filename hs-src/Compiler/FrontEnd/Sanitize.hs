module Compiler.FrontEnd.Sanitize(sanitizeParseAST)
 where

import Compiler.Error
import Compiler.FrontEnd.ModuleImport
import Compiler.Monad
import Compiler.Warning
import Control.Applicative
import Control.Exception(assert)
import Data.List(delete,intercalate)
import Data.Map.Strict(Map,insertWith,toList,traverseWithKey)
import qualified Data.Map.Strict as Map
import Data.Maybe(catMaybes)
import Data.Monoid
import Data.Word
import MonadLib
import System.FilePath
import Syntax.IL
import qualified Syntax.ParseAST as P
import Syntax.Posn

sanitizeParseAST :: P.HabitModule -> CompilerM IO HabitModule
sanitizeParseAST hm = runSanitizer (sanitizeM hm)

sanitizeM :: P.HabitModule -> SanitizeM HabitModule
sanitizeM (P.HabitModule name decls) =
  do decls <- catMaybes <$> mapM doImports decls
     (eqnmap, decls') <- gatherEqs decls Map.empty []
     mapM_ gatherNames decls'
     eqnmap' <- traverseWithKey validateEqn eqnmap
     decls'' <- mapM processD decls'
     eqns <- mapM generateEqn (toList eqnmap)
     let decls''' = decls'' ++ eqns
     undefined decls'''

-- ----------------------------------------------------------------------------
--
-- Import Gathering
--
-- ----------------------------------------------------------------------------

doImports :: P.Decl -> SanitizeM (Maybe P.Decl)
doImports (P.ImportDecl p qual (P.Name p' prefixes name) asname importmods) =
  do let mname = intercalate "." prefixes ++ "." ++ name
         fname = joinPath prefixes </> name <.> "o"
     validateImportMods importmods
     mmodl <- inBase $ openModule mname fname
     case mmodl of
       Nothing   ->
         inBase $ err (ImportNotFound p' mname fname)
       Just modl ->
         do tnames <- filterNames mname importmods (imodTypeNames modl)
            vnames <- filterNames mname importmods (imodValNames modl)
            addImportedModule modl
            qprefix <- qualifiedPrefix mname asname
            undefined qprefix tnames vnames
     return Nothing
doImports x =
  return (Just x)

qualifiedPrefix :: String -> Maybe P.Name -> SanitizeM String
qualifiedPrefix modname Nothing =
  return (modname ++ ".")
qualifiedPrefix modname (Just (P.Name p [] nm)) =
  return (nm ++ ".")
qualifiedPrefix modname (Just (P.Name p xs nm)) =
  do inBase $ err (AsNameHasPrefix p nm)
     return (nm ++ ".")

validateImportMods :: P.ImportMods -> SanitizeM ()
validateImportMods P.NoMods =
  return ()
validateImportMods (P.IncludeOnly names) =
  forM_ names $ \ (P.Name p pfx nm) ->
    unless (null pfx) $
      inBase $ err (IncludeHasPrefix p nm)
validateImportMods (P.HidingNames names) =
  forM_ names $ \ (P.Name p pfx nm) ->
    unless (null pfx) $
      inBase $ err (HiddenHasPrefix p nm)

filterNames :: String -> P.ImportMods -> [String] -> SanitizeM [String]
filterNames mname P.NoMods nms =
  return nms
filterNames mname (P.IncludeOnly []) nms =
  return []
filterNames mname (P.IncludeOnly ((P.Name p _ nm):rest)) nms
  | nm `elem` nms =
     (nm :) <$> filterNames mname (P.IncludeOnly rest) nms
  | otherwise =
     do inBase $ err (NoSuchImport p mname nm)
        filterNames mname (P.IncludeOnly rest) nms
filterNames mname (P.HidingNames []) nms = return nms
filterNames mname (P.HidingNames ((P.Name p _ nm):rest)) nms
  | nm `elem` nms =
     filterNames mname (P.HidingNames rest) (delete nm nms)
  | otherwise =
    do inBase $ err (NoSuchImport p mname nm)
       filterNames mname (P.HidingNames rest) nms

-- ----------------------------------------------------------------------------
--
-- Equation Gathering
--
-- ----------------------------------------------------------------------------

type EqnInfo = ([P.Decl], [P.Decl])

gatherEqs :: [P.Decl] -> Map String EqnInfo -> [P.Decl] ->
             SanitizeM (Map String EqnInfo, [P.Decl])
gatherEqs [] map acc = return (map, reverse acc)
gatherEqs (P.ImportDecl _ _ _ _ _ : _) _ _ =
  fail "INTERNAL ERROR: ImportDecl in gatherEqs."
gatherEqs (x@(P.TypeSigDecl _ (P.Name p pfx n) _):rest) map acc =
  do unless (null pfx) $
       inBase $ err (TopLevelHasPrefix p n)
     let map' = insertWith mappend n ([x],[]) map
     gatherEqs rest map' acc
gatherEqs (x@(P.EquationDecl _ (P.Name p pfx n) _ _ _):rest) map acc =
  do unless (null pfx) $
       inBase $ err (TopLevelHasPrefix p n)
     let map' = insertWith mappend n ([], [x]) map
     gatherEqs rest map' acc
gatherEqs (x@(P.LocalDecl p _ []):rest) map acc =
  do inBase $ err (EmptyWhere p)
     gatherEqs rest map acc
gatherEqs (x@(P.LocalDecl p wheres xs):rest) map acc =
  do splits <- splitLocalDecls xs
     case splits of
       ([], (P.EquationDecl _ (P.Name p pfx n) _ _ _):_) ->
         do unless (null pfx) $
              inBase $ err (TopLevelHasPrefix p n)
            let map' = insertWith mappend n ([], [x]) map
            gatherEqs rest map' acc
       ((P.AreaDecl _ _ _ _):_, _) ->
         gatherEqs rest map (x : acc)
       _ ->
         fail "INTERNAL ERROR: Bad local decls in gatherEqs."
gatherEqs (x:rest) map acc =
  gatherEqs rest map (x : acc)

splitLocalDecls :: [P.Decl] -> SanitizeM ([P.Decl], [P.Decl])
splitLocalDecls [] =
  return ([], [])
splitLocalDecls (e@(P.EquationDecl _ _ _ _ _) : rest) =
  (([], [e]) `mappend`) <$> splitLocalDecls rest
splitLocalDecls (a@(P.AreaDecl _ _ _ _) : rest) =
  (([a], []) `mappend`) <$> splitLocalDecls rest
splitLocalDecls (_ : rest) =
  fail "INTERNAL ERROR: Non-area / non-equation with top-level where."

-- ----------------------------------------------------------------------------
--
-- Name Gathering
--
-- ----------------------------------------------------------------------------

gatherNames :: P.Decl -> SanitizeM ()
gatherNames (P.ImportDecl _ _ _ _ _) =
  fail "INTERNAL ERROR: ImportDecl in gatherNames."
gatherNames (P.FixityDecl _ _ _ _ _) =
  return ()
gatherNames (P.TypeSigDecl _ _ _) =
  fail "INTERNAL ERROR: TypeSigDecl in gatherNames."
gatherNames (P.TypeDecl p lhst _) =
  do P.Name p' pfx n <- getNameFromTypeLhs lhst
     unless (null pfx) $ inBase $ err (TopLevelHasPrefix p' n)
     addTopLevelName p n
gatherNames (P.StructDecl p (P.Name p' pfx n) _ _ _) =
  do unless (null pfx) $ inBase $ err (TopLevelHasPrefix p' n)
     addTopLevelName p n
gatherNames (P.BitdataDecl p (P.Name p' pfx n) _ bflds _) =
  do unless (null pfx) $ inBase $ err (TopLevelHasPrefix p' n)
     addTopLevelName p n
     forM_ bflds $ \ (P.Name fp fpfx fn, _) ->
       do unless (null fpfx) $ inBase $ err (ConstructorHasPrefix fp fn)
          addTopLevelName fp fn
gatherNames (P.AreaDecl p (P.Name p' pfx n) _ _) =
  do unless (null pfx) $ inBase $ err (TopLevelHasPrefix p' n)
     addTopLevelName p n
gatherNames (P.ClassDecl p lhst _ _ _) =
  do P.Name p' pfx n <- getNameFromTypeLhs lhst
     unless (null pfx) $ inBase $ err (TopLevelHasPrefix p' n)
     addTopLevelName p n
gatherNames (P.InstanceDecl _ _) =
  return ()
gatherNames (P.DataDecl p lhst items _ _) =
  do P.Name p' pfx n <- getNameFromTypeLhs lhst
     unless (null pfx) $ inBase $ err (TopLevelHasPrefix p' n)
     cnames <- concat <$> mapM (getConstructorNames p) items
     forM_ cnames $ \ (P.Name cp cpfx cn) ->
       do unless (null cpfx) $ inBase $ err (ConstructorHasPrefix cp cn)
          addTopLevelName cp cn
gatherNames (P.EquationDecl _ _ _ _ _) =
  fail "INTERNAL ERROR: EquationDecl in gatherNames."
gatherNames (P.LocalDecl _ _ xs) =
  forM_ xs $
    \case
      P.AreaDecl p (P.Name p' pfx n) _ _ ->
        do unless (null pfx) $ inBase $ err (TopLevelHasPrefix p' n)
           addTopLevelName p n
      _ ->
        fail "INTERNAL ERROR: Non-area decl in LocalDecl in gatherNames."

getNameFromTypeLhs :: P.Type -> SanitizeM P.Name
getNameFromTypeLhs (TypeRef x) = return x
getNameFromTypeLhs (TypeApp (TypeRef x) [_, _]) = return x
getNameFromTypeLhs (TypeApp x [_]) = getNameFromType x
getNameFromTypeLhs = fail "INTERNAL ERROR: Weird value for TypeLhs"

getConstructorNames :: Posn -> [P.Type] -> SanitizeM [P.Name]
getConstructorNames _ (TypeRef n) =
  return [n]
getConstructorNames _ (TypeApp x _) =
  getConstructorNames x
getConstructorNames p _ =
  inBase $ err (InvalidConstructorType p)
  return []

-- ----------------------------------------------------------------------------
--
-- Equation Validation
--
-- ----------------------------------------------------------------------------

validateEqn :: String -> ([P.Decl], [P.Decl]) ->
               SanitizeM (Maybe P.Decl, Posn, Int, [P.Decl])
validateEqn name ([], vdecls) =
  do inBase $ warn $ NoTypeForTopLevel name
     validateArgCount (error "validateEqn: p") (error "validateEqn: Huh?")
                      Nothing vdecls
     return (Nothing, undefined, 0, vdecls)
validateEqn name ([td@(P.TypeSigDecl p (P.Name _ _ n) _)], vdecls) =
  do count <- validateArgCount p n Nothing vdecls
     return (Just td, p, count, vdecls)
validateEqn name (tds@((td@(P.TypeSigDecl p (P.Name _ _ n) _)):_), vdecls) =
  do inBase $ err $ TooManyTypeSigs p n
     count <- validateArgCount p n Nothing vdecls
     return (Just td, p, count, vdecls)

validateArgCount :: Posn -> String -> Maybe Int -> [P.Decl] -> SanitizeM Int
validateArgCount p n Nothing [] =
  do inBase $ err $ NoEqnForTypeSig p n
     return 0
validateArgCount _ _ (Just x) [] =
  return x
validateArgCount p n cnt ((P.LocalDecl _ _ ds):xs) =
  validateArgCount p n cnt (ds ++ xs)
validateArgCount p n Nothing (P.EquationDecl _ _ ptns _ _ : rest) =
  validateArgCount p n (Just (length ptns)) rest
validateArgCount p n (Just cnt) (P.EquationDecl p' _ ptns _ _ : rest) =
  do unless (cnt == length ptns) $ inBase $ err (DiffArgCounts p' n)
     validateArgCount p n (Just cnt) rest
validateArgCount _ n _ d =
  fail ("INTERNAL ERROR: Unexpected decl in validateArgCount: " ++ show d)

-- ----------------------------------------------------------------------------

generateEqn :: (String, (Maybe P.Decl, Posn, Int, [P.Decl])) ->
               SanitizeM (Maybe Declaration)
generateEqn (_, (_, _, _, [])) = return Nothing -- error generated before
generateEqn (name, (mtype, _, 0, [x])) =
  do name' <- lookupName name
     let dtype = fromMaybe TUnknown name'
     x' <- sanitizeE' x
     return (Just (Declaration name' dtype x'))
generateEqn (name, (mtype, _, 0, (_:_))) =
  do inBase $ err (TooManyEqnsForValue p name)
     return Nothing
generateEqn (name, mtype, p, count, decls) =
  do name' <- lookupName name
     names <- replicate count gensymName
     cases <- mapM generateEqnCase decls
     let dtype    = fromMaybe TUnknown name'
         lambda   = ELambda p (map (PRef p) names) caseb
         caseb    = ECase TypeUnknown argtuple cases
         argtuple = map (ERef p TypeUnknown) names
     return (Just (Declaration name' dtype lambda))

generateEqnCase :: P.Decl -> SanitizeM CaseArm
generateEqnCase (LocalDecl p locals body) =
  undefined
generateEqnCase (EquationDecl p name ptrn mguard body) =
  undefined
generateEqnCase _ =
  fail "INTERNAL ERROR: Invalid equation value reached generateEqnCase."

-- ----------------------------------------------------------------------------

processD (P.ImportDecl p qual (P.Name p' prefixes name) asname importmods) =
  undefined
processD (P.FixityDecl p ft b mnum name) =
  undefined
processD (P.TypeSigDecl p nm t) =
  undefined
processD (P.TypeDecl p t1 t2) =
  undefined
processD (P.StructDecl p n mt flds nms) =
  undefined
processD (P.BitdataDecl p n mt flds nms) =
  undefined
processD (P.AreaDecl p n me t) =
  undefined
processD (P.ClassDecl p t mt consts decls) =
  undefined
processD (P.InstanceDecl p insts) =
  undefined
processD (P.DataDecl p t ts nms consts) =
  undefined
processD (P.EquationDecl p n pts me e) =
  undefined
processD (P.LocalDecl p ds1 ds2) =
  undefined

sanitizeD (P.ImportDecl p qual (P.Name p' prefixes name) asname importmods) =
  undefined
sanitizeD (P.FixityDecl p ft b mnum name) =
  undefined
sanitizeD (P.TypeSigDecl p nm t) =
  undefined
sanitizeD (P.TypeDecl p t1 t2) =
  undefined
sanitizeD (P.StructDecl p n mt flds nms) =
  undefined
sanitizeD (P.BitdataDecl p n mt flds nms) =
  undefined
sanitizeD (P.AreaDecl p n me t) =
  undefined
sanitizeD (P.ClassDecl p t mt consts decls) =
  undefined
sanitizeD (P.InstanceDecl p insts) =
  undefined
sanitizeD (P.DataDecl p t ts nms consts) =
  undefined
sanitizeD (P.EquationDecl p n pts me e) =
  undefined
sanitizeD (P.LocalDecl p ds1 ds2) =
  undefined

checkLetDecls :: [P.Decl] -> SanitizeM [P.Decl]
checkLetDecls = undefined

sanitizeE :: P.Expr -> SanitizeM Expression
sanitizeE (P.ExprConst cv) =
  sanitizeC cv <$> gensym <*> gensym
sanitizeE (P.ExprRef nm@(P.Name p _ _)) =
  do tname <- gensym
     name' <- convertName nm
     return (ERef p (TRef p tname) name')
sanitizeE (P.ExprTuple p vals) =
  do vals' <- mapM sanitizeE vals
     let types = map exprType vals'
     return (ETuple p (TTuple p types) vals')
sanitizeE (P.ExprLet p decls expr) =
  do decls' <- mapM sanitizeD =<< checkLetDecls decls
     expr' <- sanitizeE expr
     return (ELet p (exprType expr') decls' expr')
sanitizeE (P.ExprIf p t c a) =
  do tname <- gensym
     EIf p (TRef p tname) <$> sanitizeE t <*> sanitizeE c <*> sanitizeE a
sanitizeE (P.ExprIfM p t c a) =
  do vname <- gensym
     mname <- gensym
     let monadT n = TApp p (TRef p mname) [TRef p n]
     finalType <- monadT <$> gensym
     ctype <- monadT <$> gensym
     atype <- monadT <$> gensym
     t' <- sanitizeE t
     c' <- mapM sanitizeS c
     a' <- mapM sanitizeS a
     let cpos = getFirstPos p c'
         apos = getFirstPos p a'
     return (EDo p finalType [
               SBind p vname t'
             , SExpr p (EIf p finalType (ERef p (exprType t') vname)
                           (EDo cpos ctype c')
                           (EDo apos atype a'))
             ])
sanitizeE (P.ExprCase p e cases) = undefined
sanitizeE (P.ExprCaseM p e cases) = undefined
sanitizeE (P.ExprDo p xs) =
  do mname <- gensym
     tname <- gensym
     xs' <- mapM sanitizeS xs
     return (EDo p (TApp p (TRef p mname) [TRef p tname]) xs')
sanitizeE (P.ExprLambda p ptns body) =
  do ptns' <- mapM sanitizeP ptns
     targs <- mapM (\ _ -> gensym) ptns'
     body' <- sanitizeE body
     return (ELambda p (TFun p (map (TRef p) targs) (exprType body')) ptns' body')
sanitizeE (P.ExprType p expr t) =
  do t' <- sanitizeT t
     expr' <- sanitizeE expr
     return expr'{ exprType = t' }
sanitizeE (P.ExprApply fun args) =
  do rtn <- gensym
     fun' <- sanitizeE fun
     ECall (exprPosn fun') (TRef (exprPosn fun') rtn) fun' <$> mapM sanitizeE args
sanitizeE (P.ExprInfix left rights) = undefined
sanitizeE (P.ExprFldRef p expr field) = undefined
sanitizeE (P.ExprUpdate p expr fields) = undefined
sanitizeE (P.ExprBuild p expr fields) = undefined

sanitizeS :: P.Statement -> SanitizeM Statement
sanitizeS = undefined

sanitizeT :: P.Type -> SanitizeM Type
sanitizeT = undefined

sanitizeP :: P.Pattern -> SanitizeM Pattern
sanitizeP = undefined

-- ----------------------------------------------------------------------------
--
-- Sanitize Constants
--
-- ----------------------------------------------------------------------------

sanitizeC :: P.ConstVal -> Name -> Name -> Expression
sanitizeC (P.ConstInt p v n) tn1 tn2 =
  ECall p (TPredicated p [Predicate (builtinName "NumLit") [tn1]] (TRef p tn1))
        (ERef p (TRef p tn2) (builtinName "fromIntegral"))
        [EConst p (TRef p (builtinName "Integer")) (CInteger v n)]
sanitizeC (P.ConstVec p v b l) tn1 tn2 =
  EConst p (TApp p (TRef p (builtinName "Bit")) [TInt p (fromIntegral l)])
         (CInteger v b)
sanitizeC (P.ConstFloat p v) tn1 tn2 =
  ECall p (TPredicated p [Predicate (builtinName "FloatLit") [tn1]] (TRef p tn1))
        (ERef p (TRef p tn2) (builtinName "fromFloat"))
        [EConst p (TRef p (builtinName "Float")) (CFloat v)]
sanitizeC (P.ConstDouble p v) tn1 tn2 =
  ECall p (TPredicated p [Predicate (builtinName "FloatLit") [tn1]] (TRef p tn1))
        (ERef p (TRef p tn2) (builtinName "fromDouble"))
        [EConst p (TRef p (builtinName "Double")) (CDouble v)]
sanitizeC (P.ConstUnit p) tn1 tn2 =
  EConst p (TUnit p) CUnit
sanitizeC (P.ConstLabel p n) tn1 tn2 =
  EConst p (TLabel p (convertLabel n)) (CLabel (convertLabel n))

-- ----------------------------------------------------------------------------
--
-- Helper Functions
--
-- ----------------------------------------------------------------------------

getFirstPos :: Posn -> [Statement] -> Posn
getFirstPos = undefined

convertLabel :: P.Name -> Name
convertLabel = undefined

convertName :: P.Name -> SanitizeM Name
convertName = undefined

builtinName :: String -> Name
builtinName _ = undefined

newtype SanitizeM a = SM (StateT () (CompilerM IO) a)
 deriving (Applicative,Functor,Monad)

instance BaseM SanitizeM (CompilerM IO)
  where inBase x = SM (inBase x)

runSanitizer :: SanitizeM a -> CompilerM IO a
runSanitizer (SM x) = fst `fmap` runStateT () x

gensym :: SanitizeM Name
gensym = undefined

addTopLevelName :: Posn -> String -> SanitizeM ()
addTopLevelName = undefined

addImportedModule :: ImportedModule -> SanitizeM ()
addImportedModule = undefined

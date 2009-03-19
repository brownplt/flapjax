module Flapjax.Compiler where

import Data.List(partition)
import System.Random(randomRIO)
import Flapjax.Syntax
import WebBits.Common -- pretty-printable
import WebBits.JavaScript.Syntax
import Flapjax.HtmlEmbedding() -- just instances
import Misc
import Text.ParserCombinators.Parsec(SourcePos)
import Text.PrettyPrint.HughesPJ(render)
import Control.Monad
import Control.Monad.Trans
import Data.Generics hiding (GT)
import Data.IORef -- awful!
import qualified Flapjax.Symbols as Symbols
import qualified WebBits.Html.Syntax as Html
import qualified Flapjax.Builder as J
import Computation hiding (warn)
import WebBits.JavaScript.Env (localVars)
import qualified Data.Set as S

{-{{{ Structure of the compiler:

  Full-page compilation passes:
  
   + avoidNameClashes:: CompilerOpts -> Html -> IO CompilerOpts
     
     This phase collects all Javascript identifiers on the page and ensures that
     the names used for compiler-introduced variables are unique.  Due to
     Javascript's reflection capabilities, it is possible for a page to violate
     these, but any sane programmer should not have such problems.
     
   + compileInline:: CompilerOpts -> Html -> IO Html
     
     This phase transforms all inline expressions into vanilla Flapjax scripts.
     These scripts are subsequently compiled to Javascript in compileScripts.
   
   + compileInlineAtAttribs:: CompilerOpts -> Html -> IO Html
     
     This phase transforms all attribute-inline expressions into vanilla
     Flapjax scripts.
   
   + compileScripts:: CompilerOpts -> Html -> IO Html
     
     This phase compiles all Flapjax scripts on the page into pure Javascript.
     Earlier phases transform inline-Flapjax into Flapjax scripts, so after this
     phase, the page is HTML with pure Javascript.  However, the initialization
     code has yet to be inserted.
     
   + installInit:: CompilerOpts -> Html -> IO Html
     
     This phase inserts initialization code in the head of the document.
     
   + addLoaderToBody:: CompilerOpts -> Html -> IO Html
     
     This phase adds code to the onload attribute of the body, to start the
     application.
     
}}}-}

--------------------------------------------------------------------------------
--{{{ Miscellaneous

type ParsedExpression = Expression SourcePos
type ParsedStatement = Statement SourcePos

type Html = Html.Html SourcePos Flapjax
type Attribute = Html.Attribute SourcePos Flapjax

type Compiler a = CompT CompilerWarning IO a

data CompilerOpts = CompilerOpts {
  flapjaxObject:: String,
  flapjaxLoader:: String,
  -- unique prefix used for ids on DOM nodes created by the compiler.
  flapjaxIdBase:: String,
  -- unique prefix used for ids on DOM nodes created while compiling attribute
  -- expressions.
  flapjaxAttributeIdBase:: String,
  flapjaxPath:: String
}

data CompilerWarning
  = ImpureStatement ParsedStatement String
  | ImpureExpression ParsedExpression String

defaults = CompilerOpts {
  flapjaxObject = "flapjax",
  flapjaxLoader = "loader",
  flapjaxIdBase = "flapjaxsuid",
  flapjaxAttributeIdBase = "flapjaxattribsuid",
  flapjaxPath = "flapjax.js"
}

warn:: PrettyPrintable a => String -> a -> SourcePos -> IO () 
warn desc ast pos = do
  putStr $ "Warning: " ++ desc ++ " at " ++ (show pos) ++ "\n"
  putStr $ "Offending expression:\n"
  putStr $ render (pp ast)
  putStr $ "\n"

isCompilerFunction:: (Id a) -> Bool
isCompilerFunction (Id p v) = v `elem` Symbols.flapjaxCompilerFuns

isCoreFunction:: (Id a) -> Bool
isCoreFunction (Id p v) = v `elem` Symbols.flapjaxCoreFuns

isUnliftedFunction:: (Id a) -> Bool
isUnliftedFunction (Id _ v) = v `elem` Symbols.flapjaxNoliftFuns

isUnliftedMethod:: Id a -> Bool
isUnliftedMethod (Id _ v) = v `elem` Symbols.flapjaxNoliftMethods

--}}}

--------------------------------------------------------------------------------
--{{{ Whole-page pass: avoid name clashes

-- Merge two lists of lists that are sorted, by the lengths of the lists first,
-- and then the natural ordering of the elements.
--   E.g.: merge ["aaa"] ["zz"] => ["zz","aaa"]
--         merge ["zz","aaa"] ["a"] => ["a","zz","aaa"]
merge:: (Ord a) => [[a]] -> [[a]] -> [[a]]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) =
  let cmp a b =
       case compare (length a) (length b) of
         LT -> LT
         GT -> GT
         EQ -> compare a b
    in case cmp x y of
         LT -> x:(merge xs (y:ys))
         GT -> y:(merge (x:xs) ys)
         EQ -> x:(merge xs ys)

idName:: Id SourcePos -> [String]
idName (Id p s) = [s]

allNames:: Data a => a -> [String]
allNames = everything merge ([] `mkQ` idName)

idAttribute:: Attribute -> [String]
idAttribute (Html.Attribute "id" id p) = [id]
idAttribute _                          = []

allIds:: Data a => a -> [String]
allIds = everything merge ([] `mkQ` idAttribute)

generateUniqueName:: String -> [String] -> IO String
generateUniqueName base used =
  if base `elem` used
    then do randomChar <- randomRIO ('a','z')
            generateUniqueName (base ++ [randomChar]) used
    else return base

avoidNameClashes:: CompilerOpts -> Html -> Compiler CompilerOpts
avoidNameClashes opts page =
  let names = allNames page
      ids   = allIds page
    in do obj <- lift $ generateUniqueName "flapjax" names
          loader <- lift $ generateUniqueName "loader" names
          idBase <- lift $ generateUniqueName "flapjaxId" ids
          attributeIdBase <- lift $ generateUniqueName "flapjaxId" (idBase:ids)
          return $ opts { flapjaxObject = obj, flapjaxLoader = loader, 
                          flapjaxIdBase = idBase, 
                          flapjaxAttributeIdBase = attributeIdBase }

--}}}

--------------------------------------------------------------------------------
--{{{ Whole-page pass: Compile Inline Flapjax-scripts
--
-- {! expression !} gets compiled to:
--
-- <script type="text/flapjax">
--         insertDomB(expression,uid,'over');
-- </script><span id=uid></span>

mixedInsertDom opts = J.call (J.ref (J.id "insertDomB"))

inlineToScript:: CompilerOpts -> IORef Int -> Html -> Compiler Html
inlineToScript opts refId (Html.InlineScript (Inline p e) _ init) = do
  id <- lift (readIORef refId)
  let uid = (flapjaxIdBase opts) ++ show id
      e' = ExprStmt p $ mixedInsertDom opts 
                          [e,StringLit p uid,StringLit p "over"]
    in do lift $ writeIORef refId (id+1)
          return $ Html.HtmlSeq
                     [Html.Element "script" 
                                   [Html.Attribute "lang" "flapjax" p]
                                   [Html.Script (FlapjaxScript p [e']) p] p,
                      Html.Element "span"
                                   [Html.Attribute "id" uid p]
                                   [Html.Text init p] p]
inlineToScript _ _ other =
  return other

compileInline:: CompilerOpts -> Html -> Compiler Html
compileInline opts page = do
  refId <- lift (newIORef 0) -- We sequentially number each span.
  everywhereM (mkM (inlineToScript opts refId)) page  

--}}}

--------------------------------------------------------------------------------
--{{{ Whole-page pass: Compile attribute-inline Flapjax-expressions
--
-- <tag attrib={! expr !} ... > ... </tag> gets compiled to:
--
--   <span>
--     <script type="text/flapjax">
--       insertValue_b(expr,uid,attrib);
--       ..
--     </script>
-- <tag id="uid" attrib="" ...> ... </tag>
--
-- If the tag already has an id attribute, we use it instead of generating a new
-- id.

-- Return the Flapjax-free attribute and Flapjax code for the expression
compileAttributeExpr:: String -> CompilerOpts -> Attribute -> 
                       IO (Attribute,ParsedStatement)
compileAttributeExpr elemId opts 
                     (Html.AttributeExpr p id (InlineAttribute p' e) i) =
  let mixedInsert = J.call (J.ref (J.id "mixedInsertValue_eb"))
    in return (Html.Attribute id i p,
               ExprStmt p (mixedInsert [e,StringLit p elemId,StringLit p id]))

containsInlineExprs [] = False
containsInlineExprs ((Html.AttributeExpr _ _ _ _):rest) = True
containsInlineExprs (_:rest) = containsInlineExprs rest

-- Return the value of the id attribute, inserting one if necessary.
insertIdAttribute:: String -> IORef Int -> [Attribute] -> IO (String,[Attribute])
insertIdAttribute base refCounter attrs =
  case Html.attributeValue "id" attrs of
    (Just id) -> return (id,attrs)
    Nothing   -> do counter <- readIORef refCounter
                    newId <- return $ base ++ show counter
                    writeIORef refCounter (counter+1)
                    return (newId,(Html.Attribute "id" newId noPos):attrs)


compileAttribs:: CompilerOpts -> IORef Int -> Html -> IO Html
compileAttribs opts refId element@(Html.Element tag attrs children p)
  | containsInlineExprs attrs == False = return element
  | otherwise = do
      (id,attrs) <- insertIdAttribute (flapjaxAttributeIdBase opts) refId attrs
      (exprs,nonExprs) <- return $ partition Html.isAttributeExpr attrs
      (eAttrs,eStmts) <- liftM unzip (mapM (compileAttributeExpr id opts) exprs)
      return $ Html.Element "span" []
                 [Html.Element "script" [Html.Attribute "lang" "flapjax" p]
                    [Html.Script (FlapjaxScript p eStmts) p] p,
                  Html.Element tag (eAttrs ++ nonExprs) children p] p
compileAttribs opts refId v = return v

compileInlineAtAttribs:: CompilerOpts -> Html -> IO Html
compileInlineAtAttribs opts page = do
  refId <- newIORef 0
  everywhereM (mkM (compileAttribs opts refId)) page

--}}}

--------------------------------------------------------------------------------
--{{{ Whole-page pass: Compile scripts

--{{{ Pass: fix scoping
--
-- Function and variable declarations at the top-level of a Flapjax script get
-- wrapped in a loader function.  Therefore, they must be transformed into
-- assignments.

fixScopingM stmt = return $ everywhereBut (extQ (mkQ True stopFn) stopFnS) (mkT fixScope) stmt where

  stopFn :: Expression SourcePos -> Bool
  stopFn (FuncExpr{}) = False
  stopFn _ = True

  stopFnS :: Statement SourcePos -> Bool
  stopFnS (FunctionStmt{}) = False
  stopFnS _ = True

  fixScope:: Statement SourcePos -> Statement SourcePos
  fixScope (FunctionStmt p f args stmt) =
    ExprStmt p (AssignExpr p OpAssign (VarRef p f) (FuncExpr p args stmt))
  fixScope (VarDeclStmt p decls) =
    let toAssign (VarDecl p id (Just expr)) = 
          AssignExpr p OpAssign (VarRef p id) expr
        -- TODO: empty var-decls
      in ExprStmt p (ListExpr p (map toAssign decls))
  fixScope other = other

--}}}

--{{{ Pass: Lifting

mkIds:: SourcePos -> [a] -> [Id SourcePos]
mkIds pos xs = take (length xs) (map mkId [0..]) where
  mkId n = Id pos ("arg" ++ show n)

warnImpureStmt:: ParsedStatement -> IO ParsedStatement
warnImpureStmt s@(IfStmt p _ _ _) = 
  warn "unlifted if" s p >> return s
warnImpureStmt s@(IfSingleStmt p _ _) =
  warn "unlifted if" s p >> return s
warnImpureStmt s@(SwitchStmt p _ _) =
  warn "unlifted switch" s p >> return s
warnImpureStmt s@(ForStmt p _ _ _ _) =
  warn "unlifted for loop" s p >> return s
warnImpureStmt s@(ForInStmt p _ _ _) =
  warn "unlifted for..in loop" s p >> return s
warnImpureStmt s@(WhileStmt p _ _) =
  warn "unlifted while loop" s p >> return s
warnImpureStmt s@(DoWhileStmt p _ _) =
  warn "unlifted do..while loop" s p >> return s
warnImpureStmt s@(TryStmt p _ _ _) =
  warn "unlifted try block" s p >> return s
warnImpureStmt s@(ThrowStmt p _) =
  warn "unlifted throw statement" s p >> return s 
warnImpureStmt s = return s

--impurities: postfix, impure-prefix, impure-assignment

isPurePrefixOperator op = 
  op `elem` [PrefixTypeof,PrefixPlus,PrefixMinus,PrefixBNot,PrefixLNot]

isPureAssignOperator op =
  op `elem` [OpAssign]

liftExprM :: S.Set String
          -> CompilerOpts -> ParsedExpression -> IO ParsedExpression
liftExprM fxenv opts expr = liftM expr where
  fj = VarRef noPos (Id noPos (flapjaxObject opts))
  cxt = DotRef noPos fj (Id noPos "maybeEmpty")
  mixedLift args = --J.call (J.ref (J.id "mixedLift_eb")) (cxt:args)
    CallExpr noPos (VarRef noPos (Id noPos "compilerLift")) args
  mixedCall args =
    CallExpr noPos (VarRef noPos (Id noPos "compilerCall")) args
  mixedIf test cons altr =
    CallExpr noPos (VarRef noPos (Id noPos "compilerIf"))
             [test,cons,altr]

  unbehavior id = CallExpr noPos (VarRef noPos (Id noPos "compilerUnbehavior"))
                    [id]

  liftM e@(VarRef _ id) = case S.member (unId id) fxenv of
    False -> return (unbehavior e)
    True  -> return e 
  -- obj[prop]:
  --   mixedLift_eb(function(obj,prop) { return obj[prop]; },obj,prop);
  liftM (BracketRef p container key) =
    let [containerId,keyId] = mkIds p [container,key]
        lift = J.func [containerId,keyId]
                 (J.return (J.bracket (J.ref containerId) (J.ref keyId)))
      in do container' <- liftM container
            key'       <- liftM key
            return $ mixedLift [lift,container',key']
  -- { prop: val, ... }:
  --   { prop: mixedLift_eb(val), ... }
  liftM (ObjectLit p props) = do
    props' <- mapM (\(name,val) -> do val' <- liftM val
                                      return (name,val'))
                   props
    return (ObjectLit p props') 
  -- obj.prop:
  --   mixedLift_eb(function(obj) { return obj.prop; },obj);
  liftM (DotRef p obj prop) =
    let [objId] = mkIds p [obj]
        lift = J.func [objId] (J.return (J.ref objId `J.dot` prop))
      in do obj' <- liftM obj
            return $ mixedLift [lift,obj']
  -- new obj(arg ...):
  --   mixedLift_eb(function(obj,arg ...) { return new obj(arg ...); },
  --                obj, arg ...)
  liftM (NewExpr p obj args) =
    let (objId:argIds) = mkIds p (obj:args)
        lift = J.func (objId:argIds)
                 (J.return $ J.new (J.ref objId) (map J.ref argIds))
      in do obj'  <- liftM obj
            args' <- mapM liftM args
            return $ mixedLift (lift:obj':args')
  -- All postfix operators are impure.
  liftM e@(PostfixExpr p _ _) = do
    warn "unlifted postfix operator" e p
    return e
  -- Some prefix operators are impure.  For those that are not
  --   op(arg): 
  --     mixedLift_eb(function(arg) { return op(arg); },arg)
  liftM e@(PrefixExpr p op expr) =
    if isPurePrefixOperator op
      then let [argId] = mkIds p [expr]
               lift = J.func [argId]
                        (J.return $ J.prefix op (J.ref argId))
             in do expr' <- liftM expr
                   return $ mixedLift [lift,expr']
      else warn "unlifted prefix operator" e p >> return e
  -- Strange treatment for assignment
  liftM e@(AssignExpr p op left right) = do
    if isPureAssignOperator op
      then do left <- liftM left
              right <- liftM right
              return (AssignExpr p op left right)
      else warn "unlifted assignment operator" e p >> return e
  -- test ? cons : altr :
  --   mixedIf_eb(test,cons,altr)
  liftM e@(CondExpr p test cons altr) = do
    test' <- liftM test
    cons' <- liftM cons
    altr' <- liftM altr
    return $ mixedIf test' cons' altr'
  -- expr1 op expr2:
  --   mixedLift_eb(function(e1,e2) { return e1 op e2; }, expr1, expr2)
  liftM (InfixExpr p op e1 e2) =
    let [e1Id,e2Id] = mkIds p [e1,e2]
        lift = J.func [e1Id,e2Id] 
                 (J.return $ J.inf op (J.ref e1Id) (J.ref e2Id))
      in do e1' <- liftM e1
            e2' <- liftM e2
            return $ mixedLift [lift,e1',e2']
  -- funcId(arg ...):
  --   mixedCall_eb(f,arg ...)
  liftM e@(CallExpr p f@(VarRef _ id) args)
    | isUnliftedFunction id = do
        args' <- mapM liftM args
        return $ CallExpr p f args'
    | otherwise = do 
        unless (S.member (unId id) fxenv) $
        (f':args') <- mapM liftM (f:args)
        return $ mixedCall (f':args')
  -- obj.method(arg0, arg1, ...):
  --   mixedLift_eb(function(obj,arg0,arg1, ...)
  --                  { return obj.method(arg0, arg1, ...); },
  --                obj, arg0, arg1, ....);
  liftM (CallExpr p ref@(DotRef p' obj method) args) =
    if isUnliftedMethod method
      then do (obj':args') <-  mapM liftM (obj:args)
              return $ CallExpr p (DotRef p' obj' method) args'
      else let (objId:argIds) = mkIds p (obj:args)
               lift = J.func (objId:argIds)
                        (J.return
                          (J.call (J.dot (J.ref objId) method)
                                  (map J.ref argIds)))
             in do (obj':args') <- mapM liftM (obj:args)
                   return $ mixedLift (lift:obj':args')
  -- func-expr(arg ...): (indirect call)
  --   mixedCall_eb(func-expr,arg ...)
  liftM (CallExpr p f args) = do
    (f':args') <- mapM liftM (f:args)
    return $ mixedCall (f':args')
  -- mutual-recursion to lift statements in expressions
  liftM (FuncExpr p ids stmt) = do
    let fxenv' = S.unions [localVars [stmt],S.fromList $ map unId ids,fxenv]
    stmt' <- liftStmtM fxenv' opts stmt
    return $ FuncExpr p ids stmt'
  -- lifting a list of expressions
  liftM (ListExpr p exprs) = do
    exprs' <- mapM liftM exprs
    return $ ListExpr p exprs'
  -- all other expressions are either atomic or have other expressions as
  -- sub-expressions
  liftM e = do
    gmapM (mkM liftM) e

liftVarDecl fxenv opts decl = case decl of
  VarDecl _ _ Nothing -> return decl
  VarDecl p id (Just e) -> do
    e' <- liftExprM fxenv opts e
    return (VarDecl p id (Just e'))

liftStmtM :: S.Set String
          -> CompilerOpts -> ParsedStatement -> IO ParsedStatement
liftStmtM fxenv opts stmt = case stmt of
  ReturnStmt p (Just expr) -> do
    expr' <- liftExprM fxenv opts expr
    return (ReturnStmt p (Just expr'))
  BlockStmt p stmts -> do
    stmts' <- mapM (liftStmtM fxenv opts) stmts
    return (BlockStmt p stmts')
  VarDeclStmt p decls -> do
    decls' <- mapM (liftVarDecl fxenv opts) decls
    return (VarDeclStmt p decls')
  otherwise -> do
    -- Warn for impurities in this statement.
    stmt <- warnImpureStmt stmt
    -- Apply liftExprM to expressions in statements, without recursively
    -- descending into expressions.  liftExprM is explicitly-recursive.
    stmt <- gmapM (mkM (liftExprM fxenv opts)) stmt
    -- recurse to immediate sub-statements
    gmapM (mkM (liftStmtM fxenv opts)) stmt
  
--}}}


compileStatement :: S.Set String 
                 -> CompilerOpts -> ParsedStatement -> IO ParsedStatement
compileStatement fxenv opts stmt = do
  stmt <- fixScopingM stmt
  stmt <- liftStmtM fxenv opts stmt
  -- stmt <- qualifyKeywordsM opts stmt
  return stmt

-- loaderArray.push(name)
pushLoader:: SourcePos -> CompilerOpts -> Id SourcePos -> ParsedStatement
pushLoader p opts name =
  let loaderArr = VarRef p (Id p (flapjaxLoader opts))
      loader = VarRef p name
    in  ExprStmt p (CallExpr p (DotRef p loaderArr (Id p "push")) [loader])

-- function loader() { statements ... }
wrapScriptBlock:: SourcePos -> Id SourcePos -> [ParsedStatement] 
               -> ParsedStatement
wrapScriptBlock pos loader statements =
  FunctionStmt pos loader [] (BlockStmt pos statements)

-- Compiles a single <script lang="flapjax">...</script> script
compile :: S.Set String
        -> IORef Int -> CompilerOpts -> Flapjax -> IO Flapjax
compile fxenv refUID opts (FlapjaxScript pos statements) = do
  -- Compile each statement
  statements <- mapM (compileStatement fxenv opts) statements
  -- Generate the name of the loading function
  uid <- readIORef refUID
  thisLoader <- return $ Id pos (flapjaxLoader opts ++ show uid)
  -- The returned script contains the wrapped code and pushes the loader into
  -- the loader array.
  writeIORef refUID (uid+1)
  return $ FlapjaxScript pos 
                         [wrapScriptBlock pos thisLoader statements,
                          pushLoader pos opts thisLoader]
compile fxenv refUID opts other = 
  return other

-- As an artifact of the parse tree, we have to change lang="flapjax" tags to
-- lang="javascript" tags in a seperate pass.
flapjaxTagsToJavascript:: Html -> Html
flapjaxTagsToJavascript (Html.Element "script" attrs children pos) =
  let toJavascript v = 
        if (v == "flapjax" || v == "text/flapjax")
          then "text/javascript"
          else v
      attrs' = Html.attributeUpdate "type" toJavascript
                 (Html.attributeUpdate "lang" toJavascript attrs)
    in Html.Element "script" attrs' children pos
flapjaxTagsToJavascript other =
  other

compileScripts:: CompilerOpts -> Html -> IO Html
compileScripts opts page = do
  refUID <- newIORef 0
  -- top-level names defined in "text/flapjax" scripts
  let fxenv = fxScriptGlobalEnv page
  page <- everywhereM (mkM (compile fxenv refUID opts)) page
  return $ everywhere (mkT flapjaxTagsToJavascript) page

--}}}

-- ----------------------------------------------------------------------------
-- Names in Flapjax scripts

fxScriptGlobalEnv :: Html -> S.Set String
fxScriptGlobalEnv html = everything S.union (mkQ S.empty getEnv) html where
  getEnv :: Flapjax -> S.Set String
  getEnv (FlapjaxScript _ stmts) = localVars stmts
  getEnv (Javascript _) = S.empty
  getEnv (Inline _ _) = S.empty -- cannot declare global variables
  getEnv (InlineAttribute _ _) = S.empty -- as above


--------------------------------------------------------------------------------
--{{{ Whole-page pass: add initialization

-- TODO: no guarantee that head exists and certainly no guarantee that multiple
-- head elements exist
installInit:: CompilerOpts -> Html -> IO Html
installInit compilerOpts (Html.Element "head" attributes children p) =
  let script = Html.Element "script" 
                            [Html.Attribute "lang" "text/javascript" p] 
                            [Html.Script (Javascript js) p] p
      flapjaxSrc = flapjaxPath compilerOpts
      fjScript = Html.Element "script"
                              [Html.Attribute "lang" "text/javascript" p,
                               Html.Attribute "src" flapjaxSrc p]
                               [] p
      fj = Id noPos (flapjaxObject compilerOpts)
      loader = Id noPos (flapjaxLoader compilerOpts)
      js = Script p [VarDeclStmt p [VarDecl p loader (Just $ ArrayLit p [])]]
    in do return $ Html.Element "head" attributes (fjScript:script:children) p
installInit _ e = return e

addInitCode:: CompilerOpts -> Html -> IO Html
addInitCode opts page = do
  everywhereM (mkM (installInit opts)) page

--}}}

--------------------------------------------------------------------------------
--{{{ Whole-page pass: add loading code

-- Adds the loading code to the body element
-- TODO: Optimize for efficiency
addLoaderToBody:: CompilerOpts -> Html -> IO Html
addLoaderToBody opts (Html.Element "body" attrs children p) =
  let loader = flapjaxLoader opts
      flapjax = flapjaxObject opts
      code = "forEach(function(l){ l(); }, " ++ loader ++ ")"
      attrs' = Html.attributeUpdate -- does not clobber existing loading code
                 "onload" (\c -> if c == "" then code else c ++ "; " ++ code)
                           -- c is the existing content of the onload attribute
                 attrs
    in return $ Html.Element "body" attrs' children p
addLoaderToBody _ e = 
  return e

addLoader:: CompilerOpts -> Html -> IO Html
addLoader opts page = do
  everywhereM (mkM (addLoaderToBody opts)) page

--}}}

--------------------------------------------------------------------------------
--{{{ Compiler: Standalone compiler

compileStandalone:: CompilerOpts -> Maybe String -> Flapjax -> Compiler Flapjax
compileStandalone opts maybeLoader fx = do
  opts <- avoidNameClashes opts (Html.Script fx noPos) -- inclusion
  opts <- case maybeLoader of
            Nothing -> return opts
            (Just l) -> return $ opts { flapjaxLoader = l }
  refUID <- lift (newIORef 0)
  fx <- lift (compile S.empty refUID opts fx)
  return fx
  
--}}}

--------------------------------------------------------------------------------
--{{{ Compiler: Whole-page compiler


compilePage:: CompilerOpts -> Html -> Compiler Html
compilePage opts page = do
  opts <- avoidNameClashes opts page
  page <- compileInline opts page
  page <- lift $ compileInlineAtAttribs opts page
  page <- lift $ compileScripts opts page
  page <- lift $ addInitCode opts page
  page <- lift $ addLoader opts page
  return page

--}}}

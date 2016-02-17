-- | A lexical environment analysis of ECMAScript programs

module Language.ECMAScript5.Analysis.LexicalEnvironment
  ( env
  , localVars
  , EnvTree (..)
  ) where

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Text.ParserCombinators.Parsec.Pos (SourcePos)
import Data.Monoid

import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Parser (SourceSpan (..))

-- | Intermediate data structure that contains locally declared names and
-- all references to identifers.
data Partial = Partial {
  partialLocals :: M.Map String SourceSpan,
  partialReferences :: M.Map String SourceSpan,
  partialNested :: [Partial]
}

instance Monoid Partial where
  mempty = Partial M.empty M.empty []
  mappend p1 p2 = p1 {partialLocals = partialLocals p1 `M.union` partialLocals p2
                     ,partialReferences = partialReferences p1 `M.union` partialReferences p2
                     ,partialNested = partialNested p1 ++ partialNested p2}

empty = mempty

-- | Combine partial results from the same lexical scope.
unions :: [Partial] -> Partial
unions = mconcat

  
ref :: Id SourceSpan -> Partial
ref (Id p v) = Partial M.empty (M.singleton v p) []

decl :: Id SourceSpan -> Partial
decl (Id p v) = Partial (M.singleton v p) M.empty []

nest :: Partial -> Partial
nest partial = Partial M.empty M.empty [partial]


javascript :: Program SourceSpan -> Partial
javascript (Program _ ss) = unions (map stmt ss)


expr :: Expression SourceSpan -> Partial
expr e = case e of
  StringLit _ _ -> empty
  RegexpLit {} -> empty
  NumLit _ _ -> empty
  BoolLit _ _ -> empty
  NullLit _ -> empty
  ArrayLit _ es -> unions $ catMaybes $ (map (fmap expr) es)
  ObjectLit _ props -> unions (map prop props)
  ThisRef _ -> empty
  VarRef _ id -> empty
  DotRef _ e _ -> expr e
  BracketRef _ e1 e2 -> unions [expr e1, expr e2]
  NewExpr _ e1 es -> unions [expr e1, unions $ map expr es]
  PrefixExpr _ _ e -> expr e
  InfixExpr _ _ e1 e2 -> unions [expr e1, expr e2]
  CondExpr _ e1 e2 e3 -> unions [expr e1, expr e2, expr e3]
  AssignExpr _ _ lv e -> unions [expr lv, expr e]
  UnaryAssignExpr _ _ lv -> expr lv
  CommaExpr _ es -> unions (map expr es)
  CallExpr _ e es -> unions [expr e, unions $ map expr es]
  FuncExpr _ _ args ss -> nest $ unions [unions $ map decl args
                                        ,unions $ map stmt ss]

prop :: PropAssign SourceSpan -> Partial
prop p = case p of
  PValue _ _ e -> expr e
  PGet _ _ body -> unions $ map stmt body
  PSet _ _ _ body -> unions $ map stmt body

caseClause :: CaseClause SourceSpan -> Partial
caseClause cc = case cc of
  CaseClause _ e ss -> unions [expr e, unions $ map stmt ss]
  CaseDefault _ ss -> unions $ map stmt ss

-- TODO: Verify that this is a declaration and not a reference.
catchClause :: CatchClause SourceSpan -> Partial
catchClause (CatchClause _ id s) = unions $ (decl id):map stmt s

varDecl :: VarDecl SourceSpan -> Partial
varDecl (VarDecl _ id Nothing) = decl id
varDecl (VarDecl _ id (Just e)) = unions [decl id, expr e]
 
forInit :: ForInit SourceSpan -> Partial
forInit fi = case fi of
  NoInit -> empty
  VarInit ds -> unions $ map varDecl ds
  ExprInit e -> expr e 

forInInit :: ForInInit SourceSpan -> Partial
forInInit (ForInVar d) = varDecl d
forInInit (ForInExpr e) =  expr e
  
stmt :: Statement SourceSpan -> Partial
stmt s = case s of
  BlockStmt _ ss -> unions $ map stmt ss
  EmptyStmt _ -> empty
  ExprStmt _ e -> expr e
  IfStmt _ e s1 s2 -> unions [expr e, stmt s1, stmt s2]
  SwitchStmt _ e cases -> unions [expr e, unions $ map caseClause cases]
  WhileStmt _ e s -> unions [expr e, stmt s]
  DoWhileStmt _ s e -> unions [stmt s, expr e]
  BreakStmt _ _ -> empty
  ContinueStmt _ _ -> empty
  LabelledStmt _ _ s -> stmt s
  ForInStmt _ fii e s -> unions [forInInit fii, expr e, stmt s]
  ForStmt _ fi  me1 me2 s -> 
    unions [forInit fi, maybe empty expr me1, maybe empty expr me2, stmt s]
  TryStmt _ s mcatch ms ->
    unions $ catMaybes $ (catchClause <$> mcatch):(fmap (unions . map stmt) ms):(map (Just . stmt) s)
  ThrowStmt _ e -> expr e
  ReturnStmt _ me -> maybe empty expr me
  WithStmt _ e s -> unions [expr e, stmt s]
  VarDeclStmt _ decls -> unions $ map varDecl decls
  FunctionStmt _ fnId args ss ->
    unions [decl fnId, nest $ unions [unions $ map decl args,
                                      unions $ map stmt ss]]

-- |The statically-determinate lexical structure of a JavaScript program.
data EnvTree = EnvTree (M.Map String SourceSpan) [EnvTree]

-- A 'Partial' specifies identifier references in addition to identifier
-- declarations.  We descend into a 'Partial', pushing enclosing declarations
-- in to remove references to identifiers declared in the enclosing scope.
-- Any referencs to identifiers not declared in either the current or the
-- enclosing scope are local definitions of global variables.
makeEnvTree :: Map String SourceSpan -- ^enclosing environment
            -> Partial -- ^local environment and references
            -> (EnvTree,Map String SourceSpan) 
            -- ^environment and global definitions
makeEnvTree enclosing (Partial locals references nested) = (tree,globals) where
  nestedResults = map (makeEnvTree (locals `M.union` enclosing)) nested
  tree = EnvTree locals (map fst nestedResults)
  globals' = (references `M.difference` locals) `M.difference` enclosing
  globals = M.unions (globals':map snd nestedResults)

env :: Map String SourceSpan -- ^browser/testing environment
    -> [Statement SourceSpan] 
    -> (EnvTree,Map String SourceSpan)
env globals program = makeEnvTree globals (unions $ map stmt program)


localVars :: [Statement SourceSpan]
          -> [(String, SourceSpan)]
localVars body = M.toList locals where
  Partial locals _ _ = unions $ map stmt body


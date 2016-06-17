-- | Label-set analysis which annotates all the statements in the script
-- with their label sets according to ECMAScript specification,
-- section 12.12. The result of this analysis are useful for building
-- control-flow graphs.

module Language.ECMAScript5.Analysis.LabelSet (annotateLabelSets 
                                              ,Label(..)) where

import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Annotations
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Generics.Uniplate.Data
import Data.Data (Data)
import Control.Applicative
import Data.Typeable (Typeable)
import Lens.Simple

-- | Labels are either strings (identifiers) or /empty/ (see 12.12 of
-- the spec)
data Label = Label String
           | EmptyLabel
             deriving (Ord, Eq, Data, Typeable)

instance Show Label where
  show (Label s) = s
  show EmptyLabel = ""

-- | Annotates statements with their label sets. Assuming you've also imported
-- 'Language.ECMAScript5.Syntax.Annotations', 'Control.Arrow' and
-- 'Data.Set', the example use would be:
--
-- @
-- lsAnalysis :: 'Program' a -> 'Program' (a, 'Set' 'Label')  
-- lsAnalysis = annotateLabelSets snd (\\labs -> second $ union labs) . reannotate (\a -> (a, empty))
-- @
annotateLabelSets :: Data a =>
                     (a -> Set Label) -- ^ annotation read function
                  -> (Set Label -> a -> a) -- ^ annotation write function
                  -> Program a  -- ^ the program to analyse
                  -> Program a
annotateLabelSets r w = transformBi (annotateFuncStmtBodies r w)
                      . transformBi (annotateFuncExprBodies r w)
                      . descendBi   (annotateStatement r w)

annotateFuncStmtBodies :: Data a => 
                          (a -> Set Label)
                       -> (Set Label -> a -> a) 
                       -> Statement a
                       -> Statement a
annotateFuncStmtBodies r w s = case s of
  FunctionStmt a name params body -> 
    let newbody = map (descend (annotateStatement r w)) body
    in  FunctionStmt a name params newbody
  _ -> s
                       
annotateFuncExprBodies :: Data a => 
                          (a -> Set Label)
                       -> (Set Label -> a -> a) 
                       -> Expression a 
                       -> Expression a
annotateFuncExprBodies r w e = case e of
  FuncExpr a mname params body -> 
    let newbody = map (descend (annotateStatement r w)) body
    in  FuncExpr a mname params newbody
  _ -> e

-- | 12.12 ECMA262: the production /Identifier/ : /Statement/ is
-- evaluated by adding /Identifier/ to the label ser of /Statement/
-- and then evluating /Statement/. If the /LabelledStatement/ itsef
-- has a non-empty label set, these labels are also added to the label
-- set of /Statement/ before evaluating it. ... Prior to evaluation of
-- a /LabelledStatement/, the contained /Statement/ is regarded as
-- possessing an empty label set, unless it is an /IterationStatement/
-- or a /SwitchStatement/, in which case it is regarded as possessing
-- a label set consisting of the single element, @empty@.
annotateStatement :: Data a => 
                     (a -> Set Label)
                  -> (Set Label -> a -> a)
                  -> Statement a   
                  -> Statement a
annotateStatement r w s = case s of
  LabelledStmt ann lab stmt -> 
    let labelset = Set.insert (id2Label lab) (r ann) 
        newstmt  = annotateStatement r w $ stmt&annotation %~ w labelset
    in  LabelledStmt ann lab newstmt
  SwitchStmt {} -> 
    let labelset = Set.insert EmptyLabel (r $ getAnnotation s)
    in  descend (annotateStatement r w) $ s&annotation %~ w labelset
  _ | isIterationStmt s ->
    let labelset = Set.insert EmptyLabel (r $ getAnnotation s)
    in  descend (annotateStatement r w) (s&annotation %~ w labelset)
  _                     -> descend (annotateStatement r w) s

id2Label :: Id a -> Label
id2Label = Label . unId
                                

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE FlexibleContexts    #-}

module Language.Fortran.Analysis.Types.Traverse where

import Prelude hiding ( EQ, LT, GT )

import           Language.Fortran.AST
import           Language.Fortran.Analysis
import           Language.Fortran.Analysis.Types.Internal
import           Language.Fortran.Analysis.Types.Util
import qualified Language.Fortran.Analysis.Types.Derive     as Derive
import           Language.Fortran.Analysis.Parameters
import           Language.Fortran.Repr.Type
import           Language.Fortran.Repr.Value
import           Language.Fortran.Intrinsics
import           Language.Fortran.Util.Position
import           Language.Fortran.Version
import           Language.Fortran.Parser.Utils

import           Data.Data
import           Data.Generics.Uniplate.Data
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import qualified Data.Map as Map
import           Data.Map ( Map )
import           Data.Maybe (maybeToList)
import           Data.List (find, foldl')

-- | Annotate AST nodes with type information and also return a type
-- environment mapping names to type information.
analyseTypes :: Data a => ProgramFile (Analysis a) -> (ProgramFile (Analysis a), TypeEnv)
analyseTypes = analyseTypesWithEnv Map.empty

-- | Annotate AST nodes with type information and also return a type
-- environment mapping names to type information; provided with a
-- starting type environment.
analyseTypesWithEnv :: Data a => TypeEnv -> ProgramFile (Analysis a) -> (ProgramFile (Analysis a), TypeEnv)
analyseTypesWithEnv env pf = (pf', tenv)
  where
    (pf', endState) = analyseTypesWithEnv' env pf
    tenv            = environ endState

-- | Annotate AST nodes with type information, return a type
-- environment mapping names to type information and return any type
-- errors found; provided with a starting type environment.
analyseAndCheckTypesWithEnv
  :: Data a => TypeEnv -> ProgramFile (Analysis a) -> (ProgramFile (Analysis a), TypeEnv, [TypeError])
analyseAndCheckTypesWithEnv env pf = (pf', tenv, terrs)
  where
    (pf', endState) = analyseTypesWithEnv' env pf
    tenv            = environ endState
    terrs           = typeErrors endState

analyseTypesWithEnv' :: Data a => TypeEnv -> ProgramFile (Analysis a) -> (ProgramFile (Analysis a), InferState)
analyseTypesWithEnv' env pf@(ProgramFile mi _) = runInfer (miVersion mi) env $ do
  -- Do const work
  modify $ \s -> s { constMap = gatherConsts pf }

  -- Gather information.
  mapM_ intrinsicsExp (allExpressions pf)
  mapM_ programUnit (allProgramUnits pf)
  --mapM_ recordArrayDeclarator (allDeclarators pf)
  mapM_ statement (allStatements pf)

  -- Gather types for known entry points.
  eps <- gets (Map.toList . entryPoints)
  forM_ eps $ \ (eName, (fName, mRetName)) -> do
    mFType <- getRecordedType fName
    case mFType of
      Just (IDType fVType fCType) -> do
        recordMType fVType fCType eName
        -- FIXME: what about functions that return arrays?
        maybe (return ()) (error "Entry points with result variables unsupported" >> recordMType fVType Nothing) mRetName
      _                           -> return ()

  annotateTypes pf              -- Annotate AST nodes with their types.

-- TODO here, we should parse values into a Repr.Value type, allowing us to
-- check for initial safety. we can't put them into the parameter map, but we
-- should be able to store their validated/strong repr value in the annotation!
annotateExpression :: Data a => Expression (Analysis a) -> Infer (Expression (Analysis a))
annotateExpression = undefined

annotateProgramUnit :: Data a => ProgramUnit (Analysis a) -> Infer (ProgramUnit (Analysis a))
annotateProgramUnit pu | Named n <- puName pu = maybe pu (`setIDType` pu) `fmap` getRecordedType n
annotateProgramUnit pu                        = return pu

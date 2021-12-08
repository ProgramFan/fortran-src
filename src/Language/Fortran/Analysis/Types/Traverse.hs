{-# LANGUAGE FlexibleContexts #-}

module Language.Fortran.Analysis.Types.Traverse where

import           Language.Fortran.AST
import           Language.Fortran.Util.Position
import           Language.Fortran.Analysis
import           Language.Fortran.Analysis.Util
import           Language.Fortran.Analysis.Types.Util
import           Language.Fortran.Analysis.Types.Internal
import qualified Language.Fortran.Analysis.Types.Derive     as Derive
import           Language.Fortran.Repr.Type

import           Data.Data
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import qualified Data.Map               as Map

programUnit
    :: (MonadState InferState m, MonadReader InferConfig m, Data a)
    => ProgramUnit (Analysis a)
    -> m ()
programUnit pu@(PUFunction _ _ mRetType _ _ _ mRetVar blocks _)
  | Named n <- puName pu   = do
    -- record some type information that we can glean
    recordCType CTFunction n
    case (mRetType, mRetVar) of
      (Just ts@(TypeSpec _ _ _ _), Just v) -> do
        tryDerive Derive.fromTypeSpec ts $ \ft -> do
          recordScalarType n           ft
          recordScalarType (varName v) ft
      (Just ts@(TypeSpec _ _ _ _), _)      -> do
        tryDerive Derive.fromTypeSpec ts $ \ft -> do
          recordScalarType n ft
      _                                        -> return ()
    -- record entry points for later annotation
    forM_ blocks $ \ block ->
      sequence_ [ recordEntryPoint n (varName v) (fmap varName mRetVar') | (StEntry _ _ v _ mRetVar') <- allStatements block ]
programUnit pu@(PUSubroutine _ _ _ _ _ blocks _) | Named n <- puName pu = do
  -- record the fact that this is a subroutine
  recordCType CTSubroutine n
  -- record entry points for later annotation
  forM_ blocks $ \ block ->
    sequence_ [ recordEntryPoint n (varName v) Nothing | (StEntry _ _ v _ _) <- allStatements block ]
programUnit _                                           = return ()

declarator :: (MonadState InferState m, Data a) => Declarator (Analysis a) -> m ()
declarator (Declarator _ _ v (Just ddAList) _ _) =
    recordCType (CTArray $ dimDeclarator ddAList) (varName v)
declarator _ = return ()

-- TODO const eval them, don't limit to ints!!
dimDeclarator :: AList DimensionDeclarator a -> [(Maybe Int, Maybe Int)]
dimDeclarator ddAList = [ (lb, ub) | DimensionDeclarator _ _ lbExp ubExp <- aStrip ddAList
                                   , let lb = do ExpValue _ _ (ValInteger i _) <- lbExp
                                                 return $ read i
                                   , let ub = do ExpValue _ _ (ValInteger i _) <- ubExp
                                                 return $ read i ]

--------------------------------------------------------------------------------

recordScalarType :: MonadState InferState m => Name -> FTypeScalar -> m ()
recordScalarType n ft =
    modify $ \s -> s { environ = Map.alter changeFunc n (environ s) }
  where changeFunc mIDType = Just (IDType (Just ft) (mIDType >>= idCType))

tryDerive
    :: (MonadState InferState m, Spanned a)
    => (a -> m (Either Derive.Error FTypeScalar))
    -> a
    -> (FTypeScalar -> m ())
    -> m ()
tryDerive fDerive a useType =
    fDerive a >>= \case
      Left err -> typeError ("error while deriving a type: " <> show err) (getSpan a)
      Right ft -> useType ft

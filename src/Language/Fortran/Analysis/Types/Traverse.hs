{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Fortran.Analysis.Types.Traverse where

import           Language.Fortran.AST
import           Language.Fortran.Util.Position
import           Language.Fortran.Analysis
import           Language.Fortran.Analysis.Util
import           Language.Fortran.Analysis.Types.Util
import           Language.Fortran.Analysis.Types.Internal
import qualified Language.Fortran.Analysis.Types.Derive     as Derive
import           Language.Fortran.Repr.Type
import           Language.Fortran.Intrinsics

import           Data.Data
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import qualified Data.Map                                   as Map
import qualified Data.List                                  as List

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

intrinsicsExp
    :: (MonadState InferState m, MonadReader InferConfig m, Data a)
    => Expression (Analysis a) -> m ()
intrinsicsExp = \case
  ExpSubscript _ _ nexp _    -> go nexp
  ExpFunctionCall _ _ nexp _ -> go nexp
  _ -> return ()
  where
    go nexp | isNamedExpression nexp = do
      itab <- asks inferConfigIntrinsics
      case getIntrinsicReturnType (srcName nexp) itab of
        Just _ -> do
          let n = varName nexp
          recordCType CTIntrinsic n
          -- recordBaseType _  n -- FIXME: going to skip base types for the moment
        _             -> return ()
    go _ = return ()

statement :: (MonadState InferState m, MonadReader InferConfig m, Data a) => Statement (Analysis a) -> m ()
statement (StDeclaration _ _ ts mAttrAList declAList) = do
  decls <- handleDeclaration ts (aStrip' mAttrAList) (aStrip declAList)
  forM_ decls $ uncurry recordType'
statement (StExternal _ _ varAList) = do
  let vars = aStrip varAList
  mapM_ (recordCType CTExternal . varName) vars
statement (StExpressionAssign _ _ (ExpSubscript _ _ v ixAList) _)
  --  | any (not . isIxSingle) (aStrip ixAList) = recordCType CTArray (varName v)  -- it's an array (or a string?) FIXME
  | all isIxSingle (aStrip ixAList) = do
    mIDType <- getExprRecordedType v
    case mIDType of
      Just (IDType _ (Just CTArray{})) -> return ()                -- do nothing, it's already known to be an array
      _                                -> recordCType CTFunction (varName v) -- assume it's a function statement

-- FIXME: if StFunctions can only be identified after types analysis
-- is complete and disambiguation is performed, then how do we get
-- them in the first place? (iterate until fixed point?)
statement (StFunction _ _ v _ _) = recordCType CTFunction (varName v)
-- (part of answer to above is) nullary function statement: foo() = ...
statement (StExpressionAssign _ _ (ExpFunctionCall _ _ v Nothing) _) = recordCType CTFunction (varName v)

statement (StDimension _ _ declAList) = do
  let decls = aStrip declAList
  forM_ decls $ \ decl -> case decl of
    Declarator _ _ v (Just ddAList) _ _ ->
      recordCType (CTArray $ dimDeclarator ddAList) (varName v)
    _                           -> return ()

statement (StStructure _ _ mName itemAList) = handleStructure mName itemAList

statement _ = return ()

-- | Auxiliary function for getting semantic and construct type of a
--   declaration. Used in standard declarations and structures.
--
-- Returns a list of names to their derived 'IDType'. Variables that errored
-- during scalar type derivation have the relevant 'IDType' field left blank.
--
-- TODO report if given dimension info on LHS and RHS
-- TODO handle params... agh...
handleDeclaration
    :: (MonadState InferState m, MonadReader InferConfig m, Data a)
    => TypeSpec (Analysis a)
    -> [Attribute (Analysis a)]
    -> [Declarator (Analysis a)]
    -> m [(Name, IDType)]
handleDeclaration = undefined
--handleDeclaration ts attrs decls = tryDeriveInitialCType >>= _
{-
      Just ct ->
      Nothing ->
    let mct | any isAttrExternal attrs = return $ Just CTExternal
            | Just (AttrDimension _ _ ddAlist) <- List.find isAttrDimension attrs
    cType <-
        then return CTExternal
        else case List.find isAttrDimension attrs of
               Just (AttrDimension _ _ ddAList) ->
                 return $ CTArray $ dimDeclarator ddAList
               _ ->
                 if   any isAttrParameter attrs
                 then return CTParameter
                 else Nothing getRecordedType n >>= \case 
                | Just (IDType _ (Just ct)) <- Map.lookup n env
                , ct /= CTIntrinsic                           = ct
    let cType n | isExtrn                                     = CTExternal
                | Just (AttrDimension _ _ ddAList) <- attrDim = CTArray (dimDeclarator ddAList)
                | isParam                                     = CTParameter
                | Just (IDType _ (Just ct)) <- Map.lookup n env
                , ct /= CTIntrinsic                           = ct
                | otherwise                                   = CTVariable
        handler rs = \case
          Declarator _ declSs v (Just ddAList) mLenExpr _ -> do
            tryDerive (Derive.fromDeclaration mLenExpr) ts $ \ft -> do
              pure $ (varName v, st, CTArray  $ dimDeclarator ddAList) : rs
          Declarator _ declSs v Nothing mLenExpr _ -> do
            tryDerive (Derive.fromDeclaration mLenExpr) ts $ \ft -> do
              pure $ (varName v, st, cType (varName v)) : rs
    in foldM handler [] decls
-}

-- | Try to derive the scalar type information for a variable and record if
--   successful.
tryRecordScalarType
    :: (MonadState InferState m, MonadReader InferConfig m, Data a)
    => Name -> Maybe (Expression (Analysis a)) -> TypeSpec (Analysis a)
    -> m ()
tryRecordScalarType v mDeclExpr ts = do
    tryDerive (Derive.fromDeclaration mDeclExpr) ts $ recordScalarType v

-- | Create a structure env from the list of fields and add it to the InferState
handleStructure
    :: (MonadState InferState m, MonadReader InferConfig m, Data a)
    => Maybe String -> AList StructureItem (Analysis a) -> m ()
handleStructure mName itemAList = do
  case mName of
    Just n -> do
      structEnv <- foldM handleStructureItem Map.empty (aStrip itemAList)
      recordStruct structEnv n
    Nothing -> pure ()

handleStructureItem
    :: (MonadState InferState m, MonadReader InferConfig m, Data a)
    => StructMemberTypeEnv -> StructureItem (Analysis a) -> m StructMemberTypeEnv
handleStructureItem mt (StructFields _ _ ts mAttrAList declAList) = do
  ds <- handleDeclaration ts (aStrip' mAttrAList) (aStrip declAList)
  pure $ List.foldl' (\m (n, idt) -> Map.insert n idt m) mt ds
-- TODO: These should eventually be implemented
handleStructureItem mt StructUnion{} = pure mt
handleStructureItem mt StructStructure{} = pure mt

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

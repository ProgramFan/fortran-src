-- TODO dislike having to use String
-- TODO unlike fortran-src, we're not using each expression's unique label.
-- would be good for efficiency
--
-- SymValMap has to be built up via Statements (parameter declarations,
-- assignments). Then can be used in expressions. fortran-src currently does
-- these the other way round -- unclear if swapping them will impact anything
-- (appears unlikely?)
--
-- The overall approach here is rewriting the fortran-vars eval story (some
-- constructors in SymbolTable, an Eval module) into a classy interface that
-- provides the eval function, which SymbolTable can implement, and we can
-- improve fortran-src to also do similar work.
--
-- F90 ISO spec is great for this. See pg.38.

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Language.Fortran.Analysis.Parameters where

import           Language.Fortran.AST
import           Language.Fortran.Analysis
import           Language.Fortran.Repr.Value
import           Language.Fortran.Repr.Type

import           Data.Data
import           Data.Generics.Uniplate.Operations
import qualified Data.Map                   as Map
import           Data.Map                   ( Map )
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Function              ( on )

type ConstMap = Map Name FValScalar

-- | Gather labeled constants (PARAMETERS).
--
-- The AST must have gone through a variable renaming pass. (That's the only use
-- of 'Analysis a' here.)
--
-- TODO: F90 ISO pg.193 init expr. Their definition appears a little wider than
-- params, but it's pretty much what we're going for?
gatherConsts :: forall a. Data a => ProgramFile (Analysis a) -> ConstMap
gatherConsts = foldr go Map.empty . universeBi
  where
    go :: Statement (Analysis a) -> ConstMap -> ConstMap
    go (StParameter _ _ decls) cm = undefined -- traverse handleParamDecl cm (aStrip decls)
    go (StDeclaration _ _ _ (Just attrs) decls) cm =
        case getAttr (AttrParameter undefined undefined) (aStrip attrs) of
          Just{}  -> undefined -- traverse (handleParamDecl cm) $ (aStrip decls)
          Nothing -> cm
    go _ cm = cm

-- Only checks the constructor, not the fields inside.
getAttr :: Data a => Attribute a -> [Attribute a] -> Maybe (Attribute a)
getAttr aCmp = go
  where go = \case
               []   -> Nothing
               a:as -> if sameConstructor aCmp a then Just a else go as

sameConstructor :: Data a => a -> a -> Bool
sameConstructor = (==) `on` toConstr

handleParamDecl :: Data a => Declarator (Analysis a) -> ConstMap -> Either Error ConstMap
handleParamDecl (Declarator _ _ varExpr mDims _ mInitExpr) cm =
    case mDims of
      Just{}  -> error "impossible parse: array declarator in parameter declarator list"
      Nothing ->
        case mInitExpr of
          Nothing -> error "impossible parse: no init expr in parameter declarator"
          Just initExpr ->
            let var = varName varExpr
                val = evalInitExpr cm initExpr
             in case val of
                  Left err -> Left err
                  Right val' ->
                    case runState (assignConst var val') cm of
                      (Right (), cm') -> Right cm'
                      (Left err, _)   -> Left err

assignConst :: MonadState ConstMap m => Name -> FValScalar -> m (Either Error ())
assignConst var val = do
    cm <- get
    case Map.member var cm of
      True  -> return $ Left $ ErrorReassignedParameter var
      False -> do
        modify $ Map.insert var val
        return $ Right ()

-- | F90 ISO R505 pg.38
--
-- TODO: also must be >0 (+ve, non-zero)
evalKindParam :: ConstMap -> Expression a -> Either Error FValInt
evalKindParam = evalScalarIntInitExpr

-- TODO split this module into 2
data Error
  = ErrorEvalNotAParam Name
  | ErrorEvalUnsupportedExpression
  | ErrorEvalUnsupportedValue
  -- | ErrorEvalWrongType FTypeScalar FValScalar
  -- ^ TODO can't do because FTypeScalar is concrete, I don't want to talk about the rep tag
  | ErrorEvalOther String
  | ErrorReassignedParameter Name

-- | F90 ISO R505 pg.41
--
-- Does *not* handle @CHARACTER(*)@.
evalCharLengthInt :: ConstMap -> Expression a -> Either Error FValInt
evalCharLengthInt = evalScalarIntInitExpr

-- | Used for various things, including kind parameters.
evalScalarIntInitExpr :: ConstMap -> Expression a -> Either Error FValInt
evalScalarIntInitExpr cm e =
    case evalInitExpr cm e of
      Left err                -> Left err
      Right (FValScalarInt i) -> return i
      Right v                 -> Left $ ErrorEvalOther "scalar int init expression was not a value of any integer type"

-- | (F90 ISO R504 pg.38) Limited initialization expression evaluation function.
--   Only handles some some expressions, some types and no intrinsics.
--
--   TODO eval from fortran-vars
evalInitExpr :: ConstMap -> Expression a -> Either Error FValScalar
evalInitExpr cm = \case
  ExpValue _ _ valExpr -> case valExpr of
    ValVariable v -> do
      case Map.lookup v cm of
        Nothing  -> Left $ ErrorEvalNotAParam v
        Just val -> return val
    _ -> evalScalarValue valExpr
  _ -> Left ErrorEvalUnsupportedExpression

-- | Must not be a 'ValVariable'.
evalScalarValue :: Value a -> Either Error FValScalar
evalScalarValue = \case
  ValInteger i mkp -> FValScalarInt <$> evalInt (read i) mkp
  _                -> Left ErrorEvalUnsupportedValue

evalInt :: Integer -> Maybe (Expression a) -> Either Error FValInt
evalInt = undefined

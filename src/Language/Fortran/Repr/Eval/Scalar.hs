{-| Fortran expression evaluation.

TODO
  * inefficient kind defaulting: could go straight to constructor, instead of
    through string to then parse
  * should I be returning a type as well? I mean, I should be computing one.
-}

module Language.Fortran.Repr.Eval.Scalar where

import           Language.Fortran.AST
import           Language.Fortran.AST.Literal.Real
import           Language.Fortran.Repr.Type
import           Language.Fortran.Repr.Type.Scalar
import           Language.Fortran.Repr.Value
import qualified Language.Fortran.Repr.Eval.Op as Op
import           Language.Fortran.Repr.Eval.Op ( Op' )

import qualified Data.Data as Data
import           Data.Data ( Data )
import qualified Data.Map  as Map
import           Data.Map  ( Map )
import qualified Data.Text as Text
import           Data.Text ( Text )
import           Data.Maybe ( fromMaybe )

-- | Immutable expression evaluation environment.
data Env = Env
  { envVars :: Map Name FValScalar
  , envOps  :: Map Name Op'
  }

data Error
  = ErrorVarUndefined Name
  | ErrorNoSuchOp String
  | ErrorUnsupportedExpression String
  | ErrorOpError Op.Error

  | ErrorUnsupportedValue String -- ^ yet-unsupported 'Value' constructor
  | ErrorUnsupportedCommonType String String
  | ErrorUnsupportedBinOp String
  -- ^ No intrinsic with the given name in the intrinsic map. May indicate you
  --   used a non-compile time intrinsic.

  | ErrorNoSuchKindForType String String
  -- ^ The kind provided is invalid for the given kinded scalar type.

  | ErrorNonIntegerKind String
    deriving (Eq, Show)

eval :: Data a => Env -> Expression a -> Either Error FValScalar
eval env = \case
  ExpValue _a _ss ve ->
    case ve of
      ValVariable v -> evalLookup env v

      ValInteger i mkp -> FValScalarInt     <$> evalInt  env i (maybeKP "4" mkp)
      ValReal    r mkp -> FValScalarReal    <$> evalReal env r (maybeKP "4" mkp)
      ValLogical b mkp -> FValScalarLogical <$> evalBool env b (maybeKP "4" mkp)

      ValString s -> return $ FValScalarString $ Text.pack s
      _ -> Left $ ErrorUnsupportedValue $ show $ Data.toConstr ve

  ExpUnary _a _ss uop e -> do
    v <- eval env e
    evalUnaryOp uop v

  ExpBinary _a _ss bop valExpr1 valExpr2 -> do
    res1 <- eval env valExpr1
    res2 <- eval env valExpr2
    evalBinaryOp env bop res1 res2

  ExpFunctionCall _a _ss funcNameExpr args -> do
    let ExpValue _ _ (ValVariable funcName) = funcNameExpr
    evalOp env funcName (map argExtractExpr . aStrip $ fromMaybe (AList undefined undefined []) args)

  e -> Left $ ErrorUnsupportedExpression $ show $ Data.toConstr e

maybeKP :: String -> Maybe (KindParam' a) -> KindParam
maybeKP defKP = \case Nothing                 -> KindParamInt defKP
                      Just (KindParam _ _ kp) -> kp

evalInt :: Env -> String -> KindParam -> Either Error FValInt
evalInt env iStr = \case
  KindParamInt kpIStr -> go kpIStr
  KindParamVar kpV -> evalLookupKind env kpV >>= go
  where
    go x = case parseKindInt x of
             Nothing -> Left $ ErrorNoSuchKindForType x "INTEGER"
             Just k  -> Right $ FValInt k (read iStr)

evalReal :: Env -> RealLit -> KindParam -> Either Error FValReal
evalReal env rl = \case
  KindParamInt kpIStr -> go kpIStr
  KindParamVar kpV -> evalLookupKind env kpV >>= go
  where
    go x = case parseKindReal x of
             Nothing -> Left $ ErrorNoSuchKindForType x "REAL"
             Just k  -> Right $ FValReal k (readRealLit rl)

-- we do all the kind computing, but actually just throw it away (because we
-- don't need it on value level)
evalBool :: Env -> Bool -> KindParam -> Either Error Bool
evalBool env b = \case
  KindParamInt kpIStr -> go kpIStr
  KindParamVar kpV -> evalLookupKind env kpV >>= go
  where
    go x = case parseKindInt x of
             Nothing -> Left $ ErrorNoSuchKindForType x "LOGICAL"
             Just k  -> Right b

-- Note that we do not permit BOZs here, because the syntax you'd have to
-- use is forbidden. You can't assign a BOZ to a variable (it's an untyped
-- compile-time construct), and the kind parameter syntax is limited, so
-- you can't write e.g. @123_x'10'@. (This goes for gfortran, at least.)
--
-- There shouldn't be a problem if you did want to permit them, though.
evalLookupKind :: Env -> Name -> Either Error String
evalLookupKind env kpV =
    evalLookup env kpV >>= \case
      FValScalarInt (FValInt _ kpI) -> return $ show kpI
      val -> Left $ ErrorNonIntegerKind (show val)

evalLookup :: Env -> Name -> Either Error FValScalar
evalLookup env v =
    case Map.lookup v (envVars env) of
      Nothing  -> Left $ ErrorVarUndefined v
      Just val -> Right val

evalUnaryOp :: UnaryOp -> FValScalar -> Either Error FValScalar
evalUnaryOp uop v = undefined

evalBinaryOp :: Env -> BinaryOp -> FValScalar -> FValScalar -> Either Error FValScalar
evalBinaryOp env bop v1 v2 =
    case bop of
      Addition ->
        case Map.lookup "+" (envOps env) of
          Nothing -> Left $ ErrorNoSuchOp "+"
          Just op ->
            case Op.op op [FValScalar v1, FValScalar v2] of
              Left  err -> Left $ ErrorOpError err
              Right res ->
                let FValScalar res' = res in return res'
      _ -> undefined

{-
evalBinaryOp bop (FValScalarInt v1@(FValInt ty1 val1)) (FValScalarInt v2@(FValInt ty2 val2)) =
  case joinType ty1 ty2 of
    Nothing -> Left  $ ErrorUnsupportedCommonType (show ty1) (show ty2)
    Just jt ->
      (FValScalarInt . FValInt jt) <$>
        case bop of
          Addition       -> Right $ val1 + val2
          Multiplication -> Right $ val1 * val2
          Subtraction    -> Right $ val1 - val2
          -- Division is not a homomorphism wrt. representation, so must convert first
          Division       -> Right $ fvalInt (toRuntimeRepr v1) `div`  fvalInt (toRuntimeRepr v2)
          _              -> Left $ ErrorUnsupportedExpression ""

evalBinaryOp _ _ _ = Left $ ErrorUnsupportedValue ""
-}

evalOp :: Env -> Name -> [Expression a] -> Either Error FValScalar
evalOp env op args = undefined

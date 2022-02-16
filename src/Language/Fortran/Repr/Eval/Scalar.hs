-- | Fortran expression evaluation.

module Language.Fortran.Repr.Eval.Scalar where

import           Language.Fortran.AST
import           Language.Fortran.Repr.Type
import           Language.Fortran.Repr.Value

import qualified Data.Data as Data
import           Data.Data ( Data )
import qualified Data.Map  as Map
import           Data.Map  ( Map )

-- | Immutable expression evaluation environment.
data Env = Env
  { envVars       :: Map Name FValScalar
  , envIntrinsics :: Map Name ()
  -- ^ Named intrinsics and their implementations.
  }

data Error
  = ErrorVarUndefined Name
  | ErrorNoSuchIntrinsic String
  | ErrorUnsupportedExpression String

  | ErrorUnsupportedValue String -- ^ yet-unsupported 'Value' constructor
  | ErrorUnsupportedCommonType String String
  | ErrorUnsupportedBinOp String
  -- ^ No intrinsic with the given name in the intrinsic map. May indicate you
  --   used a non-compile time intrinsic.
    deriving (Eq, Show)

eval :: Data a => Env -> Expression a -> Either Error FValScalar
eval env = \case
  ExpValue _ _ ve ->
    case ve of
      ValVariable v ->
        case Map.lookup v (envVars env) of
          Nothing  -> Left $ ErrorVarUndefined v
          Just val -> return val
      _ -> evalValue ve

  ExpBinary _a _s op valExpr1 valExpr2 -> do
    res1 <- eval env valExpr1
    res2 <- eval env valExpr2
    case evalBinaryOp op res1 res2 of
      Left  _   -> Left $ ErrorUnsupportedBinOp $ show op
      Right res -> Right res

  e -> Left $ ErrorUnsupportedExpression $ show $ Data.toConstr e

-- | Helper for evaluating non-variable 'Value's, which don't need the
--   evaluation environment.
evalValue :: Data a => Value a -> Either Error FValScalar
evalValue = \case
  ValInteger i mkp -> FValScalarInt <$> evalInt i mkp
  v                -> Left $ ErrorUnsupportedValue $ show $ Data.toConstr v

evalInt :: String -> Maybe (Expression a) -> Either Error FValInt
evalInt = undefined

evalBinaryOp :: BinaryOp -> FValScalar -> FValScalar -> Either Error FValScalar
evalBinaryOp op (FValScalarInt v1@(FValInt ty1 val1)) (FValScalarInt v2@(FValInt ty2 val2)) =
  case joinType ty1 ty2 of
    Nothing -> Left  $ ErrorUnsupportedCommonType (show ty1) (show ty2)
    Just jt ->
      (FValScalarInt . FValInt jt) <$>
        case op of
          Addition       -> Right $ val1 + val2
          Multiplication -> Right $ val1 * val2
          Subtraction    -> Right $ val1 - val2
          -- Division is not a homomorphism wrt. representation, so must convert first
          Division       -> Right $ fvalInt (toRuntimeRepr v1) `div`  fvalInt (toRuntimeRepr v2)
          _              -> Left $ ErrorUnsupportedExpression ""


evalBinaryOp _ _ _ = Left $ ErrorUnsupportedValue ""

-- | Fortran expression evaluation.

module Language.Fortran.Repr.Eval.Scalar where

import           Language.Fortran.AST
import           Language.Fortran.Repr.Type
import           Language.Fortran.Repr.Type.Scalar
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

  | ErrorNoSuchKindForType String String
  -- ^ The kind provided is invalid for the given kinded scalar type.

  | ErrorNonIntegerKind String
    deriving (Eq, Show)

eval :: Data a => Env -> Expression a -> Either Error FValScalar
eval env = \case
  ExpValue _ _ ve ->
    case ve of
      ValVariable v ->
        case Map.lookup v (envVars env) of
          Nothing  -> Left $ ErrorVarUndefined v
          Just val -> return val
      ValInteger i mkp ->
        -- TODO integer kind param defaulting
        let kp = maybe (KindParamInt "4") (\(KindParam _ _ kp') -> kp') mkp
         in FValScalarInt <$> evalInt env i kp
      _                -> Left $ ErrorUnsupportedValue $ show $ Data.toConstr ve

  ExpBinary _a _s op valExpr1 valExpr2 -> do
    res1 <- eval env valExpr1
    res2 <- eval env valExpr2
    case evalBinaryOp op res1 res2 of
      Left  _   -> Left $ ErrorUnsupportedBinOp $ show op
      Right res -> Right res

  e -> Left $ ErrorUnsupportedExpression $ show $ Data.toConstr e

evalInt :: Env -> String -> KindParam -> Either Error FValInt
evalInt env iStr = \case
  KindParamInt kpIStr -> go kpIStr
  KindParamVar kpV ->
    evalLookup env kpV >>= \case
      -- Note that we do not permit BOZs here, because the syntax you'd have to
      -- use is forbidden. You can't assign a BOZ to a variable (it's an untyped
      -- compile-time construct), and the kind parameter syntax is limited, so
      -- you can't write e.g. @123_x'10'@. (This goes for gfortran, at least.)
      --
      -- There shouldn't be a problem if you did want to permit them, though.
      FValScalarInt (FValInt _ kpI) -> go (show kpI)
      val ->
        Left $ ErrorNonIntegerKind (show val)
  where
    go x = case parseKindInt x of
             Nothing -> Left $ ErrorNoSuchKindForType x "INTEGER"
             Just k  -> Right $ FValInt k (read iStr)

evalLookup :: Env -> Name -> Either Error FValScalar
evalLookup env v =
    case Map.lookup v (envVars env) of
      Nothing  -> Left $ ErrorVarUndefined v
      Just val -> Right val

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

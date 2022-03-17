-- | Fortran expression evaluation.

module Language.Fortran.Repr.Eval where

import           Language.Fortran.AST
import           Language.Fortran.AST.Literal.Real
import           Language.Fortran.Repr.Type.Scalar
import           Language.Fortran.Repr.Value
import           Language.Fortran.Repr.Value.Scalar
import qualified Language.Fortran.Repr.Eval.Op as Op
import           Language.Fortran.Repr.Eval.Op ( Op' )

import qualified Data.Data as Data
import           Data.Data ( Data )
import qualified Data.Map  as Map
import           Data.Map  ( Map )
import qualified Data.Text as Text
import           Data.Maybe ( fromMaybe )

-- | Immutable expression evaluation environment.
data Env = Env
  { envVars :: Map Name FVal
  , envOps  :: Map Name Op'
  }

data Error
  = ErrorVarUndefined Name
  | ErrorUnsupportedOp String
  | ErrorUnsupportedExpression String
  | ErrorOpError Op.Error

  | ErrorUnsupportedValue String -- ^ yet-unsupported 'Value' constructor
  | ErrorUnsupportedCommonType String String

  | ErrorNoSuchKindForType String String
  -- ^ The kind provided is invalid for the given kinded scalar type.

  | ErrorNonIntegerKind String
    deriving (Eq, Show)

-- | Resolve a 'KindParam'' to a typed kind tag using the given function.
--
-- You must also provide a default kind tag to use in the case that there is no
-- kind parameter.
resolveKindParamTo
    :: Env -> String
    -> kindTag -> (String -> Maybe kindTag) -> Maybe (KindParam' a)
    -> Either Error kindTag
resolveKindParamTo env prettyTypeName defKind parseKind = \case
  Nothing                                   -> return defKind
  Just (KindParam _ _ (KindParamInt kpInt)) -> parseKind' kpInt
  Just (KindParam _ _ (KindParamVar kpVar)) -> do
    -- the 'show' here is sensible: kinds are primarily tags => strings, we have
    -- to put them into integers just to do arithmetic. the alternatives are to
    -- 'read' on the other case instead, or provide both parsers
    show <$> evalLookupKind env kpVar >>= parseKind'
  where
    parseKind' kpInt =
        case parseKind kpInt of
          Nothing -> Left $ ErrorNoSuchKindForType kpInt prettyTypeName
          Just k -> return k

eval :: Data a => Env -> Expression a -> Either Error FVal
eval env = \case
  ExpValue _a _ss ve ->
    case ve of
      ValVariable v -> evalLookup env v

      -- kinded numeric literals: resolve kind, possibly bundle with value
      ValInteger i  mkp -> do
        k <- resolveKindParamTo env "INTEGER" FTypeInt4  parseKindInt  mkp
        return $ FValScalar $ FValScalarInt  $ FValInt  k (read i)
      ValReal    rl mkp -> do
        k <- resolveKindParamTo env "REAL"    FTypeReal4 parseKindReal mkp
        return $ FValScalar $ FValScalarReal $ FValReal k (readRealLit rl)
      ValLogical b  mkp -> do
        -- resolve kind param, but throw it away (we store a true 'Bool' for
        -- LOGICALs)
        _ <- resolveKindParamTo env "LOGICAL" FTypeInt4  parseKindInt  mkp
        return $ FValScalar $ FValScalarLogical b

      ValString s -> return $ FValScalar $ FValScalarString $ Text.pack s
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

-- Note that we do not permit BOZs here, because the syntax you'd have to
-- use is forbidden. You can't assign a BOZ to a variable (it's an untyped
-- compile-time construct), and the kind parameter syntax is limited, so
-- you can't write e.g. @123_x'10'@. (This goes for gfortran, at least.)
--
-- There shouldn't be a problem if you did want to permit them, though.
evalLookupKind :: Env -> Name -> Either Error Integer
evalLookupKind env kpV =
    evalLookup env kpV >>= \case
      FValScalar (FValScalarInt (FValInt _ kpI)) -> return kpI
      val -> Left $ ErrorNonIntegerKind (show val)

evalLookup :: Env -> Name -> Either Error FVal
evalLookup env v =
    case Map.lookup v (envVars env) of
      Nothing  -> Left $ ErrorVarUndefined v
      Just val -> Right val

evalUnaryOp :: UnaryOp -> FVal -> Either Error FVal
evalUnaryOp _uop _v = undefined

evalBinaryOp :: Env -> BinaryOp -> FVal -> FVal -> Either Error FVal
evalBinaryOp env bop v1 v2 =
    case bop of
      Addition    -> evalAs "+"
      Subtraction -> evalAs "-"
      _ -> Left $ ErrorUnsupportedOp $ show bop
  where
    evalAs opName =
        case Map.lookup opName (envOps env) of
          Nothing -> Left $ ErrorUnsupportedOp opName
          Just op ->
            case Op.op op [v1, v2] of
              Left  err -> Left $ ErrorOpError err
              Right res -> return res

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

evalOp :: Env -> Name -> [Expression a] -> Either Error FVal
evalOp _env _op _args = undefined

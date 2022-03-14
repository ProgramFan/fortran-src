{-# LANGUAGE KindSignatures, DataKinds, RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Fortran.Repr.Eval.Op where

import Language.Fortran.Repr.Type
import Language.Fortran.Repr.Type.Scalar
import Language.Fortran.Repr.Type.Array
import Language.Fortran.Repr.Value

import GHC.TypeLits
import qualified Data.Vector.Sized as V
import           Data.Vector.Sized ( Vector )
import Control.Monad.Except

data OpType
  = UnaryOp  -- ^ prefix, 1 arg, op style
  | BinaryOp -- ^ infix, 2 args of same type, op style
  | Func Nat -- ^ prefix, n args

-- | Operator of type @ty@, producing values of type @ty@.
data Op ty val = Op
  { op   :: forall m. MonadError Error m => [val] -> m val
  , opTy :: forall m. MonadError Error m => [ty]  -> m ty
  }

type Op' = Op FType FVal

data Error = ErrorStr String
    deriving (Eq, Show)

opPlus :: Op FType FVal
opPlus = Op{..}
  where
    op args = do
        let a1 = args !! 0
            a2 = args !! 1
        case (a1, a2) of
          (FValScalar (FValScalarInt i1), FValScalar (FValScalarInt i2)) -> do
            case fValIntSafeAdd i1 i2 of
              (i, True)   -> return $ FValScalar $ FValScalarInt i
              (_i, False) -> err "plus op over/underflow"
          _ -> err "opPlus: unsupported values"
    opTy args = do
        let a1 = args !! 0
            a2 = args !! 1
        case (a1, a2) of
          (FTypeScalar _a1ts, FTypeArray' _a2ta) -> error "scalar x array"
          (FTypeArray' _a1ta, FTypeScalar _a2ts) -> error "scalar x array"
          (FTypeArray' a1ta, FTypeArray' a2ta) -> do
            case sameShape (fTypeArrayShape a1ta) (fTypeArrayShape a2ta) of
              Nothing -> err "array  x array (same shape)"
              Just e  -> err $ "arrays diff shape: " <> e
          (FTypeScalar a1ts, FTypeScalar a2ts) -> do
            case (a1ts, a2ts) of
              (FTypeScalarInt i1, FTypeScalarInt i2) ->
                return $ FTypeScalar $ FTypeScalarInt $ max i1 i2
              _ -> err "opPlus: unsupported types"
    err :: forall a m. MonadError Error m => String -> m a
    err = throwError . ErrorStr

sameShape :: ArrayShape -> ArrayShape -> Maybe String
sameShape _arr1 _arr2 = Just "sameShape: not yet implemented"

{-
data Fixity
  = Prefix -- ^ functions ("non-symbolic" operators?), unary operators
  | Infix  -- ^ only makes sense for binary operators

-- 'a' is type of argument
data Op (s :: Symbol) (n :: Nat) (x :: Fixity) a = Op
  { opName :: Text
  , ???

_ :: MonadError Error m => Op f n a -> [a] -> m a

data Error = ErrorOther String

opPlus :: Op "+" 2 'Infix
opPlus =
-}

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
          (FType _a1sty Nothing,        FType _a2sty (Just _a2ashp)) -> error "scalar x array"
          (FType _a1sty (Just _a1ashp), FType _a2sty Nothing) -> error "scalar x array"
          (FType _a1sty (Just a1ashp), FType _a2sty (Just a2ashp)) -> do
            case sameShape a1ashp a2ashp of
              Nothing -> err "array x array (same shape)"
              Just e  -> err $ "arrays diff shape: " <> e
          (FType a1sty Nothing, FType a2sty Nothing) -> do
            case (a1sty, a2sty) of
              (FTypeScalarInt i1, FTypeScalarInt i2) ->
                return $ FType (FTypeScalarInt (max i1 i2)) Nothing
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

{-| Fortran operation evaluation. Primarily intended for constant expression
    evaluation.

Fortran intrinsic procedures can have many, many properties:

  * work on subset of the intrinsic types, with return type depending on input
  * take optional kind parameter
  * work on arrays too, element-wise (elemental procedures)
  * in gfortran, work differently depending on compiler flags

We choose to pack everything into a simple operator type, and handle these
properties via combinators and a large error sum type. Binary operators, unary
operators and function call-style intrinsic procedures are all put into the same
type.
-}

{-# LANGUAGE KindSignatures, DataKinds, RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Fortran.Repr.Eval.Op where

import Language.Fortran.Repr.Type
import Language.Fortran.Repr.Type.Scalar
import Language.Fortran.Repr.Type.Array
import Language.Fortran.Repr.Value
import Language.Fortran.Repr.Value.Scalar

import GHC.TypeLits
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
opPlus = opNumericBin valOp typeOp
  where
    valOp sv1 sv2 = do
        case (sv1, sv2) of
          (FValScalarInt i1, FValScalarInt i2) -> do
            case fValIntSafeAdd i1 i2 of
              (i, True)   -> return $ FValScalarInt i
              (_i, False) -> err "plus op over/underflow"
          (FValScalarReal r1, FValScalarReal r2) -> do
            return $ FValScalarReal $ fValRealAdd r1 r2
          (FValScalarComplex c1, FValScalarComplex c2) -> do
            return $ FValScalarComplex $ fValComplexAdd c1 c2
          _ -> err $ "opPlus: not yet supported: given scalars:" <> show (sv1, sv2)
    typeOp sty1 sty2 = do
        case (sty1, sty2) of
          (FTypeScalarInt i1, FTypeScalarInt i2) ->
            return $ FTypeScalarInt $ max i1 i2
          _ -> err "opPlus: unsupported scalar types"
    err :: String -> Either String a
    err = Left

opMinus :: Op FType FVal
opMinus = opNumericBin valOp typeOp
  where
    valOp sv1 sv2 = do
        case (sv1, sv2) of
          (FValScalarInt i1, FValScalarInt i2) -> do
            case fValIntSafeMinus i1 i2 of
              (i, True)   -> return $ FValScalarInt i
              (_i, False) -> err "plus op over/underflow"
          (FValScalarReal r1, FValScalarReal r2) -> do
            return $ FValScalarReal $ fValRealMinus r1 r2
          (FValScalarComplex c1, FValScalarComplex c2) -> do
            return $ FValScalarComplex $ fValComplexMinus c1 c2
          _ -> err $ "opMinus: not yet supported: given scalars:" <> show (sv1, sv2)
    typeOp sty1 sty2 = do
        case (sty1, sty2) of
          (FTypeScalarInt i1, FTypeScalarInt i2) ->
            return $ FTypeScalarInt $ max i1 i2
          _ -> err "opMinus: unsupported scalar types"
    err :: String -> Either String a
    err = Left

opNumericBin
    :: (FValScalar -> FValScalar -> Either String FValScalar)
    -> (FTypeScalar -> FTypeScalar -> Either String FTypeScalar)
    -> Op FType FVal
opNumericBin valOp typeOp = Op{..}
  where
    op args = do
        let a1 = args !! 0
            a2 = args !! 1
        case (a1, a2) of
          (FValScalar a1s, FValScalar a2s) ->
            case a1s `valOp` a2s of
              Left  e -> err e
              Right x -> return $ FValScalar x
          --(FValArray a1s, FValScalar a2s) ->
          _ -> err "mada desu"
    opTy args = do
        let a1 = args !! 0
            a2 = args !! 1
        case (a1, a2) of
          (FType _a1sty Nothing,        FType _a2sty (Just _a2ashp)) -> err "scalar x array"
          (FType _a1sty (Just _a1ashp), FType _a2sty Nothing) -> err "scalar x array"
          (FType _a1sty (Just a1ashp), FType _a2sty (Just a2ashp)) -> do
            case sameShape a1ashp a2ashp of
              Nothing -> err "array x array (same shape)"
              Just e  -> err $ "arrays diff shape: " <> e
          (FType a1sty Nothing, FType a2sty Nothing) -> do
            case a1sty `typeOp` a2sty of
              Left  e -> err e
              Right x -> return $ FType x Nothing
    err :: forall a m. MonadError Error m => String -> m a
    err = throwError . ErrorStr

sameShape :: ArrayShape -> ArrayShape -> Maybe String
sameShape _arr1 _arr2 = Just "sameShape: not yet implemented"

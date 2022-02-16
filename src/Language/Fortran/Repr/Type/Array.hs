{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, KindSignatures #-}

module Language.Fortran.Repr.Type.Array where

import Language.Fortran.Repr.Type.Scalar ( FTypeScalar )

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ( NonEmpty )

import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )
import Data.Binary ( Binary )
import Text.PrettyPrint.GenericPretty ( Out(..) )

data FTypeArray = FTypeArray
  { fTypeArrayScalar :: FTypeScalar
  , fTypeArrayShape  :: ArrayShape
  } deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

data ArrayShape
  = ArrayShape              (NonEmpty Dimension)
  | ArrayShapeAssumedShape
  | ArrayShapeAssumedSize   [Dimension] -- ^ Implicit @*@ final dimension.
    deriving stock    (Eq, Ord, Show, Data, Typeable, Generic)
    deriving anyclass (Out, Binary)

instance Out a => Out (NonEmpty a) where
    doc = docList . NE.toList
    docPrec _ = doc

-- | (lower, upper) for indexing into the dimension. Negatives allowed.
--
-- TODO confirm maximum array size (and likely use Int64 explicitly)
type Dimension = (Int, Int)

module Language.Fortran.Analysis.Types.Internal where

import           Language.Fortran.AST
import           Language.Fortran.Analysis
import           Language.Fortran.Analysis.Parameters
import           Language.Fortran.Version
import           Language.Fortran.Util.Position
import           Language.Fortran.Intrinsics

import           Control.Monad.State.Strict
import           Control.Monad.Reader

import           Data.Map ( Map )

-- | Mapping of names to type information.
type TypeEnv = Map Name IDType

-- | Information about a detected type error.
type TypeError = (String, SrcSpan)

-- | Mapping of structures to field types
type StructTypeEnv = Map Name StructMemberTypeEnv
type StructMemberTypeEnv = Map Name IDType

-- Monad for type inference work
type Infer a = StateT InferState (Reader InferConfig) a

data InferState = InferState
  { langVersion :: FortranVersion
  , intrinsics  :: IntrinsicsTable
  , environ     :: TypeEnv
  , structs     :: StructTypeEnv
  , entryPoints :: Map Name (Name, Maybe Name)
  , typeErrors  :: [TypeError]
  , constMap    :: ConstMap
  } deriving (Show)

data InferConfig = InferConfig
  { inferConfigAcceptNonCharLengthAsKind :: Bool
  -- ^ How to handle declarations like @INTEGER x*8@. If true, providing a
  --   character length for a non-character data type will treat it as a kind
  --   parameter. In both cases, a warning is logged (nonstandard syntax).
  } deriving (Eq, Show)

type InferFunc t = t -> Infer ()

module Fig.Bless.Types where

import Fig.Prelude

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Aeson as Aeson

data BType
  = BTypeVariable Text
  | BTypeInteger
  | BTypeDouble
  | BTypeString
  | BTypeProgram BProgType
  | BTypeArray BType
  deriving (Show, Eq, Ord, Generic)
instance Aeson.ToJSON BType
instance Pretty BType where
  pretty (BTypeVariable s) = "!" <> s
  pretty BTypeInteger = "integer"
  pretty BTypeDouble = "double"
  pretty BTypeString = "string"
  pretty (BTypeProgram p) = "(" <> pretty p <> ")"
  pretty (BTypeArray p) = "Array<" <> pretty p <> ">"

data BProgType = BProgType
  { inp :: [BType]
  , out :: [BType]
  }
  deriving (Show, Eq, Ord, Generic)
instance Aeson.ToJSON BProgType
instance Pretty BProgType where
  pretty p = unwords (pretty <$> p.inp) <> " -- " <> unwords (pretty <$> p.out)

renameVars :: (Text -> Text) -> BType -> BType
renameVars f (BTypeVariable v) = BTypeVariable $ f v
renameVars f (BTypeArray t) = BTypeArray $ renameVars f t
renameVars f (BTypeProgram p) = BTypeProgram BProgType
  { inp = renameVars f <$> p.inp
  , out = renameVars f <$> p.out
  }
renameVars _ x = x

substitute :: Text -> BType -> BType -> BType
substitute n v (BTypeVariable n') | n == n' = v
substitute n v (BTypeArray t) = BTypeArray $ substitute n v t
substitute n v (BTypeProgram p) = BTypeProgram BProgType
  { inp = substitute n v <$> p.inp
  , out = substitute n v <$> p.out
  }
substitute _ _ x = x

applySubstitution :: Map Text BType -> BType -> BType
applySubstitution s (BTypeVariable v)
  | Just x <- Map.lookup v s = x
  | otherwise = BTypeVariable v
applySubstitution s (BTypeArray t) = BTypeArray $ applySubstitution s t
applySubstitution s (BTypeProgram p) = BTypeProgram BProgType
  { inp = applySubstitution s <$> p.inp
  , out = applySubstitution s <$> p.out
  }
applySubstitution _ x = x

typeVariables :: BType -> Set Text
typeVariables (BTypeVariable v) = Set.singleton v
typeVariables (BTypeArray t) = typeVariables t
typeVariables (BTypeProgram p) = Set.unions $ (typeVariables <$> p.inp) <> (typeVariables <$> p.out)
typeVariables _ = Set.empty

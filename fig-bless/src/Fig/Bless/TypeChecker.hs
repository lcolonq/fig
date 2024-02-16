{-# Language ImplicitParams #-}

module Fig.Bless.TypeChecker where

import Fig.Prelude

import Control.Exception.Safe (Typeable)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Data.Aeson as Aeson

import Fig.Bless.Syntax
import Fig.Bless.Types
import Fig.Bless.Runtime

data Env m t = Env
  { defs :: [(Word, BProgType)]
  , ext :: Extractor m t
  }

data TypeError t
  = TypeErrorWordNotFound (Maybe t) Word
  | TypeErrorMismatch (Maybe t) BType BType
  | TypeErrorArityMismatch (Maybe t) BProgType BProgType
  | TypeErrorMixedArray (Maybe t)
  deriving (Show, Eq, Ord, Generic)
instance (Show t, Typeable t) => Exception (TypeError t)
instance Aeson.ToJSON t => Aeson.ToJSON (TypeError t)
typeErrorPrefix :: Pretty t => Maybe t -> Text
typeErrorPrefix Nothing = ""
typeErrorPrefix (Just t) = mconcat
  [ "while typechecking term: ", pretty t, "\n"
  ]

instance Pretty t => Pretty (TypeError t) where
  pretty (TypeErrorWordNotFound t w) = mconcat
    [ typeErrorPrefix t
    , "word definition not found for: ", pretty w
    ]
  pretty (TypeErrorMismatch t expected actual) = mconcat
    [ typeErrorPrefix t
    , "type mismatch:\n"
    , "expected: ", pretty expected, "\n"
    , "actual: ", pretty actual
    ]
  pretty (TypeErrorArityMismatch t expected actual) = mconcat
    [ typeErrorPrefix t
    , "arity mismatch:\n"
    , "expected: ", pretty expected, "\n"
    , "actual: ", pretty actual
    ]
  pretty (TypeErrorMixedArray t) = mconcat
    [ typeErrorPrefix t
    , "array literal has mixed types"
    ]

type Typing m t = (MonadIO m, MonadThrow m, Typeable t, Show t, ?term :: Maybe t)

completeSubstitution :: [(BType, BType)] -> Either ((BType, BType), [(BType, BType)]) (Map Text BType)
completeSubstitution = go Map.empty
  where
    go acc [] = Right acc
    go acc ((BTypeVariable v, x):xs) -- ignore variables that are "done", e.g. they don't occur elsewhere
      | not (Set.member v (Set.unions $ (typeVariables . fst <$> xs) <> (typeVariables . snd <$> xs)))
      = go (Map.insert v x acc) xs
    go acc (x:xs) = Left (x, xs <> fmap (first BTypeVariable) (Map.toList acc))

unifyTypes :: forall m t. Typing m t => [(BType, BType)] -> m (Map Text BType)
unifyTypes subst = case completeSubstitution subst of
  Right f -> pure f
  Left ((BTypeArray t0, BTypeArray t1), xs) -- decompose arrays
    -> unifyTypes ((t0, t1):xs)
  Left ((BTypeProgram p0, BTypeProgram p1), xs) -- decompose programs
    | length p0.inp == length p1.inp
      && length p0.out == length p1.out
      -> unifyTypes (zip p0.inp p1.inp <> zip p0.out p1.out <> xs) -- only if the programs have the same arity
    | otherwise -> throwM $ TypeErrorArityMismatch ?term p0 p1 -- otherwise, arity mismatch
  Left ((t0, t1@(BTypeVariable _)), xs) -> unifyTypes $ (t1, t0):xs -- swap variables to lhs
  Left ((t0@(BTypeVariable v), t1), xs) -> unifyTypes -- eliminate
    $ (t0, t1):(bimap (substitute v t1) (substitute v t1) <$> xs)
  Left ((t0, t1), xs)
    | t0 == t1 -> unifyTypes xs -- delete non-variable matches
    | otherwise -> throwM $ TypeErrorMismatch ?term t0 t1 -- otherwise, mismatch

combineProgTypes :: forall m t. Typing m t => BProgType -> BProgType -> m BProgType
combineProgTypes f s' = do
  let fvars = typeVariables $ BTypeProgram f
  let s = BProgType { inp = ensureUniqueVars fvars <$> s'.inp, out = ensureUniqueVars fvars <$> s'.out }
  subst <- unifyTypes $ zip f.out s.inp
  let finp = applySubstitution subst <$> f.inp
  let fout = applySubstitution subst <$> f.out
  let sinp = applySubstitution subst <$> s.inp
  let sout = applySubstitution subst <$> s.out
  (leftover, dig) <- foldM
    ( \(l, d) t -> case l of
        x:xs
          | x == t -> pure (xs, d)
          | otherwise -> throwM $ TypeErrorMismatch ?term t x
        [] -> pure (l, d <> [t])
    )
    (fout, [])
    sinp
  pure BProgType
    { inp = finp <> dig
    , out = sout <> leftover
    }

typeOfLiteral :: Typing m t => Env m t -> LiteralF t -> m BType
typeOfLiteral _ LiteralInteger{} = pure BTypeInteger
typeOfLiteral _ LiteralDouble{} = pure BTypeDouble
typeOfLiteral _ LiteralString{} = pure BTypeString
typeOfLiteral e (LiteralArray xs) = mapM (typeOfLiteral e) xs >>= \case
  [] -> pure $ BTypeArray (BTypeVariable "a")
  ts@(t:_)
    | length (Set.fromList ts) == 1 -> pure $ BTypeArray t
    | otherwise -> throwM $ TypeErrorMixedArray ?term
typeOfLiteral e (LiteralQuote p) = BTypeProgram <$> typeOfProgram e p

typeOfProgram :: Typing m t => Env m t -> ProgramF t -> m BProgType
typeOfProgram e (Program p) = case p of
  ft:ts -> do
    x <- typeOf e ft
    foldM (\pt t -> let ?term = Just t in combineProgTypes pt =<< typeOf e t) x ts
  [] -> pure BProgType {inp = [] , out = []}

typeOf :: Typing m t => Env m t -> t -> m BProgType
typeOf e wt = do
  let ?term = Just wt
  e.ext wt >>= \case
    TermWord w -> case lookup w e.defs of
      Nothing -> throwM $ TypeErrorWordNotFound ?term w
      Just p -> pure p
    TermLiteral l -> do
      out <- typeOfLiteral e l
      pure BProgType
        { inp = []
        , out = [out]
        }

initializeEnv :: Extractor m t -> Builtins m t -> Env m t
initializeEnv ext bs = Env
  { ext = ext
  , defs = (\(w, (_, p)) -> (w, p)) <$> Map.toList (bs Nothing)
  }

checkDictionary :: forall m t. Typing m t => Env m t -> DictionaryF t -> m (Env m t)
checkDictionary env d = foldM
  ( \e (w, p) -> do
      pty <- typeOfProgram e p
      pure Env
        { ext = e.ext
        , defs = (w, pty):e.defs
        }
  )
  env
  d.defs

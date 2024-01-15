{-# Language ImplicitParams #-}

module Fig.Bless.Runtime
  ( ValueF(..), Value
  , ValueSort(..), valueSort
  , RuntimeError(..)
  , RunningTop, Running
  , Builtin, Builtins
  , Extractor
  , VM(..)
  , initialize
  , runProgram, runWord, run
  ) where

import Fig.Prelude

import Control.Exception.Safe (Typeable)

import qualified Data.Text as Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Fig.Bless.Syntax as Syn

data ValueF t v
  = ValueInteger Integer
  | ValueDouble Double
  | ValueString Text
  | ValueWord Syn.Word
  | ValueProgram (Syn.ProgramF t)
  | ValueArray [v]
  deriving (Show, Eq, Ord)
instance (Pretty t, Pretty v) => Pretty (ValueF t v) where
  pretty (ValueInteger i) = tshow i
  pretty (ValueDouble d) = tshow d
  pretty (ValueString s) = tshow s
  pretty (ValueWord s) = pretty s
  pretty (ValueProgram p) = pretty p
  pretty (ValueArray vs) = mconcat
    [ "["
    , Text.intercalate ", " $ pretty <$> vs
    , "]"
    ]
type Value = ValueF (Fix Syn.TermF) (Fix (ValueF (Fix Syn.TermF)))

data ValueSort
  = ValueSortInteger
  | ValueSortDouble
  | ValueSortString
  | ValueSortWord
  | ValueSortProgram
  | ValueSortArray
  deriving (Show, Eq, Ord)
instance Pretty ValueSort where
  pretty ValueSortInteger = "integer"
  pretty ValueSortDouble = "double"
  pretty ValueSortString = "string"
  pretty ValueSortWord = "word"
  pretty ValueSortProgram = "program"
  pretty ValueSortArray = "list"

valueSort :: ValueF t v -> ValueSort
valueSort (ValueInteger _) = ValueSortInteger
valueSort (ValueDouble _) = ValueSortDouble
valueSort (ValueString _) = ValueSortString
valueSort (ValueWord _) = ValueSortWord
valueSort (ValueProgram _) = ValueSortProgram
valueSort (ValueArray _) = ValueSortArray

data RuntimeError t
  = RuntimeErrorWordNotFound (Maybe t) Syn.Word
  | RuntimeErrorOutOfFuel (Maybe t)
  | RuntimeErrorStackUnderflow (Maybe t)
  | RuntimeErrorSortMismatch (Maybe t) ValueSort ValueSort
  deriving (Show, Eq, Ord)
instance (Show t, Typeable t) => Exception (RuntimeError t)
runtimeErrorPrefix :: Pretty t => Maybe t -> Text
runtimeErrorPrefix Nothing = ""
runtimeErrorPrefix (Just t) = mconcat
  [ "while evaluating term: ", pretty t, "\n"
  ]

instance Pretty t => Pretty (RuntimeError t) where
  pretty (RuntimeErrorWordNotFound t w) = mconcat
    [ runtimeErrorPrefix t
    , "word definition not found for: ", pretty w
    ]
  pretty (RuntimeErrorOutOfFuel t) = mconcat
    [ runtimeErrorPrefix t
    , "out of fuel"
    ]
  pretty (RuntimeErrorStackUnderflow t) = mconcat
    [ runtimeErrorPrefix t
    , "stack underflow"
    ]
  pretty (RuntimeErrorSortMismatch t expected actual) = mconcat
    [ runtimeErrorPrefix t
    , "sort mismatch at runtime (this probably shouldn't happen, please report it as a bug):\n"
    , "expected: ", pretty expected, "\n"
    , "actual: ", pretty actual
    ]

type RunningTop m t v = (MonadThrow m, Typeable t, Show t)
type Running m t v = (RunningTop m t v, ?term :: Maybe t)
type Builtin m t v = VM m t v -> m (VM m t v)
type Builtins m t v = Maybe t -> Map Syn.Word (Builtin m t v)
type Extractor m t = t -> m (Syn.TermF t)
data VM m t v = VM
  { fuel :: Maybe Integer
  , bindings :: Syn.DictionaryF t
  , builtins :: Builtins m t v
  , stack :: [ValueF t v]
  }

initialize :: Running m t v => Maybe Integer -> Syn.DictionaryF t -> Builtins m t v -> VM m t v
initialize fuel bindings builtins = VM{..}
  where
    stack = []

checkFuel :: Running m t v => VM m t v -> m (VM m t v)
checkFuel vm
  | Just f <- vm.fuel
  = if f <= 0
    then throwM $ RuntimeErrorOutOfFuel ?term
    else pure vm { fuel = Just $ f - 1 }
checkFuel vm = pure vm

push :: Running m t v => ValueF t v -> VM m t v -> VM m t v
push v vm = vm
  { stack = v : vm.stack
  }

runProgram :: Running m t v => Extractor m t -> Syn.ProgramF t -> VM m t v -> m (VM m t v)
runProgram f (Syn.Program p) vm = foldM (flip (run f)) vm p

runWord :: Running m t v => Extractor m t -> Syn.Word -> VM m t v -> m (VM m t v)
runWord _ w vm | Just b <- Map.lookup w $ vm.builtins ?term = b vm
runWord f w vm | Just p <- Map.lookup w vm.bindings.defs = runProgram f p vm
runWord _ w _ = throwM $ RuntimeErrorWordNotFound ?term w

run :: Running m t v => Extractor m t -> t -> VM m t v -> m (VM m t v)
run f v vm =
  let ?term = Just v in f v >>= \case
    Syn.TermLiteral (Syn.LiteralInteger i) -> push (ValueInteger i) <$> checkFuel vm
    Syn.TermLiteral (Syn.LiteralDouble i) -> push (ValueDouble i) <$> checkFuel vm
    Syn.TermLiteral (Syn.LiteralString i) -> push (ValueString i) <$> checkFuel vm
    Syn.TermQuote p -> push (ValueProgram p) <$> checkFuel vm
    Syn.TermWord w -> runWord f w =<< checkFuel vm

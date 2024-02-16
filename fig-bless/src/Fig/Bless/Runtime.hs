{-# Language ImplicitParams #-}

module Fig.Bless.Runtime
  ( ValueF(..)
  , EffectF(..)
  , ValueSort(..), valueSort
  , RuntimeError(..)
  , RunningTop, Running
  , BuiltinProgram, Builtin, Builtins
  , VM(..)
  , initialize
  , runProgram, runWord, run
  ) where

import Fig.Prelude

import Control.Exception.Safe (Typeable)

import qualified Data.Text as Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Data.Aeson as Aeson

import Fig.Bless.Types
import qualified Fig.Bless.Syntax as Syn

data ValueF t
  = ValueInteger Integer
  | ValueDouble Double
  | ValueString Text
  | ValueProgram (Syn.ProgramF t)
  | ValueArray [ValueF t]
  deriving (Show, Eq, Ord, Generic)
instance Aeson.ToJSON t => Aeson.ToJSON (ValueF t)
instance Pretty t => Pretty (ValueF t) where
  pretty (ValueInteger i) = tshow i
  pretty (ValueDouble d) = tshow d
  pretty (ValueString s) = tshow s
  pretty (ValueProgram p) = pretty p
  pretty (ValueArray vs) = mconcat
    [ "{"
    , unwords $ pretty <$> vs
    , "}"
    ]

data EffectF t
  = EffectPrint (ValueF t)
  | EffectPrintBackwards (ValueF t)
  | EffectSoundboard (ValueF t)
  | EffectModelToggle (ValueF t)
  deriving (Show, Eq, Ord, Generic)
instance Aeson.ToJSON t => Aeson.ToJSON (EffectF t)
instance Pretty t => Pretty (EffectF t) where
  pretty (EffectPrint x) =  "(print " <> pretty x <> ")"
  pretty (EffectPrintBackwards x) =  "(print-backwards " <> pretty x <> ")"
  pretty (EffectSoundboard x) =  "(soundboard " <> pretty x <> ")"
  pretty (EffectModelToggle x) =  "(moddle-toggle " <> pretty x <> ")"

data ValueSort
  = ValueSortInteger
  | ValueSortDouble
  | ValueSortString
  | ValueSortWord
  | ValueSortProgram
  | ValueSortArray
  deriving (Show, Eq, Ord, Generic)
instance Aeson.ToJSON ValueSort
instance Pretty ValueSort where
  pretty ValueSortInteger = "integer"
  pretty ValueSortDouble = "double"
  pretty ValueSortString = "string"
  pretty ValueSortWord = "word"
  pretty ValueSortProgram = "program"
  pretty ValueSortArray = "list"
valueSort :: ValueF t -> ValueSort
valueSort (ValueInteger _) = ValueSortInteger
valueSort (ValueDouble _) = ValueSortDouble
valueSort (ValueString _) = ValueSortString
valueSort (ValueProgram _) = ValueSortProgram
valueSort (ValueArray _) = ValueSortArray

data RuntimeError t
  = RuntimeErrorWordNotFound (Maybe t) Syn.Word
  | RuntimeErrorOutOfFuel (Maybe t)
  | RuntimeErrorStackUnderflow (Maybe t)
  | RuntimeErrorSortMismatch (Maybe t) ValueSort ValueSort
  deriving (Show, Eq, Ord, Generic)
instance (Show t, Typeable t) => Exception (RuntimeError t)
instance Aeson.ToJSON t => Aeson.ToJSON (RuntimeError t)
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

type RunningTop m t = (MonadThrow m, Typeable t, Show t)
type Running m t = (RunningTop m t, ?term :: Maybe t)
type BuiltinProgram m t = VM m t -> m (VM m t)
type Builtin m t = (BuiltinProgram m t, BProgType)
type Builtins m t = Maybe t -> Map Syn.Word (Builtin m t)
data VM m t = VM
  { fuel :: Maybe Integer
  , bindings :: Syn.DictionaryF t
  , builtins :: Builtins m t
  , stack :: [ValueF t]
  , effects :: [EffectF t]
  }

initialize :: Running m t => Maybe Integer -> Syn.DictionaryF t -> Builtins m t -> VM m t
initialize fuel bindings builtins = VM{..}
  where
    stack = []
    effects = []

checkFuel :: Running m t => VM m t -> m (VM m t)
checkFuel vm
  | Just f <- vm.fuel
  = if f <= 0
    then throwM $ RuntimeErrorOutOfFuel ?term
    else pure vm { fuel = Just $ f - 1 }
checkFuel vm = pure vm

push :: Running m t => ValueF t -> VM m t -> VM m t
push v vm = vm
  { stack = v : vm.stack
  }

runProgram :: Running m t => Syn.Extractor m t -> Syn.ProgramF t -> VM m t -> m (VM m t)
runProgram f (Syn.Program p) vm = foldM (flip (run f)) vm p

runWord :: Running m t => Syn.Extractor m t -> Syn.Word -> VM m t -> m (VM m t)
runWord _ w vm | Just (b, _) <- Map.lookup w $ vm.builtins ?term = b vm
runWord f w vm | Just p <- lookup w vm.bindings.defs = runProgram f p vm
runWord _ w _ = throwM $ RuntimeErrorWordNotFound ?term w

literalValue :: Syn.LiteralF t -> ValueF t
literalValue (Syn.LiteralInteger i) = ValueInteger i
literalValue (Syn.LiteralDouble i) = ValueDouble i
literalValue (Syn.LiteralString i) = ValueString i
literalValue (Syn.LiteralArray xs) = ValueArray $ literalValue <$> xs
literalValue (Syn.LiteralQuote p) = ValueProgram p

run :: Running m t => Syn.Extractor m t -> t -> VM m t -> m (VM m t)
run f v vm =
  let ?term = Just v in f v >>= \case
    Syn.TermLiteral l -> push (literalValue l) <$> checkFuel vm
    Syn.TermWord w -> runWord f w =<< checkFuel vm

{-# Language ImplicitParams #-}

module Fig.Bless.Builtins
  ( arithmetic
  ) where

import Fig.Prelude

import Control.Monad.State.Strict (execStateT, StateT)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Fig.Bless.Syntax as Syn
import Fig.Bless.Runtime

-- * Helper functions
stateful :: Running m t v => StateT (VM m t v) m a -> Builtin m t v
stateful = execStateT

push :: (Running m t v, MonadState (VM m' t v) m) => ValueF t v -> m ()
push v = state \vm -> ((), vm { stack = v : vm.stack })

pop :: (Running m t v, MonadState (VM m' t v) m) => m (ValueF t v)
pop = get >>= \case
  vm | x:xs <- vm.stack -> do
         put vm { stack = xs }
         pure x
     | otherwise -> throwM $ RuntimeErrorStackUnderflow ?term

int :: Running m t v => ValueF t v -> m Integer
int (ValueInteger i) = pure i
int v = throwM $ RuntimeErrorSortMismatch ?term ValueSortInteger (valueSort v)

double :: Running m t v => ValueF t v -> m Double
double (ValueDouble d) = pure d
double v = throwM $ RuntimeErrorSortMismatch ?term ValueSortDouble (valueSort v)

string :: Running m t v => ValueF t v -> m Text
string (ValueString s) = pure s
string v = throwM $ RuntimeErrorSortMismatch ?term ValueSortString (valueSort v)

program :: Running m t v => ValueF t v -> m (Syn.ProgramF t)
program (ValueProgram p) = pure p
program v = throwM $ RuntimeErrorSortMismatch ?term ValueSortProgram (valueSort v)

array :: Running m t v => ValueF t v -> m [v]
array (ValueArray a) = pure a
array v = throwM $ RuntimeErrorSortMismatch ?term ValueSortProgram (valueSort v)

-- * Arithmetic builtins
add :: Running m t v => Builtin m t v
add = stateful do
  y <- int =<< pop
  x <- int =<< pop
  push . ValueInteger $ x + y

mul :: Running m t v => Builtin m t v
mul = stateful do
  y <- int =<< pop
  x <- int =<< pop
  push . ValueInteger $ x * y

sub :: Running m t v => Builtin m t v
sub = stateful do
  y <- int =<< pop
  x <- int =<< pop
  push . ValueInteger $ x - y

div :: Running m t v => Builtin m t v
div = stateful do
  y <- int =<< pop
  x <- int =<< pop
  push . ValueInteger $ quot x y

arithmetic :: RunningTop m t v => Builtins m t v
arithmetic t = let ?term = t in Map.fromList
  [ ("+", add)
  , ("*", mul)
  , ("-", sub)
  , ("/", div)
  ]

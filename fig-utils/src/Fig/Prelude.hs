module Fig.Prelude
  ( quot, mod, rem, quotRem
  , module GHC.Num
  , module GHC.Float

  , module System.IO
  , module System.FilePath.Posix

  , module Data.Kind
  , module Data.Void
  , module Data.Bool
  , module Data.Char
  , module Data.Int
  , module Data.Text
  , module Data.Text.IO
  , module Data.Text.Encoding
  , module Data.ByteString
  , module Data.Tuple
  , module Data.Maybe
  , module Data.Either
  , module Data.List
  , module Data.Function
  , module Data.Eq
  , module Data.Ord
  , module Data.Semigroup
  , module Data.Monoid
  , module Data.Functor
  , module Data.Bifunctor
  , module Data.Traversable
  , module Data.Foldable

  , module Text.Show
  , module Text.Read

  , module Control.Applicative
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Control.Monad.State.Class
  , module Control.Monad.Reader.Class
  , module Control.Exception.Safe

  , tshow
  , headMay, atMay
  , throwLeft
  , log

  , Pretty(..)
  ) where

import Prelude (quot, mod, rem, quotRem)

import GHC.Num (Num(..), Integer)
import GHC.Float (Double)

import System.IO (IO, stdin, stdout, stderr, FilePath, Handle)
import System.FilePath.Posix ((</>))

import Data.Kind (Type)
import Data.Void (Void)
import Data.Bool (Bool(..), otherwise, not, (&&), (||))
import Data.Char (Char, isUpper)
import Data.Int (Int)
import Data.Text (Text, pack, unpack, unwords)
import Data.Text.IO (hPutStrLn)
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import Data.ByteString (ByteString, readFile, writeFile)
import Data.Tuple (fst, snd, curry, uncurry, swap)
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust, catMaybes)
import Data.Either (Either(..))
import Data.List (take, drop, dropWhile, filter, reverse, lookup, zip, zip3, replicate, sortOn, concatMap, elemIndex)
import Data.Function (id, const, flip, ($), (&), (.))
import Data.Eq (Eq(..))
import Data.Ord (Ord(..), Down(..))
import Data.Semigroup(Semigroup(..), (<>))
import Data.Monoid (Monoid(..), mconcat)
import Data.Functor (Functor(..), (<$>), (<$), ($>))
import Data.Bifunctor (Bifunctor(..), first, second)
import Data.Traversable (Traversable(..), forM, sequence)
import Data.Foldable (Foldable(..), any, all, mapM_, forM_)
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time

import Text.Show (Show(..))
import Text.Read (readMaybe)

import Control.Applicative (Applicative(..), (<*), (*>))
import Control.Monad (Monad(..), join, forever, mapM, forM, foldM, void, (>>=), (=<<), (>=>), (<=<))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Class (MonadState(..), get, put, modify)
import Control.Monad.Reader.Class (MonadReader(..), ask)
import Control.Exception.Safe (Exception, SomeException, IOException, MonadThrow, MonadCatch, MonadMask, throwM, try, catch, catchIO, bracket, bracketOnError)

tshow :: Show a => a -> Text
tshow = pack . show

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

atMay :: [a] -> Int -> Maybe a
atMay [] _ = Nothing
atMay (x:_) 0 = Just x
atMay (_:xs) n = atMay xs $ n - 1

throwLeft :: (Exception e, MonadThrow m) => (b -> e) -> Either b a -> m a
throwLeft f (Left x) = throwM $ f x
throwLeft _ (Right x) = pure x

log :: MonadIO m => Text -> m ()
log msg = do
  t <- liftIO Time.getCurrentTime
  let time = Time.formatTime Time.defaultTimeLocale "[%F %T] " t
  liftIO . hPutStrLn stderr $ pack time <> msg

class Pretty a where
  pretty :: a -> Text

instance Pretty Void where
  pretty _ = ""

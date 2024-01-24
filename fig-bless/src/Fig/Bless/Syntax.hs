{-# Language ApplicativeDo #-}

module Fig.Bless.Syntax
  ( Word(..)
  , Literal(..)
  , ProgramF(..)
  , Program
  , TermF(..)
  , Term
  , DictionaryF(..)
  , Dictionary
  , Extractor
  , word, literal, termF, term, programF, program, dictionaryF, dictionary, P.eof
  , Spanning(..), unSpanning, spanning
  , ParseError(..)
  , parse
  ) where

import Fig.Prelude

import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.String (IsString(..))

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P.C
import qualified Text.Megaparsec.Char.Lexer as P.C.L

import qualified Data.Aeson as Aeson

newtype Word = Word Text
  deriving (Show, Eq, Ord, Generic)
instance Aeson.ToJSON Word
instance IsString Word where
  fromString = Word . fromString
instance Pretty Word where
  pretty (Word t) = t

data Literal
  = LiteralInteger Integer
  | LiteralDouble Double
  | LiteralString Text
  | LiteralArray [Literal]
  deriving (Show, Eq, Ord, Generic)
instance Aeson.ToJSON Literal
instance Pretty Literal where
  pretty (LiteralInteger i) = tshow i
  pretty (LiteralDouble d) = tshow d
  pretty (LiteralString s) = tshow s
  pretty (LiteralArray xs) = "{" <> unwords (pretty <$> xs) <> "}"

newtype ProgramF t = Program [t]
  deriving (Show, Eq, Ord, Generic, Functor)
instance Aeson.ToJSON t => Aeson.ToJSON (ProgramF t)
instance Pretty t => Pretty (ProgramF t) where
  pretty (Program ts) = unwords $ pretty <$> ts
type Program = ProgramF (Fix TermF)

data TermF t
  = TermWord Word
  | TermLiteral Literal
  | TermQuote (ProgramF t)
  deriving (Show, Eq, Ord, Generic, Functor)
instance Aeson.ToJSON t => Aeson.ToJSON (TermF t)
instance Pretty t => Pretty (TermF t) where
  pretty (TermWord w) = pretty w
  pretty (TermLiteral l) = pretty l
  pretty (TermQuote p) = pretty p
type Term = TermF (Fix TermF)

newtype DictionaryF t = Dictionary
  { defs :: [(Word, ProgramF t)]
  } deriving (Show, Eq, Ord)
instance Pretty t => Pretty (DictionaryF t) where
  pretty d = unlines $ d.defs <&> \(w, p) -> mconcat
    [ pretty w
    , " = "
    , pretty p
    ]
type Dictionary = DictionaryF (Fix TermF)

type Extractor m t = t -> m (TermF t)

type Parser = P.Parsec Void Text

ws :: Parser ()
ws = P.C.L.space
  P.C.space1
  (P.C.L.skipLineComment "//")
  (P.C.L.skipBlockComment "/*" "*/")

wordChar :: Char -> Bool
wordChar c = not $ elem @[] c ['[', ']', '=', ';'] || isSpace c
word :: Parser Word
word = Word . pack <$> P.some (P.satisfy wordChar)

literal :: Parser Literal
literal =
  P.try ( (LiteralDouble <$> P.C.L.signed (pure ()) P.C.L.float)
    P.<?> "floating-point literal"
  ) P.<|>
  ( (LiteralInteger <$>
      P.C.L.signed (pure ())
      ( (P.C.string' "0x" *> P.C.L.hexadecimal) P.<|>
        (P.C.string' "0o" *> P.C.L.octal) P.<|>
        (P.C.string' "0b" *> P.C.L.binary) P.<|>
        P.C.L.decimal
      )
    ) P.<?> "integer literal"
  ) P.<|>
  ( (LiteralString . pack <$> (P.C.char '"' *> P.manyTill P.C.L.charLiteral (P.C.char '"')))
    P.<?> "string literal"
  ) P.<|>
  ( (LiteralArray <$> (P.C.char '{' *> P.many (ws *> literal <* ws) <* P.C.char '}'))
    P.<?> "array literal"
  )

programF :: Parser t -> Parser (ProgramF t)
programF pt = Program <$> P.many (ws *> pt <* ws)

program :: Parser Program
program = programF $ Fix <$> term

termF :: Parser t -> Parser (TermF t)
termF pt =
  ( P.try (TermLiteral <$> literal)
    P.<|> (TermWord <$> word)
    P.<|> (TermQuote <$> (P.C.char '[' *> programF pt <* P.C.char ']'))
  ) P.<?> "term"

term :: Parser Term
term = termF $ Fix <$> term

dictionaryF :: Parser t -> Parser (DictionaryF t)
dictionaryF pt = Dictionary <$> P.many
  ( (,)
    <$> (ws *> word <* ws <* P.C.char '=')
    <*> (ws *> programF pt <* ws <* P.C.char ';')
  )

dictionary :: Parser Dictionary
dictionary = dictionaryF $ Fix <$> term

newtype ParseError = ParseError (P.ParseErrorBundle Text Void)
  deriving (Show, Generic)
instance Exception ParseError
instance Aeson.ToJSON ParseError where
  toJSON (ParseError b) = Aeson.object
    [ "errors" Aeson..= Aeson.toJSON (bimap P.parseErrorPretty P.sourcePosPretty <$> posed)
    ]
    where
      (posed, _) = P.attachSourcePos P.errorOffset (P.bundleErrors b) (P.bundlePosState b)
instance Pretty ParseError where
  pretty (ParseError b) = mconcat
    [ "failed to read program:\n"
    , pack $ P.errorBundlePretty b
    ]

data Spanning = Spanning
  { t :: TermF Spanning
  , start :: P.SourcePos
  , end :: P.SourcePos
  } deriving (Show, Eq, Ord, Generic)
instance Aeson.ToJSON Spanning where
  toJSON s = Aeson.object
    [ "term" Aeson..= Aeson.toJSON s.t
    , "start" Aeson..= pack (P.sourcePosPretty s.start)
    , "end" Aeson..= pack (P.sourcePosPretty s.end)
    ]
instance Pretty Spanning where
  pretty s = mconcat
    [ pretty s.t, "\n"
    , pack $ P.sourcePosPretty s.start, ":"
    ]
unSpanning :: Spanning -> TermF Spanning
unSpanning s = s.t

spanning :: Parser Spanning
spanning = do
  start <- P.getSourcePos
  t <- termF spanning
  end <- P.getSourcePos
  pure Spanning{..}

parse :: MonadThrow m => Text -> Parser a -> Text -> m a
parse nm p inp = case P.runParser p (unpack nm) inp of
  Left err -> throwM $ ParseError err
  Right x -> pure x

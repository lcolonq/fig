{-# Language TemplateHaskellQuotes #-}

module Fig.Utils.SExpr
  ( SExprWith(..)
  , SExpr
  , parseSExpr
  , sexp
  ) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as Q
import qualified Language.Haskell.TH.Syntax as Q

import Fig.Prelude

import Control.Monad (fail, mzero)

import Data.Data (Data, cast)
import Data.Char (isSpace)

import Text.Megaparsec
import Text.Megaparsec.Char

data SExprWith :: Type -> Type where
  SExprExt :: forall a. a -> SExprWith a
  SExprSymbol :: forall a. Text -> SExprWith a
  SExprString :: forall a. Text -> SExprWith a
  SExprInteger :: forall a. Integer -> SExprWith a
  SExprFloat :: forall a. Double -> SExprWith a
  SExprList :: forall a. [SExprWith a] -> SExprWith a
deriving instance Show a => Show (SExprWith a)
deriving instance Eq a => Eq (SExprWith a)
deriving instance Ord a => Ord (SExprWith a)
deriving instance Data a => Data (SExprWith a)
deriving instance Functor SExprWith

escapeStr :: Char -> [Char]
escapeStr '\n' = ['\\', 'n']
escapeStr c
  | elem @[] c ['\\', '"'] || isSpace c = ['\\', c]
  | otherwise = [c]

escapeSym :: Char -> [Char]
escapeSym '\n' = ['\\', 'n']
escapeSym c
  | elem @[] c ['\\', '"', '.', '(', ')'] || isSpace c = ['\\', c]
  | otherwise = [c]

instance Pretty a => Pretty (SExprWith a) where
  pretty (SExprExt x) = pretty x
  pretty (SExprSymbol s) = pack . mconcat $ escapeSym <$> unpack s
  pretty (SExprString s) = mconcat ["\"", pack . mconcat $ escapeStr <$> unpack s, "\""]
  pretty (SExprInteger i) = tshow i
  pretty (SExprFloat f) = tshow f
  pretty (SExprList xs) = "(" <> unwords (pretty <$> xs) <> ")"

type SExpr = SExprWith Void

type Parser = Parsec Void Text

sexprWith :: forall a. Parser a -> Parser (SExprWith a)
sexprWith ext = spaces *>
  ( SExprExt <$> ext
    <|> SExprString . pack <$> (char '"' *> manyTill (escapedNewline <|> (char '\\' *> anyNonNewline) <|> strchar) (char '"'))
    <|> parseNumber
    <|> SExprSymbol . pack <$> some (escapedNewline <|> (char '\\' *> anyNonNewline) <|> symchar)
    <|> SExprList <$> (char '(' *> spaces *> many (spaces *> sexprWith ext <* spaces) <* char ')')
  )
  where
    escapedNewline = string "\\n" $> '\n'
    anyNonNewline = satisfy (/='\n')
    spaces = many spaceChar
    symchar = satisfy $ \c -> not (isSpace c || c `elem` special)
    strchar = satisfy (\x -> x /= '"' && x /= '\n')
    special :: [Char]
    special = "\".()"
    classifyNumber :: Bool -> [Char] -> Parser (SExprWith a)
    classifyNumber neg str = do
      let
        maybeNeg :: forall n. Num n => n -> n
        maybeNeg = if neg then negate else id
        res =
          if '.' `elem` str
          then SExprFloat . maybeNeg <$> readMaybe str
          else SExprInteger . maybeNeg <$> readMaybe str
      maybe mzero pure res
    parseNumber :: Parser (SExprWith a)
    parseNumber = do
      neg <- option False $ char '-' $> True
      leading <- many digitChar
      trailing <- optional $ char '.' *> many digitChar
      classifyNumber neg $ leading <> maybe "" (\x -> if null x then "" else "." <> x) trailing

parseSExprWith :: Parser a -> Text -> Maybe (SExprWith a)
parseSExprWith ext inp = case runParser (sexprWith ext) "" inp of
  Left _ -> Nothing
  Right s -> Just s

parseSExpr :: Text -> Maybe SExpr
parseSExpr = parseSExprWith empty

data AntiSExpr
  = AntiSExpr Text
  | AntiSExprSplice Text
  deriving (Show, Eq, Ord, Data)

antisexpr :: Parser AntiSExpr
antisexpr =
  AntiSExprSplice . pack <$> (string ",@" *> ((:) <$> letterChar <*> many alphaNumChar))
  <|> AntiSExpr . pack <$> (char ',' *> ((:) <$> letterChar <*> many alphaNumChar))

antiSExprExp :: SExprWith AntiSExpr -> Maybe (Q.Q Q.Exp)
antiSExprExp (SExprExt (AntiSExpr nm)) = Just $ TH.varE (TH.mkName $ unpack nm)
antiSExprExp (SExprList xs) = do
  let exps = flip fmap xs \case
        SExprExt (AntiSExprSplice nm) -> TH.varE . TH.mkName $ unpack nm
        s -> TH.listE [liftSExpr s]
  Just $ TH.appE
    (TH.conE $ TH.mkName "SExprList")
    (TH.appE (TH.varE $ TH.mkName "mconcat") (TH.listE exps) )
antiSExprExp _ = Nothing

liftText :: Text -> Q.Q Q.Exp
liftText txt = Q.AppE (Q.VarE 'pack) <$> Q.lift (unpack txt)

liftSExpr :: Data a => a -> Q.Q Q.Exp
liftSExpr = Q.dataToExpQ (\a -> maybe (liftText <$> cast a) antiSExprExp $ cast a)

sexp :: Q.QuasiQuoter
sexp = Q.QuasiQuoter
  { quoteExp = \s -> do
      expr <- maybe (fail "parse error") pure . parseSExprWith antisexpr $ pack s
      liftSExpr expr
  , quotePat = \_ -> fail "unsupported s-expression in pattern context"
  , quoteType = \_ -> fail "unsupported s-expression in type context"
  , quoteDec = \_ -> fail "unsupported s-expression in declaration context"
  }

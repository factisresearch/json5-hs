{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aeson.Json5
  ( parseJson,
  )
where

import Control.Applicative (Applicative (..))
import Data.Aeson
import Data.Bits
import Data.Char
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.Proxy
import Data.Scientific (Scientific)
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Vector as V
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

parseJson :: ParseInput s => s -> Either String Value
parseJson s =
  case runParser (jsonP <* jsonFiller <* eof) "" s of
    Left e -> Left (errorBundlePretty e)
    Right x -> Right x

class (Stream s, Token s ~ Char, IsString (Tokens s)) => ParseInput s where
  toBuilder :: Proxy s -> Tokens s -> TLB.Builder

instance ParseInput T.Text where
  toBuilder _ = TLB.fromText

instance ParseInput TL.Text where
  toBuilder _ = TLB.fromLazyText

instance c ~ Char => ParseInput [c] where
  toBuilder _ = TLB.fromString

type Parser s a = Parsec Void s a

jsonFiller :: ParseInput s => Parser s ()
jsonFiller = C.space

jsonP :: ParseInput s => Parser s Value
jsonP =
  jsonFiller *> jsonP'

jsonP' :: ParseInput s => Parser s Value
jsonP' =
  Number <$> numberP
    <|> nullP
    <|> Bool <$> boolP
    <|> String <$> stringP
    <|> Array <$> arrayP
    <|> Object <$> dictP

arrayP :: ParseInput s => Parser s Array
arrayP =
  let arrayElem = C.char ',' *> jsonP
      nonEmptyContents = liftA2 (:) jsonP' (some arrayElem) <* jsonFiller
   in C.char '['
        *> jsonFiller
        *> (fmap V.fromList nonEmptyContents <|> pure V.empty) <* C.char ']'

dictP :: ParseInput s => Parser s Object
dictP =
  let dictKeyPair = do
        key <- stringP
        jsonFiller
        C.char ':'
        val <- jsonP
        return (key, val)
      dictElem = C.char ',' *> jsonFiller *> dictKeyPair
      nonEmptyContents !acc = do
        jsonFiller
        (dictElem >>= nonEmptyContents . flip (:) acc)
          <|> (C.char '}' $> HM.fromList acc)
   in C.char '{'
        *> jsonFiller
        *> ( (dictKeyPair >>= nonEmptyContents . (: []))
               <|> HM.empty <$ C.char '}'
           )

nullP :: ParseInput s => Parser s Value
nullP = C.string "null" $> Null

boolP :: ParseInput s => Parser s Bool
boolP = C.string "true" $> True <|> C.string "false" $> False

numberP :: ParseInput s => Parser s Scientific
numberP =
  L.signed (return ()) L.scientific

stringP :: ParseInput s => Parser s T.Text
stringP = C.char '"' *> go mempty
  where
    go !acc =
      (C.char '"' $> TL.toStrict (TLB.toLazyText acc)) <|> (chars >>= go . (acc <>))

chars :: forall s. ParseInput s => Parser s TLB.Builder
chars =
  try (C.string "\\\"" $> "\"")
    <|> try (C.string "\\\\" $> "\\")
    <|> try (C.string "\\/" $> "/")
    <|> try (C.string "\\b" $> "\b")
    <|> try (C.string "\\f" $> "\f")
    <|> try (C.string "\\n" $> "\n")
    <|> try (C.string "\\r" $> "\r")
    <|> try (C.string "\\t" $> "\t")
    <|> try (fmap TLB.singleton unicodeChar)
    <|> fmap
      (toBuilder (Proxy @s))
      (takeWhile1P Nothing (\c -> c /= '"' && c /= '\\' && not (isControl c)))

unicodeChar :: ParseInput s => Parser s Char
unicodeChar = do
  C.string "\\u"
  u1 <- fmap digitToInt C.hexDigitChar
  u2 <- fmap digitToInt C.hexDigitChar
  u3 <- fmap digitToInt C.hexDigitChar
  u4 <- fmap digitToInt C.hexDigitChar
  return $! chr (shiftL 12 u1 .|. shiftL 8 u2 .|. shiftL 4 u3 .|. u4)

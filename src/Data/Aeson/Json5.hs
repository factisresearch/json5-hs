{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aeson.Json5
  ( parseJson,
    parseJson5,
  )
where

import Data.Aeson
import Data.Bits
import Data.Char
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.Proxy
import Data.Scientific (Scientific, base10Exponent, coefficient, scientific)
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Vector as V
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

data ParseMode = JSON | JSON5

parseJson :: ParseInput s => s -> Either String Value
parseJson = parseJson' JSON

parseJson5 :: ParseInput s => s -> Either String Value
parseJson5 = parseJson' JSON5

parseJson' :: ParseInput s => ParseMode -> s -> Either String Value
parseJson' mode s =
  case runParser (jsonP mode <* jsonFiller mode <* eof) "" s of
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

jsonFiller :: ParseInput s => ParseMode -> Parser s ()
jsonFiller mode =
  let jsonSpace =
        oneOf . map chr $
          [ 0x20,
            0x09,
            0x0A,
            0x0D
          ]
   in case mode of
        JSON -> skipMany jsonSpace
        JSON5 ->
          L.space
            (skipSome jsonSpace)
            (L.skipLineComment "//")
            (L.skipBlockComment "/*" "*/")

jsonP :: ParseInput s => ParseMode -> Parser s Value
jsonP mode =
  jsonFiller mode *> jsonP' mode

jsonP' :: ParseInput s => ParseMode -> Parser s Value
jsonP' mode =
  Number <$> numberP
    <|> nullP
    <|> Bool <$> boolP
    <|> String <$> stringP
    <|> Array <$> arrayP mode
    <|> Object <$> dictP mode

arrayP :: ParseInput s => ParseMode -> Parser s Array
arrayP mode =
  let next (Just firstElem) = return (Just (firstElem, Nothing))
      next Nothing = do
        jsonFiller mode
        fmap (fmap (,Nothing)) $
          case mode of
            JSON -> optional (C.char ',' *> jsonP JSON <* jsonFiller JSON)
            JSON5 -> do
              mC <- optional $ C.char ',' *> jsonFiller JSON5
              case mC of
                Nothing -> return Nothing
                Just _ -> optional $ jsonP' JSON5 <* jsonFiller JSON5
   in C.char '['
        *> jsonFiller mode
        *> ( (jsonP' mode >>= V.unfoldrM next . Just)
               <|> pure V.empty
           )
          <* C.char ']'

dictP :: ParseInput s => ParseMode -> Parser s Object
dictP mode =
  let dictKeyPair = do
        key <-
          case mode of
            JSON -> stringP
            JSON5 -> stringP <|> identifyerNameP
        jsonFiller mode
        void $ C.char ':'
        val <- jsonP mode
        return (key, val)
      dictElem = C.char ',' *> jsonFiller mode *> dictKeyPair
      nonEmptyContents !acc = do
        jsonFiller mode
        (dictElem >>= nonEmptyContents . flip (:) acc)
          <|> (C.char '}' $> HM.fromList acc)
   in C.char '{'
        *> jsonFiller mode
        *> ( (dictKeyPair >>= nonEmptyContents . (: []))
               <|> HM.empty <$ C.char '}'
           )

identifyerNameP :: ParseInput s => Parser s T.Text
identifyerNameP = do
  let identifyerStart = C.letterChar <|> oneOf ("$_" :: String) <|> unicodeChar
      identifierPart = identifyerStart <|> C.digitChar
  first <- identifyerStart
  rest <- many identifierPart
  return . T.pack $ first : rest

nullP :: ParseInput s => Parser s Value
nullP = C.string "null" $> Null

boolP :: ParseInput s => Parser s Bool
boolP = C.string "true" $> True <|> C.string "false" $> False

data Sign = Positive | Negative

numberP :: forall s. ParseInput s => Parser s Scientific
numberP = do
  let nonZeroLeadingInt :: Num i => Parser s i
      nonZeroLeadingInt = (C.char '0' $> 0) <|> L.decimal
  let signedInt :: Num i => Parser s (Sign, i)
      signedInt =
        C.char '-' *> fmap ((Negative,) . negate) nonZeroLeadingInt
          <|> fmap (Positive,) nonZeroLeadingInt
  let signedIntPositive :: Num i => Parser s (Sign, i)
      signedIntPositive = C.char '+' *> fmap (Positive,) nonZeroLeadingInt <|> signedInt
  (intSign, intPart) <- signedInt
  let parseFractional = do
        void $ C.char '.'
        offsetBefore <- getOffset
        fractionalInts <- L.decimal
        offsetAfter <- getOffset
        let fractionalPartRaw = scientific fractionalInts (offsetBefore - offsetAfter)
            fractionalPart =
              case intSign of
                Positive -> fractionalPartRaw
                Negative -> negate fractionalPartRaw
        return $ intPart + fractionalPart
  rawNumber <- parseFractional <|> pure intPart
  let parseExponent = do
        void $ C.char 'e' <|> C.char 'E'
        (_sign, e) <- signedIntPositive
        return (scientific (coefficient rawNumber) (base10Exponent rawNumber + e))
  parseExponent <|> pure rawNumber

stringP :: ParseInput s => Parser s T.Text
stringP = C.char '"' *> go mempty
  where
    go !acc =
      (C.char '"' $> TL.toStrict (TLB.toLazyText acc)) <|> (chars >>= go . (acc <>))

chars :: forall s. ParseInput s => Parser s TLB.Builder
chars =
  (C.string "\\\"" $> "\"")
    <|> (C.string "\\\\" $> "\\")
    <|> (C.string "\\/" $> "/")
    <|> (C.string "\\b" $> "\b")
    <|> (C.string "\\f" $> "\f")
    <|> (C.string "\\n" $> "\n")
    <|> (C.string "\\r" $> "\r")
    <|> (C.string "\\t" $> "\t")
    <|> fmap TLB.singleton unicodeChar
    <|> fmap
      (toBuilder (Proxy @s))
      (takeWhile1P Nothing (\c -> not (c == '"' || c == '\\' || (c <= chr 0x1f && isControl c))))

unicodeChar :: ParseInput s => Parser s Char
unicodeChar = do
  void $ C.string "\\u"
  u1 <- fmap digitToInt C.hexDigitChar
  u2 <- fmap digitToInt C.hexDigitChar
  u3 <- fmap digitToInt C.hexDigitChar
  u4 <- fmap digitToInt C.hexDigitChar
  return $! chr (shiftL u1 12 .|. shiftL u2 8 .|. shiftL u3 4 .|. u4)

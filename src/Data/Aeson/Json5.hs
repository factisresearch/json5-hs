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
import Data.CaseInsensitive
import Data.Char
import Data.Functor
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
import Data.Aeson.Key (fromText)

data ParseMode = JSON | JSON5

{-# SPECIALIZE parseJson :: T.Text -> Either String Value #-}
{-# SPECIALIZE parseJson :: TL.Text -> Either String Value #-}
{-# SPECIALIZE parseJson :: String -> Either String Value #-}
parseJson :: ParseInput s => s -> Either String Value
parseJson = parseJson' JSON

{-# SPECIALIZE parseJson5 :: T.Text -> Either String Value #-}
{-# SPECIALIZE parseJson5 :: TL.Text -> Either String Value #-}
{-# SPECIALIZE parseJson5 :: String -> Either String Value #-}
parseJson5 :: ParseInput s => s -> Either String Value
parseJson5 = parseJson' JSON5

parseJson' :: ParseInput s => ParseMode -> s -> Either String Value
parseJson' mode s =
  case runParser (jsonP mode <* jsonFiller mode <* eof) "" s of
    Left e -> Left (errorBundlePretty e)
    Right x -> Right x

class (
  VisualStream s, TraversableStream s,
  Token s ~ Char, IsString (Tokens s), FoldCase (Tokens s)) => ParseInput s where
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
        oneOf
          [ ' ',
            '\t',
            '\n',
            '\r'
          ]
      json5Space =
        oneOf . fmap chr $
          [ 0x9,
            0xa,
            0xb,
            0xc,
            0xd,
            0x20,
            0xa0,
            0xa0,
            0x2028,
            0x2029,
            0xfeff
          ]
   in case mode of
        JSON -> skipMany jsonSpace
        JSON5 ->
          L.space
            (skipSome json5Space)
            (C.string "//" *> void (takeWhileP (Just "character") (\c -> c /= '\n' && c /= '\r')))
            (L.skipBlockComment "/*" "*/")

escapePrefix :: ParseInput s => Parser s ()
escapePrefix = void $ C.char '\\'

jsonP :: ParseInput s => ParseMode -> Parser s Value
jsonP mode =
  jsonFiller mode *> jsonP' mode

jsonP' :: ParseInput s => ParseMode -> Parser s Value
jsonP' mode =
  Number <$> numberP mode
    <|> nullP
    <|> Bool <$> boolP
    <|> String <$> stringP mode
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

dictP :: forall s. ParseInput s => ParseMode -> Parser s Object
dictP mode =
  let dictKeyPair = do
        key <-
          case mode of
            JSON -> stringP JSON
            JSON5 -> stringP JSON5 <|> identifyerNameP
        jsonFiller mode
        void $ C.char ':'
        val <- jsonP mode
        return (fromText key .= val)
      nonEmptyContents :: Object -> Parser s Object
      nonEmptyContents !acc = do
        jsonFiller mode
        let closing = C.char '}' $> acc
        case mode of
          JSON ->
            (C.char ',' *> jsonFiller JSON *> dictKeyPair >>= nonEmptyContents . (<> acc))
              <|> closing
          JSON5 -> do
            mC <- optional $ C.char ',' *> jsonFiller JSON5
            case mC of
              Nothing -> closing
              Just _ -> (dictKeyPair >>= nonEmptyContents . (<> acc)) <|> closing
   in C.char '{'
        *> jsonFiller mode
        *> ( (dictKeyPair >>= nonEmptyContents)
               <|> mempty <$ C.char '}'
           )

identifyerNameP :: ParseInput s => Parser s T.Text
identifyerNameP = label "ES5 IdentifierName" $ do
  let identifyerStart = C.letterChar <|> oneOf ("$_" :: String) <|> (escapePrefix *> unicodeChar)
      identifierPart = identifyerStart <|> C.digitChar
  first <- identifyerStart
  rest <- many identifierPart
  return . T.pack $ first : rest

nullP :: ParseInput s => Parser s Value
nullP = C.string "null" $> Null

boolP :: ParseInput s => Parser s Bool
boolP = C.string "true" $> True <|> C.string "false" $> False

numberP :: forall s. ParseInput s => ParseMode -> Parser s Scientific
numberP mode = do
  let signMode =
        case mode of
          JSON -> OnlyNegative
          JSON5 -> AllowPositive
  sign <- signP signMode
  fmap sign $
    case mode of
      JSON -> numberRegular JSON
      JSON5 ->
        (C.string' "0x" *> L.hexadecimal)
          <|> numberRegular JSON5

data SignMode = OnlyNegative | AllowPositive

signP :: (ParseInput s, Num a) => SignMode -> Parser s (a -> a)
signP mode =
  case mode of
    OnlyNegative -> (C.string "-" $> negate) <|> pure id
    AllowPositive -> (C.string "-" $> negate) <|> (C.string "+" $> id) <|> pure id

numberRegular :: forall s. ParseInput s => ParseMode -> Parser s Scientific
numberRegular mode = do
  let nonZeroLeadingInt :: Num i => Parser s i
      nonZeroLeadingInt = label "integer (without leading 0)" $ (C.char '0' $> 0) <|> L.decimal
  let parseFractional = do
        offsetBefore <- getOffset
        fractionalInts <- L.decimal
        offsetAfter <- getOffset
        let fractionalPart = scientific fractionalInts (offsetBefore - offsetAfter)
        return fractionalPart
  rawNumber <-
    case mode of
      JSON -> do
        intPart <- nonZeroLeadingInt
        fmap (+ intPart) (C.char '.' *> parseFractional) <|> pure intPart
      JSON5 -> do
        let withIntPart = do
              intPart <- nonZeroLeadingInt
              mC <- optional $ C.char '.'
              case mC of
                Nothing -> pure intPart
                Just _ ->
                  fmap (+ intPart) parseFractional <|> pure intPart
        withIntPart <|> (C.char '.' *> parseFractional)
  let parseExponent = do
        void $ C.char 'e' <|> C.char 'E'
        signE <- signP AllowPositive
        rawE <- nonZeroLeadingInt
        let e = signE rawE
        return (scientific (coefficient rawNumber) (base10Exponent rawNumber + e))
  parseExponent <|> pure rawNumber

data StringMode = StringJSON | StringSingle | StringDouble

stringP :: ParseInput s => ParseMode -> Parser s T.Text
stringP mode =
  case mode of
    JSON -> C.char '"' *> go StringJSON mempty
    JSON5 -> (C.char '"' *> go StringDouble mempty) <|> (C.char '\'' *> go StringSingle mempty)
  where
    go stringMode !acc =
      case stringMode of
        StringJSON -> (C.char '"' $> TL.toStrict (TLB.toLazyText acc)) <|> (charsJSON >>= go stringMode . (acc <>))
        StringDouble -> (C.char '"' $> TL.toStrict (TLB.toLazyText acc)) <|> (charsDouble >>= go stringMode . (acc <>))
        StringSingle -> (C.char '\'' $> TL.toStrict (TLB.toLazyText acc)) <|> (charsSingle >>= go stringMode . (acc <>))

singleCharJSON :: ParseInput s => Parser s TLB.Builder
singleCharJSON =
  (C.char '\"' $> "\"")
    <|> (C.char '\\' $> "\\")
    <|> (C.char '/' $> "/")
    <|> (C.char 'b' $> "\b")
    <|> (C.char 'f' $> "\f")
    <|> (C.char 'n' $> "\n")
    <|> (C.char 'r' $> "\r")
    <|> (C.char 't' $> "\t")
    <|> fmap TLB.singleton unicodeChar

charsJSON :: forall s. ParseInput s => Parser s TLB.Builder
charsJSON =
  (escapePrefix *> singleCharJSON)
    <|> fmap
      (toBuilder (Proxy @s))
      (label "unescaped char" $ takeWhile1P Nothing (\c -> not (c == '"' || c == '\\' || (c <= chr 0x1f && isControl c))))

singleCharJSON5 :: ParseInput s => Parser s TLB.Builder
singleCharJSON5 =
  singleCharJSON
    <|> (C.char '\'' $> "\'")
    <|> (C.char '\0' $> "\0")
    <|> (C.char '\v' $> "\v")
    <|> (C.char '\r' *> optional (C.char '\n') $> "")
    <|> (C.char '\n' $> "")

charsDouble :: forall s. ParseInput s => Parser s TLB.Builder
charsDouble =
  (escapePrefix *> singleCharJSON5)
    <|> fmap
      (toBuilder (Proxy @s))
      (label "unescaped char" $ takeWhile1P Nothing (\c -> not (c == '"' || c == '\\' || (c <= chr 0x1f && isControl c))))

charsSingle :: forall s. ParseInput s => Parser s TLB.Builder
charsSingle =
  (escapePrefix *> singleCharJSON5)
    <|> fmap
      (toBuilder (Proxy @s))
      (label "unescaped char" $ takeWhile1P Nothing (\c -> not (c == '\'' || c == '\\' || (c <= chr 0x1f && isControl c))))

unicodeChar :: ParseInput s => Parser s Char
unicodeChar = do
  void $ C.char 'u'
  u1 <- fmap digitToInt C.hexDigitChar
  u2 <- fmap digitToInt C.hexDigitChar
  u3 <- fmap digitToInt C.hexDigitChar
  u4 <- fmap digitToInt C.hexDigitChar
  return $! chr (shiftL u1 12 .|. shiftL u2 8 .|. shiftL u3 4 .|. u4)

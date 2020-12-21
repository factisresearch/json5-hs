{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aeson.Json5
  ( parseJson,
  )
where

import Control.Applicative (Applicative (..))
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Scientific (Scientific)
import Data.String
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

parseJson :: StreamContstraint s => s -> Either String Value
parseJson s =
  case runParser (jsonP <* jsonFiller <* eof) "" s of
    Left e -> Left (errorBundlePretty e)
    Right x -> Right x

type StreamContstraint s = (Stream s, Token s ~ Char, IsString (Tokens s))

type Parser s a = Parsec Void s a

jsonFiller :: StreamContstraint s => Parser s ()
jsonFiller = C.space

jsonP :: StreamContstraint s => Parser s Value
jsonP =
  jsonFiller *> jsonP'

jsonP' :: StreamContstraint s => Parser s Value
jsonP' =
  Number <$> numberP
    <|> nullP
    <|> Bool <$> boolP
    <|> String <$> stringP
    <|> Array <$> arrayP
    <|> Object <$> dictP

arrayP :: StreamContstraint s => Parser s Array
arrayP =
  let arrayElem = C.char ',' *> jsonP
      nonEmptyContents = liftA2 (:) jsonP' (many arrayElem) <* jsonFiller
   in C.char '['
        *> jsonFiller
        *> (fmap V.fromList nonEmptyContents <|> pure V.empty) <* C.char ']'

dictP :: StreamContstraint s => Parser s Object
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
          <|> (C.char '}' *> pure (HM.fromList acc))
   in C.char '{'
        *> jsonFiller
        *> ( (dictKeyPair >>= nonEmptyContents . (:[]))
               <|> pure HM.empty <* C.char '}'
           )

nullP :: StreamContstraint s => Parser s Value
nullP = C.string "null" *> pure Null

boolP :: StreamContstraint s => Parser s Bool
boolP = C.string "true" *> pure True <|> C.string "false" *> pure False

numberP :: StreamContstraint s => Parser s Scientific
numberP =
  L.signed (return ()) L.scientific

stringP :: StreamContstraint s => Parser s T.Text
stringP = C.char '"' *> fmap T.pack (manyTill L.charLiteral (C.char '"'))

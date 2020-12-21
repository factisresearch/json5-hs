{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aeson.Json5
  ( parseJson,
  )
where

import Data.Aeson
import Data.Scientific (Scientific)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

parseJson :: StreamContstraint s => s -> Either String Value
parseJson s =
  case runParser (space *> jsonP <* space <* eof) "" s of
    Left e -> Left (errorBundlePretty e)
    Right x -> Right x

type StreamContstraint s = (Stream s, Token s ~ Char)

type Parser s a = Parsec Void s a

jsonP :: StreamContstraint s => Parser s Value
jsonP = primitiveP

primitiveP :: StreamContstraint s => Parser s Value
primitiveP =
  Number <$> numberP

numberP :: StreamContstraint s => Parser s Scientific
numberP =
  L.signed (return ()) $
  L.scientific

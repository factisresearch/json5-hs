{-# LANGUAGE TypeFamilies #-}
module Data.Aeson.Json5
  ( parseJson5,
  )
where

import Data.Aeson
import Text.Megaparsec

parseJson5 :: (Stream s, Token s ~ Char) => s -> Either String Value
parseJson5 _ = Left "Not implemented"

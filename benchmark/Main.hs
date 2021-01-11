{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Criterion.Main
import Data.Aeson as Aeson
import Data.Aeson.Json5 as Json5
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import System.Directory
import System.FilePath.Posix

main :: IO ()
main = do
  let benchDir = "benchmark/json-data"
  allFiles <- listDirectory benchDir
  let doDecode bs =
        case T.decodeUtf8' bs of
          Left e -> Left (show e)
          Right x -> Right x
  let mkBenchForFile jsonFile =
        env (BS.readFile (benchDir </> jsonFile)) $ \jsonBs ->
          bgroup
            jsonFile
            [ bench "aeson-decode" $ nf (Aeson.eitherDecodeStrict :: BS.ByteString -> Either String Aeson.Value) jsonBs,
              bench "parseJson" $ nf (doDecode >=> Json5.parseJson) jsonBs,
              bench "parseJson5" $ nf (doDecode >=> Json5.parseJson5) jsonBs
            ]
  defaultMain (map mkBenchForFile allFiles)

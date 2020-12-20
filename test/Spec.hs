{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Aeson
import Data.Aeson.Json5
import Data.Maybe
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath.Posix
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  let testDir = "json5-tests"
  allEntries <- listDirectory testDir
  allTestGroups <-
    fmap catMaybes $
      forM allEntries $ \dir -> do
        let dir' = testDir </> dir
        isDirectory <- doesDirectoryExist dir'
        case isDirectory of
          False -> return Nothing
          True -> do
            allTestFiles <- listDirectory dir'
            allTests <-
              fmap catMaybes $
                forM allTestFiles $ \testFile -> do
                  let testFile' = dir' </> testFile
                  let testParse = do
                        contents <- T.readFile testFile'
                        return . fmap (contents,) $ parseJson5 contents
                  return . fmap (testCase testFile) $
                    case takeExtension testFile' of
                      ".errorSpec" -> Nothing
                      ".json" -> Just $ do
                        res <- testParse
                        case res of
                          Left e -> assertFailure e
                          Right (contents, x) -> do
                            case eitherDecodeStrict' (T.encodeUtf8 contents) of
                              Left e -> assertFailure $ "aeson failed: " <> e
                              Right aesonRes -> assertEqual "" aesonRes x
                      ".json5" -> Nothing -- FIXME
                      ".txt" -> Just $ do
                        res <- testParse
                        case res of
                          Left _e -> return ()
                          Right x -> assertFailure $ "Unexpected successful parse: " ++ show x
                      ".js" -> Just $ do
                        res <- testParse
                        case res of
                          Left _e -> return ()
                          Right x -> assertFailure $ "Unexpected successful parse: " ++ show x
                      ext -> Just $ assertFailure $ "File with unexpected extension: " ++ ext
            return . Just $ testGroup dir allTests
  defaultMain $ testGroup "json5-tests" allTestGroups

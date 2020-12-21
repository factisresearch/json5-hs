{-# LANGUAGE RecordWildCards #-}

import Control.Arrow
import Control.Monad
import Data.Aeson
import Data.Aeson.Json5
import qualified Data.ByteString as BS
import Data.Maybe
import qualified Data.Text.Encoding as T
import Language.JavaScript.Inline
import System.Directory
import System.FilePath.Posix
import Test.Tasty
import Test.Tasty.HUnit

data TestSpec = TestSpec
  { jsonShouldParse :: Bool,
    testName :: String,
    testFile :: FilePath
  }

mkTests :: TestSpec -> TestTree
mkTests testSpec =
  let jsonTest = do
        bs <- BS.readFile (testFile testSpec)
        case (jsonShouldParse testSpec, parseJson =<< left show (T.decodeUtf8' bs)) of
          (False, Left _e) -> return ()
          (False, Right x) ->
            assertFailure $
              "Unexpected successful parse: " <> show x <> "\ninput: " <> show bs
          (True, Left e) -> assertFailure $ "failed parse:\n" <> e <> "\ninput: " <> show bs
          (True, Right jsonRes) ->
            case eitherDecodeStrict' bs of
              Left e -> assertFailure $ "Aeson failed to parse:\n" <> e
              Right aesonRes -> assertEqual ("input: " <> show bs) aesonRes jsonRes
   in testCase (testName testSpec) jsonTest

json5Tests :: Session -> IO TestTree
json5Tests _session = do
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
                  let mkTestSpec jsonShouldParse =
                        Just
                          TestSpec
                            { testName = testFile,
                              testFile = dir' </> testFile,
                              ..
                            }
                  let mTestSpec =
                        case takeExtension testFile of
                          ".json" -> mkTestSpec True
                          ".json5" -> mkTestSpec False
                          ".txt" -> mkTestSpec False
                          ".js" -> mkTestSpec False
                          _ext -> Nothing
                  return . fmap mkTests $ mTestSpec
            return . Just $ testGroup dir allTests
  return $ testGroup "json5-tests" allTestGroups

jsonTestSuite :: Session -> IO TestTree
jsonTestSuite _session = do
  let testDir = "JSONTestSuite/test_parsing"
  allEntries <- listDirectory testDir
  let allTests =
        flip mapMaybe allEntries $ \testFile ->
          let mkTestSpec jsonShouldParse =
                Just
                  TestSpec
                    { testName = testFile,
                      testFile = testDir </> testFile,
                      ..
                    }
           in case testFile of
                'y' : _ -> mkTestSpec True
                'n' : _ -> mkTestSpec False
                _ -> Nothing
  return $ testGroup "JSONTestSuite" $ map mkTests allTests

main :: IO ()
main =
  withSession defaultConfig $ \session -> do
    json5TestSuite <- json5Tests session
    jsonTests <- jsonTestSuite session
    defaultMain $ testGroup "Tests" [jsonTests, json5TestSuite]

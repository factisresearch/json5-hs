{-# LANGUAGE RecordWildCards #-}

import Control.Arrow
import Control.Monad
import Data.Aeson
import Data.Aeson.Json5
import qualified Data.ByteString as BS
import Data.Maybe
import qualified Data.Text.Encoding as T
import Scripting.Duktape
import System.Directory
import System.FilePath.Posix
import Test.Tasty
import Test.Tasty.HUnit

data TestSpec = TestSpec
  { testMode :: TestMode,
    testName :: String,
    testFile :: FilePath
  }

data TestMode
  = JSON
  | JSON5
  | MaybeJSON5
  | NoParse
  deriving (Eq)

mkTests :: TestSpec -> TestTree
mkTests testSpec =
  let jsonTest = testCase "JSON" $ do
        bs <- BS.readFile (testFile testSpec)
        let jsonShouldParse = testMode testSpec == JSON
        case (jsonShouldParse, parseJson =<< left show (T.decodeUtf8' bs)) of
          (False, Left _e) -> return ()
          (False, Right x) ->
            assertFailure $
              "Unexpected successful parse: " <> show x <> "\ninput: " <> show bs
          (True, Left e) -> assertFailure $ "failed parse:\n" <> e <> "\ninput: " <> show bs
          (True, Right jsonRes) ->
            case eitherDecodeStrict' bs of
              Left e -> assertFailure $ "Aeson failed to parse:\n" <> e
              Right aesonRes -> assertEqual ("input: " <> show bs) aesonRes jsonRes
      mJson5ShoulddParse =
        case testMode testSpec of
          JSON -> Just True
          JSON5 -> Just True
          MaybeJSON5 -> Nothing
          NoParse -> Just False
      mJson5Test = flip fmap mJson5ShoulddParse $ \json5ShouldParse -> testCase "JSON5" $ do
        bs <- BS.readFile (testFile testSpec)
        case (json5ShouldParse, parseJson5 =<< left show (T.decodeUtf8' bs)) of
          (False, Left _e) -> return ()
          (False, Right x) ->
            assertFailure $
              "Unexpected successful parse: " <> show x <> "\ninput: " <> show bs
          (True, Left e) -> assertFailure $ "failed parse:\n" <> e <> "\ninput: " <> show bs
          (True, Right json5Res) -> do
            Just ctx <- createDuktapeCtx
            Right (Just res) <- evalDuktape ctx bs
            assertEqual "" res json5Res
   in testGroup (testName testSpec) (catMaybes [Just jsonTest, mJson5Test])

json5Tests :: IO TestTree
json5Tests = do
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
                  let mkTestSpec testMode =
                        Just
                          TestSpec
                            { testName = testFile,
                              testFile = dir' </> testFile,
                              ..
                            }
                  let mTestSpec =
                        case takeExtension testFile of
                          ".json" -> mkTestSpec JSON
                          ".json5" -> mkTestSpec JSON5
                          ".txt" -> mkTestSpec NoParse
                          ".js" -> mkTestSpec NoParse
                          _ext -> Nothing
                  return . fmap mkTests $ mTestSpec
            return . Just $ testGroup dir allTests
  return $ testGroup "json5-tests" allTestGroups

jsonTestSuite :: IO TestTree
jsonTestSuite = do
  let testDir = "JSONTestSuite/test_parsing"
  allEntries <- listDirectory testDir
  let allTests =
        flip mapMaybe allEntries $ \testFile ->
          let mkTestSpec testMode =
                Just
                  TestSpec
                    { testName = testFile,
                      testFile = testDir </> testFile,
                      ..
                    }
           in case testFile of
                'y' : _ -> mkTestSpec JSON
                'n' : _ -> mkTestSpec MaybeJSON5
                _ -> Nothing
  return $ testGroup "JSONTestSuite" $ map mkTests allTests

main :: IO ()
main = do
  json5TestSuite <- json5Tests
  jsonTests <- jsonTestSuite
  defaultMain $ testGroup "Tests" [jsonTests, json5TestSuite]

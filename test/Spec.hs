{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Aeson
import Data.Aeson.Json5
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
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
        contents <- T.readFile (testFile testSpec)
        case (jsonShouldParse testSpec, parseJson contents) of
          (False, Left _e) -> return ()
          (False, Right x) ->
            assertFailure $
              "Unexpected successful parse: "
                <> show x
                <> "\nInput:\n"
                <> T.unpack contents
          (True, Left e) -> assertFailure $ "Failed parse:\n" <> e
          (True, Right jsonRes) ->
            case eitherDecodeStrict' (T.encodeUtf8 contents) of
              Left e -> assertFailure $ "Aeson failed to parse:\n" <> e
              Right aesonRes -> assertEqual "" aesonRes jsonRes
   in testGroup (testName testSpec) [testCase "JSON" jsonTest]

main :: IO ()
main =
  withSession defaultConfig $ \session -> do
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
                            ext -> Nothing
                    return . fmap mkTests $ mTestSpec
              return . Just $ testGroup dir allTests
    defaultMain $ testGroup "json5-tests" allTestGroups

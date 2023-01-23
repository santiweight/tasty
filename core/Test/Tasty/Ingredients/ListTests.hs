-- | Ingredient for listing test names
{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Tasty.Ingredients.ListTests
  ( ListTests(..)
  , testsNames
  , listingTests
  , reportTestTree
  ) where

import Data.Proxy
import Data.Typeable
import Options.Applicative

import Test.Tasty.Core
import Test.Tasty.Options
import Test.Tasty.Ingredients
import GHC.Stack (SrcLoc(..))
import Data.Bifunctor (Bifunctor(..))
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

-- | This option, when set to 'True', specifies that we should run in the
-- «list tests» mode.
--
-- @since 0.4
newtype ListTests = ListTests Bool
  deriving (Eq, Ord, Typeable)
instance IsOption ListTests where
  defaultValue = ListTests False
  parseValue = fmap ListTests . safeReadBool
  optionName = return "list-tests"
  optionHelp = return "Do not run the tests; just print their names"
  optionCLParser = mkFlagCLParser (short 'l') (ListTests True)

-- | Obtain the list of all tests in the suite.
--
-- @since 0.4
testsNames :: OptionSet -> TestTree -> [TestName]
testsNames {- opts -} {- tree -} =
  foldTestTree
    trivialFold
      { foldSingle = \ _opts name _test -> [fst name]
      , foldGroup = \ _opts groupName names -> map ((fst groupName ++ ".") ++) names
      }

-- | The ingredient that provides the test listing functionality.
--
-- @since 0.4
listingTests :: Ingredient
listingTests = TestManager [Option (Proxy :: Proxy ListTests)] $
  \opts tree ->
    case lookupOption opts of
      ListTests False -> Nothing
      ListTests True -> Just $ do
        mapM_ putStrLn $ testsNames opts tree
        return True

-- | This option, when set to 'True', specifies that we should run in the
-- «list tests» mode.
--
-- @since 0.4
newtype TestTreeInfo = TestTreeInfo Bool
  deriving (Eq, Ord, Typeable)
instance IsOption TestTreeInfo where
  defaultValue = TestTreeInfo False
  parseValue = fmap TestTreeInfo . safeReadBool
  optionName = return "tree-info"
  optionHelp = return "Do not run the tests; just print their names"
  optionCLParser = mkFlagCLParser (short 'i') (TestTreeInfo True)

data TestLoc = TestLoc {file::String, line::Int,column::Int}
  deriving stock (Generic)
  deriving anyclass ToJSON

srcLocToTestLoc :: SrcLoc -> TestLoc
srcLocToTestLoc = \case
  SrcLoc {..} -> TestLoc {file = srcLocFile, line = srcLocStartLine, column = srcLocStartCol}

data TestTreeInfoResult =
  TestCaseInfo {name::TestName,loc:: TestLoc}
  | TestGroupInfo {name::TestName,loc:: TestLoc,children:: [TestTreeInfoResult]}
  deriving stock (Generic)
  deriving anyclass ToJSON

-- | Obtain the list of all tests in the suite.
--
-- @since 0.4
testTreeInfo :: OptionSet -> TestTree -> [TestTreeInfoResult]
testTreeInfo {- opts -} {- tree -} =
  foldTestTree
    trivialFold
      { foldSingle = \ _opts (name, (_, srcLoc)) _test -> [TestCaseInfo name $ srcLocToTestLoc srcLoc]
      -- , foldGroup = \_opts groupName names -> map ((groupName ++ ".") ++) names
      , foldGroup = \ _opts (groupName, (_, srcLoc)) names -> [TestGroupInfo groupName (srcLocToTestLoc srcLoc) names]
      }

-- | The ingredient that provides the test listing functionality.
--
-- @since 0.4
reportTestTree :: Ingredient
reportTestTree = TestManager [Option (Proxy :: Proxy TestTreeInfo)] $
  \opts tree ->
    case lookupOption opts of
      TestTreeInfo False -> Nothing
      TestTreeInfo True -> Just $ do
        mapM_ (BS.putStrLn . encode) $ testTreeInfo opts tree
        return True
  where
    displayLocTestName (testName, (_, SrcLoc _ _ locFile startLine _ _ _)) = testName <> " " <> locFile <> " " <> show startLine

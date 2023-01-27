-- | Ingredient for listing test names
{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
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
import qualified Data.Map.Strict as Map
import Data.Functor ((<&>))

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

-- The names of the children must be unique
newtype TestGroupChildren = UnsafeTestGroupChildren {unTestGroupChildren :: [TestTreeInfoResult]}
  deriving newtype (ToJSON, Semigroup, Monoid)

newtype DuplicateTestsError = DuplicateTestsError {dupes :: [String]}
  deriving stock (Generic)
  deriving anyclass ToJSON

mkTestGroupChildren :: [TestTreeInfoResult] -> Either [String] TestGroupChildren
mkTestGroupChildren childs =
  let names = name <$> childs
      nameFreqs = Map.fromListWith (+) $ zip names (repeat (1 :: Int))
      dupedNames = fmap fst . filter ((> 1) . snd) $ Map.toList nameFreqs
  in case dupedNames of
      [] -> Right $ UnsafeTestGroupChildren childs
      _ -> Left dupedNames


data TestTreeInfoResult =
  TestCaseInfo {name::TestName,loc:: TestLoc}
  | TestGroupInfo {name::TestName,loc:: TestLoc,children:: TestGroupChildren}
  deriving stock (Generic)
  deriving anyclass ToJSON

newtype TestTreeResult = TestTreeResult {unTestTreeResult :: Either [String] TestGroupChildren}
  deriving newtype (ToJSON)

instance Semigroup TestTreeResult where
  (TestTreeResult t1) <> (TestTreeResult t2) = TestTreeResult $ case (t1, t2) of
      (Left e1, Left e2) -> Left $ e1 <> e2
      (Left e, _) -> Left e
      (_, Left e) -> Left e
      (Right r1, Right r2) -> Right $ r1 <> r2

newtype TestTreeSuccess = TestTreeSuccess {tree :: TestGroupChildren}
  deriving stock Generic
  deriving anyclass (ToJSON)



instance Monoid TestTreeResult where
  mempty = TestTreeResult $ Right mempty

-- | Obtain the list of all tests in the suite.
--
-- @since 0.4
testTreeInfo :: OptionSet -> TestTree -> TestTreeResult
testTreeInfo {- opts -} {- tree -} opts tree = TestTreeResult $ do
  testInfos <- unTestTreeResult $ foldTestTree
    trivialFold
      { foldSingle = \ _opts (name, (_, srcLoc)) _test -> TestTreeResult $ Right $ UnsafeTestGroupChildren [TestCaseInfo name $ srcLocToTestLoc srcLoc]
      -- , foldGroup = \_opts groupName names -> map ((groupName ++ ".") ++) names
      , foldGroup = \ _opts (groupName, (_, srcLoc)) (TestTreeResult childsEither) -> TestTreeResult $ childsEither >>= (\childs -> mkTestGroupChildren (unTestGroupChildren childs) <&> \checkedChilds -> UnsafeTestGroupChildren [TestGroupInfo groupName (srcLocToTestLoc srcLoc) checkedChilds])
      } opts tree
  mkTestGroupChildren $ unTestGroupChildren testInfos

-- | The ingredient that provides the test listing functionality.
--
-- @since 0.4
reportTestTree :: Ingredient
reportTestTree = TestManager [Option (Proxy :: Proxy TestTreeInfo)] $
  \opts tree ->
    case lookupOption opts of
      TestTreeInfo False -> Nothing
      TestTreeInfo True -> Just $ do
        case testTreeInfo opts tree of
          TestTreeResult (Left dupes) -> BS.putStrLn . encode $ DuplicateTestsError dupes
          TestTreeResult (Right res) -> BS.putStrLn . encode $ TestTreeSuccess res
        return True
  where
    displayLocTestName (testName, (_, SrcLoc _ _ locFile startLine _ _ _)) = testName <> " " <> locFile <> " " <> show startLine

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib (
    test,
) where

import Control.Exception (SomeException (SomeException), try)
import Control.Monad (foldM)
import Control.Monad.IO.Class
import Data.Data (Data)
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Traversable (for)
import Database.MongoDB hiding (lookup)
import System.Environment (getEnvironment)
import Test.Hspec
import Text.Read (readMaybe)

data Config = Config
    { hostName :: String
    , databaseName :: T.Text
    , userName :: T.Text
    , password :: T.Text
    , collect :: T.Text
    , replicaName :: Maybe ReplicaSetName
    }

setPID :: PortID
setPID = PortNumber 1

makeConfig :: IO Config
makeConfig = do
    envvars <- getEnvironment
    let config =
            Config
                <$> lookup "hname" envvars
                <*> (T.pack <$> lookup "dname" envvars)
                <*> (T.pack <$> lookup "uname" envvars)
                <*> (T.pack <$> lookup "pword" envvars)
                <*> (T.pack <$> lookup "coll" envvars)
                <*> pure (T.pack <$> lookup "repname" envvars)
    pure $ fromJust config

setupAuthConnection :: Config -> IO (Either T.Text Pipe)
setupAuthConnection Config{..} =
    let loginWith p = do
            is_logged_in <- access p master admin $ auth userName password
            if is_logged_in then pure $ Right p else pure $ Left "Login failed"
     in selectRep replicaName >>= \repset ->
            try (primary repset) >>= \case
                Left (SomeException _) ->
                    try (secondaryOk repset) >>= \case
                        Left (SomeException err) -> pure . Left . T.pack . show $ err
                        Right pipe -> loginWith pipe
                Right pipe -> loginWith pipe
  where
    selectRep = case replicaName of
        Nothing -> pure $ openReplicaSetSRV' hostName
        Just name -> pure $ openReplicaSet (name, [Host hostName (PortNumber 27017 :: PortID)])

runMongo :: Database -> Pipe -> Action IO a -> IO a
runMongo db_name p = access p master db_name

spec :: Spec
spec = do
    (config, pipe) <- initialization
    validateConnector pipe
    traverse_ (\f -> f config pipe) [testWritesWith, testDeleteWith, testReadsWith]
  where
    initialization = runIO $ do
        config <- makeConfig
        pipe <- setupAuthConnection config
        pure (config, pipe)
    validateConnector c =
        let desc = describe "Validate"
            as = it "Validate the MongoDB Atlas connector"
            target = case c of
                Left err -> print "Connector is not logged in!" >> undefined
                Right p -> do
                    verdict <- isClosed p
                    verdict `shouldBe` False
         in desc $ as target
    testDeleteWith Config{..} p =
        let desc = describe "Delete"
            as = it "Ensures deletion works"
            target = case p of
                Left err -> print "Connector is not logged in!" >> undefined
                Right c -> do
                    deleted <- runMongo databaseName c $ deleteAll collect []
                    print $ "'Deleted' results read: " ++ show deleted
                    failed deleted `shouldBe` False
         in desc $ as target
    testWritesWith Config{..} p =
        let desc = describe "Writes"
            as = it "Ensures writes work"
            target = case p of
                Left err -> print "Connector is not logged in!" >> undefined
                Right c -> do
                    let docs = map (\n -> ["test_doc" =: (n :: Int)]) [1 .. 3]
                    inserted <- runMongo databaseName c $ insertMany collect docs
                    length inserted `shouldBe` 3
         in desc $ as target
    testReadsWith Config{..} c =
        let desc = describe "Reads"
            as = it "Ensures reads work"
            target = case c of
                Left err -> print "Connector is not logged in!" >> undefined
                Right p -> do
                    let action = find (select [] collect)
                    docs <- runMongo databaseName p (action >>= rest)
                    length docs `shouldBe` 3
         in desc $ as target

test :: IO ()
test = hspec spec
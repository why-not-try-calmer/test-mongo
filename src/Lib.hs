{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib (
    test,
) where

import Control.Exception (SomeException (SomeException), try)
import Control.Monad (foldM, when)
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
    , is_replicaset :: Bool
    }
    deriving (Show)

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
                <*> ((== "true") . T.pack <$> lookup "is_replicaset" envvars)
    pure $ fromJust config

setupAuthConnection :: Config -> IO (Either T.Text Pipe)
setupAuthConnection Config{..} =
    if is_replicaset
        then do
            repset <- openReplicaSetSRV' hostName
            try (primary repset) >>= \case
                Left (SomeException _) ->
                    try (secondaryOk repset) >>= \case
                        Left (SomeException err) -> pure . Left . T.pack . show $ err
                        Right pipe -> loginWith pipe
                Right pipe -> loginWith pipe
        else connect (host hostName) >>= loginWith
  where
    loginWith p = do
        is_logged_in <- access p master admin $ auth userName password
        if is_logged_in then pure $ Right p else pure $ Left "Login failed"

runMongo :: Database -> Pipe -> Action IO a -> IO a
runMongo db_name p = access p master db_name

spec :: Spec
spec = do
    (config, pipe) <- initialization
    traverse_ (\f -> f config pipe) [testWritesWith, testReadsWith, testDeleteWith]
  where
    cleanSlate config p = case p of
        Left _ -> pure ()
        Right p -> do
            runMongo (databaseName config) p $ deleteAll (collect config) []
            pure ()
    initialization = runIO $ do
        config <- makeConfig
        pipe <- setupAuthConnection config
        cleanSlate config pipe
        when (is_replicaset config) $ print "You have connected to a *REPLICASET*"
        print $ "Full connection credentials: " ++ show config
        pure (config, pipe)
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

test :: IO ()
test = hspec spec
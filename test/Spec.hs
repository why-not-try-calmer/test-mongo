{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Exception (SomeException (SomeException), try)
import Control.Monad (foldM)
import Data.Data (Data)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Traversable (for)
import Database.MongoDB hiding (lookup)
import System.Environment (getEnvironment)
import Test.Hspec

{-
hostName :: String
hostName = "cluster0.xpugk.mongodb.net"

databaseName :: Database
databaseName = "test-db"

userName :: Username
userName = "tester"

password :: Password
password = "95BqF22T78gUUanH"

collec :: Collection
collec = "test-collection"
-}

data Config = Config
    { hostName :: String
    , databaseName :: T.Text
    , userName :: T.Text
    , password :: T.Text
    , collect :: T.Text
    }

makeConfig :: IO Config
makeConfig = do
    envvars <- getEnvironment
    pure . fromJust $
        Config
            <$> lookup "hname" envvars
            <*> (T.pack <$> lookup "dname" envvars)
            <*> (T.pack <$> lookup "uname" envvars)
            <*> (T.pack <$> lookup "pword" envvars)
            <*> (T.pack <$> lookup "coll" envvars)

setupAuthConnection :: Config -> IO (Either T.Text Pipe)
setupAuthConnection Config{..} =
    let loginWith p = do
            is_logged_in <- access p master admin $ auth userName password
            if is_logged_in then pure $ Right p else pure $ Left "Login failed"
     in openReplicaSetSRV' hostName >>= \repset ->
            try (primary repset) >>= \case
                Left (SomeException _) ->
                    try (secondaryOk repset) >>= \case
                        Left (SomeException err) -> pure . Left . T.pack . show $ err
                        Right pipe -> loginWith pipe
                Right pipe -> loginWith pipe

runMongo :: Database -> Pipe -> Action IO a -> IO a
runMongo db_name p = access p master db_name

spec :: Spec
spec = do
    (config, pipe) <- runIO $ do
        config <- makeConfig
        pipe <- setupAuthConnection config
        pure (config, pipe)
    validateConnector pipe >> testWritesWith config pipe >> testReadsWith config pipe
  where
    validateConnector c =
        let desc = describe "Validate the MongoDB Atlas connector"
            as = it "The connection should be established and login successful"
            target = case c of
                Left err -> print "Connector is not logged in!" >> undefined
                Right p -> do
                    verdict <- isClosed p
                    verdict `shouldBe` False
         in desc $ as target
    testWritesWith Config{..} p =
        let desc = describe "Tries writing to the db"
            as = it "Writes are OK"
            target = case p of
                Left err -> print "Connector is not logged in!" >> undefined
                Right c -> do
                    let docs = map (\n -> ["test_doc" =: (n :: Int)]) [1 .. 3]
                        action = do
                            deleteAll collect []
                            insertMany collect docs
                    res <- runMongo databaseName c action
                    length res `shouldBe` 3
         in desc $ as target
    testReadsWith Config{..} c =
        let desc = describe "Tries reading from the db"
            as = it "Reads are OK"
            target = case c of
                Left err -> print "Connector is not logged in!" >> undefined
                Right p -> do
                    let action = find (select [] collect)
                    docs <- runMongo databaseName p (action >>= rest)
                    length docs `shouldBe` 3
         in desc $ as target

main :: IO ()
main = hspec spec
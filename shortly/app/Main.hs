{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Main where

import qualified Database.MySQL.Base as MySQLBase
import qualified Web.Scotty.Internal.Types
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified GHC.Types
import GHC.Generics (Generic)
import GHC.Word (Word16)
import GHC.Int (Int64)
import Database.MySQL.Simple
import Database.MySQL.Simple.Types (Query (Query))
import Database.MySQL.Simple.QueryResults ( QueryResults )
import Database.MySQL.Simple.QueryParams ( QueryParams )
import Control.Monad (forM_)
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Pool (Pool, createPool, withResource)
import System.Random ( randomIO )
import Web.Scotty ( post, redirect, param, get, scotty, ActionM, json, setHeader, options, middleware, body, status, notFound, regex )
import Data.Text.Lazy (toStrict, fromStrict)
import Data.Text (unpack, pack)
import Data.Binary (encode)
import Data.Aeson
import Data.Maybe
import TokenService

data MySqlData = MySqlData {
    dbHost :: String,
    dbPort :: Word16,
    dbName :: String,
    dbUser :: String,
    dbPass :: String
} deriving (Show, Generic)

mySqlConnect :: MySqlData -> IO Connection
mySqlConnect data_ = connect defaultConnectInfo {
    connectHost = dbHost data_,
    connectUser = dbUser data_,
    connectPort = dbPort data_,
    connectPassword = dbPass data_,
    connectDatabase = dbName data_
}

mySqlFetch :: (QueryParams q, QueryResults r) => Connection -> Query -> q -> IO [r]
mySqlFetch = query

mySqlExecute :: QueryParams q => Connection -> Query -> q -> IO Int64
mySqlExecute = execute

mySqlGetConnection :: IO Connection
mySqlGetConnection = mySqlConnect (MySqlData "" 3306 "" "" "")

data ShUrlResult = ShUrlResult {
    identifier :: Int,
    real_url :: String,
    shortly_code :: String,
    access_count :: Int
}

data ShShortenResult = ShShortenResult {
    status_ :: Bool ,
    code :: String
}

shShortenUrl :: Connection -> [Char] -> IO ShShortenResult
shShortenUrl mysql url = do
    shToken <- shRandomToken "oPlVZ" 5
    last_inserted_id <- mySqlExecute mysql 
        "INSERT INTO shortly_urls (real_url, shortly_code) VALUES (?, ?);" 
            ((url, shToken) :: (String, String))
    return ShShortenResult {
        status_ = last_inserted_id /= 0,
        code = shToken
    }

shIsValidShortlyCode :: Connection -> [Char] -> IO Bool
shIsValidShortlyCode mysql code = do
    [Only n] <- mySqlFetch mysql
        "SELECT COALESCE(COUNT(*), 0) AS NumberOfCodes FROM shortly_urls WHERE shortly_code = ?;"
            (Only code) :: IO [Only Int]
    return (n /= 0)    

shGetDataByShortlyCode :: Connection -> [Char] -> IO (Maybe ShUrlResult)
shGetDataByShortlyCode mysql code = do
    isValid <- shIsValidShortlyCode mysql code
    if isValid then do
        rs <- mySqlFetch mysql 
            "SELECT id, real_url, shortly_code, access_count FROM shortly_urls WHERE shortly_code = ? ORDER BY id DESC LIMIT 1;" 
                (Only code) :: IO [(Int, String, String, Int)]
        let (id, real_url, shortly_code, access_count) = head rs
        return (Just ShUrlResult {
            identifier = id,
            real_url = real_url,
            shortly_code = shortly_code,
            access_count = access_count
        })
    else do
        return Nothing

shUpdateAccessCountById :: Connection -> Int -> IO ()
shUpdateAccessCountById mysql id = do
    mySqlExecute mysql 
        "UPDATE shortly_urls SET access_count = access_count + 1 WHERE id = ?;"
            (Only id)
    return ()

shStrToLazy :: String -> Lazy.Text 
shStrToLazy = fromStrict . pack

shLazyToStr :: Lazy.Text -> String
shLazyToStr = unpack . toStrict

data ShortenResponse = ShortenResponse { 
    shorten_result :: Bool,
    short_code :: String
} deriving (Generic, Show, ToJSON)

data ClicksResponse = ClicksResponse {
    clicks_result :: Bool,
    total_clicks :: Int
} deriving (Generic, Show, ToJSON)

shMiddleware :: Web.Scotty.Internal.Types.ActionT Lazy.Text IO ()
shMiddleware = do
    setHeader "Access-Control-Allow-Credentials" "true"
    setHeader "Access-Control-Allow-Headers" "*"
    setHeader "Access-Control-Allow-Methods" "GET, POST, OPTIONS, HEAD"
    setHeader "Access-Control-Allow-Origin" "*"

main :: IO ()
main = do
    mysql <- mySqlGetConnection
    scotty 3344 $ do

        options (regex "/^.*$/") $ do
            shMiddleware

        get "/c/:shortly_code" $ do
            shMiddleware
            sh_code <- param "shortly_code" :: ActionM Lazy.Text
            result <- liftIO $ shGetDataByShortlyCode mysql (shLazyToStr sh_code)
            case result of
                Nothing -> redirect "/"
                Just dataset -> do
                    liftIO $ shUpdateAccessCountById mysql (identifier dataset)
                    redirect $ shStrToLazy (real_url dataset)

        get "/c/:shortly_code/clicks" $ do
            shMiddleware
            sh_code <- param "shortly_code" :: ActionM Lazy.Text
            result <- liftIO $ shGetDataByShortlyCode mysql (shLazyToStr sh_code)
            case result of
                Nothing -> redirect "/"
                Just dataset -> do
                    let response = ClicksResponse {
                        clicks_result = True,
                        total_clicks = access_count dataset
                    }
                    Web.Scotty.json response

        post "/s" $ do
            shMiddleware
            real_url <- param "real_url" :: ActionM Lazy.Text
            shorten_url <- liftIO $ shShortenUrl mysql (shLazyToStr real_url)
            let response = ShortenResponse {
                shorten_result = status_ shorten_url,
                short_code = code shorten_url
            }
            Web.Scotty.json response


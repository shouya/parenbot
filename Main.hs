{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Web.Twitter.Conduit
import Web.Twitter.Types.Lens

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Default
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Network.HTTP.Conduit
import System.IO (hFlush, stdout)
import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import qualified Web.Authenticate.OAuth as OA
import System.Environment (getEnv)
import Data.Functor
import Data.Aeson.Types
import Data.Monoid

import Paren

getOAuthTokens :: IO (OAuth, Credential)
getOAuthTokens = do
    consumerKey <- getEnv' "OAUTH_CONSUMER_KEY"
    consumerSecret <- getEnv' "OAUTH_CONSUMER_SECRET"
    accessToken <- getEnv' "OAUTH_ACCESS_TOKEN"
    accessSecret <- getEnv' "OAUTH_ACCESS_SECRET"
    let oauth = twitterOAuth
            { oauthConsumerKey = consumerKey
            , oauthConsumerSecret = consumerSecret
            }
        cred = Credential
            [ ("oauth_token", accessToken)
            , ("oauth_token_secret", accessSecret)
            ]
    return (oauth, cred)
  where
    getEnv' = (B8.pack <$>) . getEnv

getTWInfoFromEnv :: IO TWInfo
getTWInfoFromEnv = do
    (oa, cred) <- getOAuthTokens
    return $ (setCredential oa cred def) { twProxy = Nothing }


main :: IO ()
main = do
    twInfo <- getTWInfoFromEnv
    putStrLn $ "starto"
    withManager $ \mgr -> do
      src <- stream twInfo mgr userstream
      src C.$$+- CL.mapM_ (liftIO . takeAction)


takeAction :: StreamingAPI -> IO ()
takeAction (SStatus st) = case matchParenT stText of
                           Just t  -> callRequest $ replyTo st (t <> face)
                           Nothing -> return ()
  where stText = st ^. statusText
        face   = "○(￣□￣○) #parenbot"
takeAction _            = return ()


callRequest :: (FromJSON b) => APIRequest a b -> IO ()
callRequest req = do
  twInfo <- getTWInfoFromEnv
  withManager $ \mgr -> do
    call twInfo mgr req
    return ()


replyTo :: Status -> T.Text -> APIRequest StatusesUpdate Status
replyTo t text = update tweetText & inReplyToStatusId ?~ (t ^. statusId)
  where tweetText = "@" <> t ^. statusUser ^. userScreenName <>
                    " " <> text


favTweet :: Status -> APIRequest FavoritesCreate Status
favTweet t = favoritesCreate (t ^. statusId)

matchParenT :: Text -> Maybe Text
matchParenT = fmap T.pack . matchParen . T.unpack

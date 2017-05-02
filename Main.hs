{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Web.Twitter.Conduit.Parameters
import Web.Twitter.Conduit hiding (inReplyToStatusId)
import Web.Twitter.Types.Lens

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Data.Text (Text)
import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import System.Environment (getEnv)
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
    putStrLn "Starting"
    mgr <- newManager tlsManagerSettings
    runResourceT $ do
      src <- stream twInfo mgr userstream
      liftIO $ putStrLn "User stream connected"
      src C.$$+- CL.mapM_ (liftIO . takeAction)


takeAction :: StreamingAPI -> IO ()
takeAction (SStatus st) = case matchParenT stText of
                           Just t  -> callRequest $ replyTo st (tweet t)
                           Nothing -> return ()
  where stText  = st ^. statusText
        crop    = T.take (140 - length face - 1)
        tweet t = crop t <> T.pack face
        face    = "○(￣□￣○)"
takeAction (SEvent e)
  | e ^. evEvent == "follow" =
    do let target = e ^. evTarget ^. to screenname
       let source = e ^. evSource ^. to screenname
       if target == "parenbot"
         then do callRequest $ followUser source
                 putStrLn $ "Followed " <> T.unpack source
         else return ()
  | otherwise = return ()
  where screenname :: EventTarget -> Text
        screenname target = case target ^? _ETUser of
          Just u -> u ^. userScreenName
          _      -> ""
takeAction _ = return ()


callRequest :: (FromJSON b) => APIRequest a b -> IO ()
callRequest req = do
  twInfo <- getTWInfoFromEnv
  mgr <- newManager tlsManagerSettings
  _ <- call twInfo mgr req
  return ()


replyTo :: Status -> T.Text -> APIRequest StatusesUpdate Status
replyTo t txt = update tweetText & inReplyToStatusId ?~ (t ^. statusId)
  where tweetText = "@" <> t ^. statusUser ^. userScreenName <>
                    " " <> txt

followUser :: Text -> APIRequest FriendshipsCreate User
followUser name = friendshipsCreate (ScreenNameParam $ T.unpack name)

favTweet :: Status -> APIRequest FavoritesCreate Status
favTweet t = favoritesCreate (t ^. statusId)

matchParenT :: Text -> Maybe Text
matchParenT = fmap T.pack . matchParen . T.unpack

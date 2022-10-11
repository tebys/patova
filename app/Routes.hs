{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes where

import           Control.Monad.IO.Class (liftIO)
import           Model
import           Control.Monad (unless)
import qualified Database.MongoDB as M
import qualified Web.Scotty as S
import           Network.HTTP.Types
import           Data.Text.Lazy (Text)
import qualified ModelWeb as ME
import           Data.Maybe (fromJust)
import           Data.Set
import qualified Data.Text as T

handleParseError :: Monoid a => Text -> S.ActionM a
handleParseError e = do
  S.text e
  S.status status400
  return mempty

createUser :: M.Pipe -> Set T.Text -> S.ActionM ()
createUser pipe initialClaims = do
  user <- S.jsonData `S.rescue` handleParseError
  unless (ME.isEmptyCreateUser user)
    $ do
      exists <- liftIO $ userExists pipe (ME.userName user)
      if exists
        then S.status status401
        else do
          newUser <- liftIO $ ME.newUser user
          insertUser pipe initialClaims newUser
          S.status status200

deleteUser :: M.Pipe -> S.ActionM ()
deleteUser pipe = do
  user <- S.param "user" `S.rescue` handleParseError
  unless (user == "")
    $ do
      removeUser pipe user
      S.status status501

deleteClaim :: M.Pipe -> S.ActionM ()
deleteClaim _ = S.status status501

addClaim :: M.Pipe -> Set T.Text -> S.ActionM ()
addClaim pipe allowedClaims = do
  user <- S.param "user" `S.rescue` handleParseError
  claim <- S.param "claim" `S.rescue` handleParseError
  exists <- liftIO $ userExists pipe user
  if user /= "" && claim /= "" && claim `member` allowedClaims && exists
    then do
      updated <- updateClaim pipe user claim True
      if updated
        then S.status status200
        else S.status status501
    else S.status status400

login :: M.Pipe -> S.ActionM ()
login pipe = do
  user <- S.jsonData `S.rescue` handleParseError
  unless (ME.isEmptyCreateUser user)
    $ do
      validUser <- liftIO $ getUser pipe (ME.userName user)
      if ME.validateLogin user validUser
        then do
          S.json $ fromJust validUser
          S.status status200
        else S.status status401
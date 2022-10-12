{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes where

import           Model
import qualified Database.MongoDB as M
import qualified Web.Scotty as S
import           Network.HTTP.Types
import           Data.Text.Lazy (Text)
import qualified ModelWeb as ME
import           Data.Maybe (fromJust)
import           Data.Set
import qualified Data.Text as T
import           Control.Monad.Reader

data Environment = Environment { pipe :: M.Pipe, allowedClaims :: Set T.Text }

handleParseError :: Monoid a => Text -> S.ActionM a
handleParseError e = do
  S.text e
  S.status status400
  return mempty

createUser :: ReaderT Environment S.ActionM ()
createUser = do
  Environment thePipe theAllowedClaims <- ask
  user <- lift $ S.jsonData `S.rescue` handleParseError
  lift
    $ unless (ME.isEmptyCreateUser user)
    $ do
      exists <- liftIO $ userExists thePipe (ME.userName user)
      if exists
        then S.status status401
        else do
          newUser <- liftIO $ ME.newUser user
          insertUser thePipe theAllowedClaims newUser
          S.status status200

deleteUser :: ReaderT Environment S.ActionM ()
deleteUser = do
  Environment thePipe _ <- ask
  user <- lift $ S.param "user" `S.rescue` handleParseError
  lift
    $ unless (user == "")
    $ do
      removeUser thePipe user
      S.status status501

changeClaim :: T.Text -> T.Text -> Bool -> ReaderT Environment IO Status
changeClaim user claim on = do
  Environment thePipe theAllowedClaims <- ask
  exists <- liftIO $ userExists thePipe user
  if user /= "" && claim /= "" && claim `member` theAllowedClaims && exists
    then do
      updated <- updateClaim thePipe user claim on
      if updated
        then return status200
        else return status501
    else return status400

deleteClaim :: ReaderT Environment S.ActionM ()
deleteClaim = do
  env <- ask
  user <- lift $ S.param "user" `S.rescue` handleParseError
  claim <- lift $ S.param "claim" `S.rescue` handleParseError
  status <- liftIO $ runReaderT (changeClaim user claim False) env
  lift $ S.status status

addClaim :: ReaderT Environment S.ActionM ()
addClaim = do
  env <- ask
  user <- lift $ S.param "user" `S.rescue` handleParseError
  claim <- lift $ S.param "claim" `S.rescue` handleParseError
  status <- liftIO $ runReaderT (changeClaim user claim True) env
  lift $ S.status status

login :: ReaderT Environment S.ActionM ()
login = do
  Environment thePipe _ <- ask
  user <- lift $ S.jsonData `S.rescue` handleParseError
  lift
    $ unless (ME.isEmptyCreateUser user)
    $ do
      validUser <- liftIO $ getUser thePipe (ME.userName user)
      if ME.validateLogin user validUser
        then do
          S.json $ fromJust validUser
          S.status status200
        else S.status status401
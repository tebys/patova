{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Model where

import           Data.Aeson.TH
import qualified Database.MongoDB as M
import qualified Control.Monad.IO.Class
import           Data.Maybe (isJust)
import qualified Data.Set as S
import           Data.Text (Text)
import           Control.Monad.IO.Class (liftIO)

type Claim = (Text, Bool)

data User = User { userName :: Text, password :: Text, claims :: [Claim] }
  deriving Show

$(deriveJSON defaultOptions ''User)

dbAction :: Control.Monad.IO.Class.MonadIO m => M.Pipe -> M.Action m a -> m a
dbAction pipe = M.access pipe M.master "patova"

-- Shouldn't have missing fields nor bad types (fail)
doc2User :: MonadFail m => M.Document -> m User
doc2User doc = do
  name <- M.lookup "username" doc
  pass <- M.lookup "password" doc
  return $ User name pass $ map map2Claim =<< M.lookup "claims" doc
  where
    map2Claim :: M.Document -> Claim
    map2Claim d2 = let name = M.label $ head d2
                       value = M.typed . M.value $ head d2
                   in (name, value)

user2Doc :: User -> M.Document
user2Doc user = [ "username" M.=: userName user
                , "password" M.=: password user
                , "claims" M.=: map claim2Doc (claims user)]
  where
    claim2Doc (name, value) = [name M.=: value]

getUser :: M.Pipe -> Text -> IO (Maybe User)
getUser pipe name = do
  users <- dbAction pipe
    $ do
      M.rest =<< M.find (M.select ["username" M.=: name] "users")
  if null users
    then return Nothing
    else return $ Just =<< doc2User (head users)

userExists :: M.Pipe -> Text -> IO Bool
userExists pipe name = do
  users <- getUser pipe name
  return $ isJust users

insertUser
  :: Control.Monad.IO.Class.MonadIO m => M.Pipe -> S.Set Text -> User -> m ()
insertUser pipe initialClaims user = do
  dbAction pipe
  $ do
    -- TODO: lenses pls!
    let iclaims = S.toList $ S.map (, False) initialClaims
    M.insert_ "users" $ user2Doc $ User (userName user) (password user) iclaims

removeUser :: Control.Monad.IO.Class.MonadIO m => M.Pipe -> String -> m ()
removeUser = undefined

updateClaim :: Control.Monad.IO.Class.MonadIO m
            => M.Pipe
            -> Text
            -> Text
            -> Bool
            -> m Bool
updateClaim pipe username claimName on = do
  user <- liftIO $ getUser pipe username
  case user of
    Nothing -> return False
    Just us -> do
      -- TODO: lenses pls :)
      let newUser = User (userName us) (password us)
            $ modifyClaim (claimName, on) (claims us)
      dbAction pipe
        $ do
          M.replace (M.select ["username" M.=: username] "users")
            $ user2Doc newUser
      return True

modifyClaim :: Claim -> [Claim] -> [Claim]
modifyClaim claim claimList = claim:[x | x <- claimList, fst x /= fst claim]
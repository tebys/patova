{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module ModelWeb where

import           Data.Aeson.TH
import qualified Model as M
import           Crypto.KDF.BCrypt (hashPassword, validatePassword)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe (isJust, fromJust)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)

data CreateUser = CreateUser { userName :: Text, password :: Text }
  deriving (Show, Eq)

instance Semigroup CreateUser where
  CreateUser a1 a2 <> CreateUser _ _ = CreateUser a1 a2

instance Monoid CreateUser where
  mempty = CreateUser "" ""

isEmptyCreateUser :: CreateUser -> Bool
isEmptyCreateUser (CreateUser u p) = u == "" && p == ""

$(deriveJSON defaultOptions ''CreateUser)

newUser :: CreateUser -> IO M.User
newUser (CreateUser uname upass) = do
  let packed = encodeUtf8 upass
  hashed <- hashPassword 12 packed :: IO B.ByteString
  let unpacked = decodeUtf8 hashed
  return $ M.User uname unpacked []

validateLogin :: CreateUser -> Maybe M.User -> Bool
validateLogin a b = isJust b
  && (let hash = encodeUtf8 $ M.password $ fromJust b
          pass = encodeUtf8 $ password a
      in validatePassword pass hash)
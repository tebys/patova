{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Toml
import           LoadToml
import qualified Web.Scotty as S
import           Network.HTTP.Types
import qualified Database.MongoDB as M
import           Routes
import           Control.Monad.Reader (runReaderT)

main :: IO ()
main = do
  tomlRes <- Toml.decodeFileEither settingsCodec "patova.toml"
  case tomlRes of
    Left errs -> print $ Toml.prettyTomlDecodeErrors errs
    Right s   -> do
      thepipe <- M.connect (M.host "localhost")
      let env = Environment thepipe (settingsClaims s)
      -- TODO: Catch db connection exeption (won't start anyways)
      let Port port = settingsPort s
      putStrLn $ "Server listening on port: " ++ show port
      S.scotty port
        $ do
          S.post "/" $ runReaderT createUser env
          S.delete "/" $ runReaderT deleteUser env
          S.delete "/claim" $ runReaderT deleteClaim env
          S.put "/claim" $ runReaderT addClaim env
          S.post "/login" $ runReaderT login env
          S.notFound $ S.status status404

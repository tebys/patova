{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Toml
import           LoadToml
import qualified Web.Scotty as S
import           Network.HTTP.Types
import qualified Database.MongoDB as M
import           Routes

main :: IO ()
main = do
  tomlRes <- Toml.decodeFileEither settingsCodec "patova.toml"
  case tomlRes of
    Left errs -> print $ Toml.prettyTomlDecodeErrors errs
    Right s   -> do
      pipe <- M.connect (M.host "localhost")
      -- TODO: Catch db connection exeption (won't start anyways)
      let Port port = settingsPort s
      putStrLn $ "Server listening on port: " ++ show port
      S.scotty port
        $ do
          let claims = settingsClaims s
          S.post "/" $ createUser pipe claims
          S.delete "/" $ deleteUser pipe
          S.delete "/claim" $ deleteClaim pipe
          S.put "/claim" $ addClaim pipe claims
          S.post "/login" $ login pipe
          S.notFound $ S.status status404

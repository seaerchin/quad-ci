module Github where

import Core
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Aeson as Aeson (parseMaybe)
import qualified Docker
import Flow
import qualified JobHandler
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import qualified RIO.Text as Text

parsePushEvent :: ByteString -> IO JobHandler.CommitInfo
parsePushEvent bs = do
  -- parse and then queue it into the handler
  let parsed = do
        Aeson.parseMaybe parser =<< Aeson.decodeStrict bs
        where
          parser = Aeson.withObject "Webhook event" $ \obj -> do
            repo <- obj .: "repository"
            head <- obj .: "head_commit"
            slug <- repo .: "full_name"
            sha <- head .: "id"
            branch <- obj .: "ref" <&> Text.dropPrefix "refs/heads/"
            message <- head .: "message"
            author <- head .: "author" >>= \author -> author .: "username"
            pure $
              JobHandler.CommitInfo
                { sha = sha,
                  repo = slug,
                  branch = branch,
                  author = author,
                  message = message
                }
  maybe (throwString $ "Unparsable webhook event" <> show bs) pure parsed

fetchRemotePipeline :: JobHandler.CommitInfo -> IO Pipeline
fetchRemotePipeline ci = do
  base <- HTTP.parseRequest "https://api.github.com"
  let req =
        HTTP.addRequestHeader "Accept" "application/vnd.github.v3.raw" base
          |> HTTP.addRequestHeader "User-Agent" "quad-ci"
          |> HTTP.setRequestPath ("/repos/" <> encodeUtf8 ci.repo <> "/contents/.quad.yml")
          |> HTTP.setRequestQueryString [("ref", Just $ encodeUtf8 ci.sha)]
  res <- HTTP.getResponseBody <$> HTTP.httpBS req
  Yaml.decodeThrow res

createCloneStep :: JobHandler.CommitInfo -> Step
createCloneStep info =
  Step
    { name = StepName "clone",
      commands =
        NonEmpty.Partial.fromList
          ["git clone -q https://github.com/" <> info.repo <> " .", "git checkout -qf " <> info.sha],
      image = Docker.Image "alpine/git" "v2.26.2"
    }
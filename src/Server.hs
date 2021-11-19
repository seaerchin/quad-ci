module Server where

import Codec.Serialise (deserialise, serialise)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Core
import qualified Data.Aeson as Aeson
import qualified Github
import qualified JobHandler
import RIO
import qualified RIO.NonEmpty as NonEmpty
import qualified Web.Scotty as Scotty

newtype Config = Config {port :: Int} deriving (Eq, Show)

jobToJson :: Core.BuildNumber -> JobHandler.Job -> Aeson.Value
jobToJson number job =
  Aeson.object
    [ ("number", Aeson.toJSON $ Core.buildNumberToInt number),
      ("state", Aeson.toJSON $ jobStateToText job.state)
    ]

jobStateToText :: JobHandler.JobState -> Text
jobStateToText = \case
  JobHandler.JobQueued -> "queued"
  JobHandler.JobAssigned -> "assigned"
  JobHandler.JobScheduled b -> case b.state of
    Core.BuildReady -> "ready"
    Core.BuildRunning _ -> "running"
    Core.BuildFinished result -> case result of
      Core.BuildSucceeded -> "succeeded"
      Core.BuildFailed -> "failed"
      Core.BuildUnexpectedState _ -> "unexpectedstate"

run :: Config -> JobHandler.Service -> IO ()
run config handler =
  Scotty.scotty config.port do
    Scotty.post "/agent/pull" do
      cmd <- Scotty.liftAndCatchIO do
        handler.dispatchCmd
      Scotty.raw $ serialise cmd
    Scotty.post "/agent/send" do
      msg <- deserialise <$> Scotty.body
      Scotty.liftAndCatchIO $ handler.processMsg msg
      Scotty.json ("message procesed" :: Text)
    Scotty.post "/webhook/github" do
      body <- Scotty.body
      number <- Scotty.liftAndCatchIO do
        info <- Github.parsePushEvent (toStrictBytes body)
        pipeline <- Github.fetchRemotePipeline info

        -- create a new clone step so that we clone the repo from github before running ci commands
        let step = Github.createCloneStep info

        handler.queueJob $
          Core.Pipeline
            { steps = NonEmpty.cons step pipeline.steps
            }
      Scotty.json $
        Aeson.object
          [ ("number", Aeson.toJSON $ Core.buildNumberToInt number),
            ("status", "job queued")
          ]
    Scotty.get "/build/:number" do
      number <- Core.BuildNumber <$> Scotty.param "number"
      job <- Scotty.liftAndCatchIO do
        maybeJob <- runMaybeT $ handler.findJob number
        case maybeJob of
          -- if nothing found, return 404 back to the user
          Nothing -> undefined
          Just job -> pure job
      pure ()
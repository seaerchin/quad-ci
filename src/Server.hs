module Server where

import Codec.Serialise (deserialise, serialise)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Core
import qualified Data.Aeson as Aeson
import qualified Github
import qualified JobHandler
import qualified Network.HTTP.Types as HTTP.Types
import qualified Network.Wai.Middleware.Cors as Cors
import RIO
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NonEmpty
import qualified System.Log.Logger as Logger
import qualified Web.Scotty as Scotty

newtype Config = Config {port :: Int} deriving (Eq, Show)

jobToJson :: Core.BuildNumber -> JobHandler.Job -> Aeson.Value
jobToJson number job =
  Aeson.object
    [ ("number", Aeson.toJSON $ Core.buildNumberToInt number),
      ("state", Aeson.toJSON $ jobStateToText job.state),
      ("info", Aeson.toJSON $ job.info),
      ("steps", Aeson.toJSON steps)
    ]
  where
    build = case job.state of
      JobHandler.JobScheduled b -> Just b
      _ -> Nothing
    steps =
      ( \step ->
          Aeson.object
            [ ("name", Aeson.String $ Core.stepNameToText step.name),
              ( "build",
                Aeson.String case build of
                  Just b -> stepStateToText b step
                  Nothing -> "ready"
              )
            ]
      )
        <$> job.pipeline.steps

stepStateToText :: Build -> Step -> Text
stepStateToText build step =
  case build.state of
    BuildRunning brs -> if brs.step == step.name then "running" else stepNotRunning
    _ -> stepNotRunning
  where
    stepNotRunning = case Map.lookup step.name build.completedSteps of
      -- skipped because prior step failed
      Nothing -> case build.state of
        BuildFinished _ -> "skipped"
        _ -> "ready"
      Just StepSucceeded -> "succeeded"
      Just (StepFailed _) -> "failed"

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
    Scotty.middleware Cors.simpleCors

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

        buildNum <-
          handler.queueJob info $
            Core.Pipeline
              { steps = NonEmpty.cons step pipeline.steps
              }

        Logger.infoM "quad.server" $ "Queued job " <> Core.displayBuildNumber buildNum
        pure buildNum

      Scotty.json $
        Aeson.object
          [ ("number", Aeson.toJSON $ Core.buildNumberToInt number),
            ("status", "job queued")
          ]

    Scotty.get "/build/:number" do
      number <- Core.BuildNumber <$> Scotty.param "number"
      maybeJob <- Scotty.liftAndCatchIO do
        runMaybeT $ handler.findJob number
      case maybeJob of
        Nothing -> Scotty.raiseStatus HTTP.Types.status404 "Build not found"
        Just job -> Scotty.json $ jobToJson number job

    Scotty.get "/build/:number/step/:step/logs" do
      number <- BuildNumber <$> Scotty.param "number"
      step <- StepName <$> Scotty.param "step"

      log <- Scotty.liftAndCatchIO $ handler.fetchLogs number step
      Scotty.raw $ fromStrictBytes $ fromMaybe "" log

    Scotty.get "/build" do
      jobs <- Scotty.liftAndCatchIO do
        handler.latestJobs
      Scotty.json $ (\(number, job) -> jobToJson number job) <$> jobs
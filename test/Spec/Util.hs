module Spec.Util where

import qualified Agent
import qualified Control.Concurrent.Async as Async
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Core
  ( BuildNumber,
    BuildResult (BuildSucceeded),
    BuildState (BuildFinished),
    Pipeline (..),
    Step (..),
    StepName (StepName),
    Steps,
  )
import qualified Data.List.NonEmpty as NP
import qualified Docker
import qualified JobHandler
import qualified JobHandler.Memory
import RIO
import qualified RIO.Process as Process
import qualified Runner
import qualified Server
import Test.Hspec (shouldBe)

type SideEffect a = a -> IO ()

-- | Remove dangling containers created by testing
cleanupDocker :: IO ()
cleanupDocker = void do
  Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=quad\")"
  Process.readProcessStdout "docker volume rm -f $(docker volume ls -q --filter \"label=quad\")"

checkBuild :: JobHandler.Service -> BuildNumber -> IO ()
checkBuild handler number = do
  result <- runMaybeT loop
  case result of
    Nothing -> fail "buildNumber should exist"
    Just br -> br `shouldBe` BuildSucceeded
  where
    loop = do
      job <- handler.findJob number
      case job.state of
        JobHandler.JobScheduled build -> do
          case build.state of
            BuildFinished s -> pure s
            _ -> loop
        _ -> loop

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
  Step
    { name = StepName name,
      image = Docker.Image {name = image, tag = "latest"},
      commands = NP.fromList commands
    }

makePipeline :: Steps -> Pipeline
makePipeline s = Pipeline {steps = s}

emptyHooks :: Runner.Hooks
emptyHooks =
  Runner.Hooks
    { logCollected = \_ -> pure (),
      buildUpdated = \_ -> pure ()
    }

runServerAndAgent :: SideEffect JobHandler.Service -> SideEffect Runner.Service
runServerAndAgent callback runner = do
  handler <- JobHandler.Memory.createService
  server <- Async.async do
    Server.run (Server.Config 9000) handler
  agent <- Async.async do
    Agent.run (Agent.Config "http://localhost:9000") runner

  -- link threads back to main so errors not silently ignored
  Async.link server
  Async.link agent

  callback handler

  -- thread clean up
  Async.cancel server
  Async.cancel agent
module Runner where

import Core
import qualified Docker
import RIO

data Service = Service {runBuild :: Build -> IO Build, prepareBuild :: Pipeline -> IO Build}

createService :: Docker.Service -> IO Service
createService docker = do
  pure Service {runBuild = runBuild_ docker, prepareBuild = prepareBuild_ docker}

-- | Runs a given build until the end state with the provided service
runBuild_ :: Docker.Service -> Build -> IO Build
runBuild_ docker build = do
  newBuild <- Core.progress docker build
  case newBuild.state of
    BuildFinished br -> pure newBuild
    _ -> do
      -- NOTE: delay to allow the step to complete before running again.
      -- Delay is in micro-seconds hence this delays for 1 second.
      threadDelay (1 * 1000 * 1000)
      runBuild_ docker newBuild

prepareBuild_ :: Docker.Service -> Pipeline -> IO Build
prepareBuild_ docker pipeline = do
  pure
    Build
      { pipeline = pipeline,
        state = BuildReady,
        completedSteps = mempty
      }

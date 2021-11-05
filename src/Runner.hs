module Runner where

import Core
import qualified Docker
import RIO

data Service = Service {runBuild :: Hooks -> Build -> IO Build, prepareBuild :: Pipeline -> IO Build}

-- This is a datatype to represent a callback.
-- This has all the hooks which we want inside it.
data Hooks = Hooks
  { logsCollected :: Log -> IO ()
  }

createService :: Docker.Service -> IO Service
createService docker = do
  pure Service {runBuild = runBuild_ docker, prepareBuild = prepareBuild_ docker}

-- | Runs a given build until the end state with the provided service
runBuild_ :: Docker.Service -> Hooks -> Build -> IO Build
runBuild_ docker hooks build = do
  newBuild <- Core.progress docker build
  case newBuild.state of
    BuildFinished br -> pure newBuild
    _ -> do
      -- NOTE: delay to allow the step to complete before running again.
      -- Delay is in micro-seconds hence this delays for 1 second.
      threadDelay (1 * 1000 * 1000)
      runBuild_ docker hooks newBuild

prepareBuild_ :: Docker.Service -> Pipeline -> IO Build
prepareBuild_ docker pipeline = do
  v <- docker.createVolume
  pure
    Build
      { pipeline = pipeline,
        state = BuildReady,
        completedSteps =
          mempty,
        volume = v
      }

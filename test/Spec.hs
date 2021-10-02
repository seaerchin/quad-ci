module Main where

import Core
import Docker
import RIO
import qualified RIO.Map as Map
import qualified RIO.NonEmpty.Partial as NP
import qualified System.Process.Typed as Process
import Test.Hspec

-- Helpers

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
  Step
    { name = StepName name,
      image = Image image,
      commands = NP.fromList commands
    }

makePipeline :: Steps -> Pipeline
makePipeline s = Pipeline {steps = s}

testPipeline :: Pipeline
testPipeline =
  makePipeline $
    NP.fromList
      [ makeStep "First step" "ubuntu" ["date"],
        makeStep "Second step" "ubuntu" ["uname -r"]
      ]

testBuild :: Build
testBuild =
  Build
    { pipeline = testPipeline,
      state = BuildReady,
      completedSteps = mempty
    }

-- | Runs a given build until the end state with the provided service
runBuild :: Docker.Service -> Build -> IO Build
runBuild docker build = do
  newBuild <- Core.progress docker build
  case newBuild.state of
    BuildFinished br -> pure newBuild
    _ -> do
      -- NOTE: delay to allow the step to complete before running again.
      -- Delay is in micro-seconds hence this delays for 1 second.
      threadDelay (1 * 1000 * 1000)
      runBuild docker newBuild

testRunSuccess :: Docker.Service -> IO ()
testRunSuccess docker = do
  result <- runBuild docker testBuild
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

-- | Remove dangling containers created by testing
cleanupDocker :: IO ()
cleanupDocker = void do
  Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=quad\")"

-- | Main test runner
main :: IO ()
main = hspec do
  -- refer here: https://hackage.haskell.org/package/hspec-2.8.3/docs/Test-Hspec.html#v:runIO
  -- This allows to run an IO action during the construction stage of the test
  docker <- runIO Docker.createService
  beforeAll cleanupDocker $ describe "Quad CI" do
    it "should run a successful build" $ do
      testRunSuccess docker

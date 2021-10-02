module Main where

import Core
import qualified Docker
import RIO
import qualified RIO.Map as Map
import qualified RIO.NonEmpty.Partial as NP
import qualified Runner
import qualified System.Process.Typed as Process
import Test.Hspec

-- Helpers

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
  Step
    { name = StepName name,
      image = Docker.Image image,
      commands = NP.fromList commands
    }

makePipeline :: Steps -> Pipeline
makePipeline s = Pipeline {steps = s}

-- | Happy path test where everything passes
testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
  result <-
    runner.runBuild
      =<< ( runner.prepareBuild $
              makePipeline $
                NP.fromList
                  [ makeStep "First step" "ubuntu" ["date"],
                    makeStep "Second step" "ubuntu" ["uname -r"]
                  ]
          )
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

-- | Expected failure
testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
  result <-
    runner.runBuild
      =<< ( runner.prepareBuild $
              makePipeline $
                NP.fromList
                  [ makeStep "Should fail" "ubuntu" ["exit 1"]
                  ]
          )
  result.state `shouldBe` BuildFinished BuildFailed
  Map.elems result.completedSteps `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]

-- | Remove dangling containers created by testing
cleanupDocker :: IO ()
cleanupDocker = void do
  Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=quad\")"

-- | Main test runner
main :: IO ()
main = hspec do
  -- refer here: https://hackage.haskell.org/package/hspec-2.8.3/docs/Test-Hspec.html#v:runIO
  -- This allows to run an IO action during the construction stage of the test
  runner <- runIO $ Runner.createService =<< Docker.createService
  beforeAll cleanupDocker $ describe "Quad CI" do
    it "should run a successful build" $ do
      testRunSuccess runner
    it "should fail and exit successfully" $ do
      testRunFailure runner

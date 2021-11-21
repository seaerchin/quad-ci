module Main where

import Core
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified Docker
import qualified JobHandler
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified RIO.ByteString as BS
import qualified RIO.HashMap as HashMap
import qualified RIO.Map as Map
import qualified RIO.NonEmpty.Partial as NP
import RIO.Set as Set
import Runner (Hooks (buildUpdated, logCollected))
import qualified Runner
import Spec.Util
import qualified System.Process.Typed as Process
import Test.Hspec

-- | Happy path test where everything passes
testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
  result <-
    runner.runBuild emptyHooks
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
    runner.runBuild emptyHooks
      =<< ( runner.prepareBuild $
              makePipeline $
                NP.fromList
                  [ makeStep "Should fail" "ubuntu" ["exit 1"]
                  ]
          )
  result.state `shouldBe` BuildFinished BuildFailed
  Map.elems result.completedSteps `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]

testSharedWorkspace :: Runner.Service -> IO ()
testSharedWorkspace runner = do
  result <-
    runner.runBuild emptyHooks
      =<< ( runner.prepareBuild $
              makePipeline $
                NP.fromList
                  -- NOTE: Volumes should persist between containers;
                  -- Hence, the test file should still exist after the container is initialized again
                  [ makeStep "Create file" "ubuntu" ["echo hello > test"],
                    makeStep "Read file" "ubuntu" ["cat test"]
                  ]
          )
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testLogCollection :: Runner.Service -> IO ()
testLogCollection runner = do
  expected <- newMVar $ Set.fromList ["hello", "world", "Linux"]
  let onLog :: Log -> IO ()
      onLog log = do
        remaining <- readMVar expected
        forM_ remaining $ \word -> do
          -- break the output using the remaining words
          case BS.breakSubstring word log.output of
            (_, "") -> pure () -- nothing found
            -- if the word exists in the output, then delete it
            -- from the set of expected words
            _ -> modifyMVar_ expected (pure . Set.delete word)
      hooks = Runner.Hooks {logCollected = onLog, buildUpdated = \build -> traceShowIO build}
  build <-
    runner.prepareBuild $
      makePipeline $
        NP.fromList
          [ makeStep "Long Step" "ubuntu" ["echo hello", "sleep 2", "echo world"],
            makeStep "Echo Linux" "ubuntu" ["uname -s"]
          ]
  result <- runner.runBuild hooks build
  -- container should execute successfully
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]
  -- logs should be collected successfully - no expected remaining logs
  readMVar expected >>= \logs -> logs `shouldBe` Set.empty

testImagePull :: Runner.Service -> IO ()
testImagePull runner = do
  Process.readProcessStdout "docker rmi -f busybox"
  build <-
    runner.prepareBuild $
      makePipeline $
        NP.fromList
          [ makeStep "First Step" "busybox" ["date"]
          ]
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded]

testYamlDecoding :: Runner.Service -> IO ()
testYamlDecoding runner = do
  result <-
    runner.runBuild emptyHooks
      =<< runner.prepareBuild
      =<< Yaml.decodeFileThrow "test/pipeline.yml"
  result.state `shouldBe` BuildFinished BuildSucceeded

testServerAndAgent :: Runner.Service -> IO ()
testServerAndAgent =
  runServerAndAgent $ \handler -> do
    let pipeline = makePipeline $ NP.fromList [makeStep "agent-test" "busybox" ["echo hello", "echo from agent"]]
        info =
          JobHandler.CommitInfo
            { sha = "00000",
              branch = "master",
              message = "test commit",
              author = "quad",
              repo = "quad-ci/quad"
            }
    -- get build number
    number <- handler.queueJob info pipeline
    checkBuild handler number

testWebhookTrigger :: Runner.Service -> IO ()
testWebhookTrigger = runServerAndAgent $ \handler -> do
  req <-
    HTTP.setRequestBodyFile "test/github-payload.sample.json"
      . HTTP.setRequestPath "/webhook/github"
      . HTTP.setRequestMethod "POST"
      <$> HTTP.parseRequest "http://localhost:9000"

  res <- HTTP.httpBS req

  let body = Aeson.eitherDecodeStrict $ HTTP.getResponseBody res

  case body of
    -- Just (Aeson.Number number) = HashMap.lookup "number" build
    Left s -> undefined
    Right (Aeson.Object build) ->
      let n = HashMap.lookup "number" build
       in case n of
            Nothing -> undefined
            Just (Aeson.Number number) -> checkBuild handler $ Core.BuildNumber (round number)
            Just _ -> undefined
    Right _ -> undefined
  pure ()

-- | Main test runner
-- NOTE: we are initializing containers again and again
-- This suggests that the containers are not cleaned up between test runs
main :: IO ()
main = hspec do
  -- refer here: https://hackage.haskell.org/package/hspec-2.8.3/docs/Test-Hspec.html#v:runIO
  -- This allows to run an IO action during the construction stage of the test
  runner <- runIO $ Runner.createService =<< Docker.createService
  beforeAll cleanupDocker $ describe "Quad CI" do
    it "should process webhooks" $ do
      testWebhookTrigger runner
    it "should pull images correctly" $ do
      testImagePull runner
    it "should decode pipelines" $ do
      testYamlDecoding runner
    it "should run server and agent" $ do
      testServerAndAgent runner
    it "should run a successful build" $ do
      testRunSuccess runner
    it "should fail and exit successfully" $ do
      testRunFailure runner
    it "should share the workspace between container initializations" $ do
      testSharedWorkspace runner
    it "should collect logs successfully" do
      testLogCollection runner
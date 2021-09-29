module Main where

import Core
import RIO
import qualified RIO.NonEmpty.Partial as NP

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
testPipeline = makePipeline $ NP.fromList [makeStep "First step" "ubuntu" ["date"], makeStep "Second step" "ubuntu" ["uname -r"]]

testBuild :: Build
testBuild =
  Build
    { pipeline = testPipeline,
      state = BuildReady,
      completedSteps = mempty
    }

main :: IO ()
main = pure ()

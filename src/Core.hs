module Core where

import RIO
import qualified RIO.List as L
import qualified RIO.Map as M (filter, insert, member, size)
import RIO.NonEmpty (head)

-- a build is a series of commands, which leads to a result (in text)

-- a pipeline (i.e., a build) is modelled as a series of steps
newtype Pipeline = Pipeline {steps :: Steps} deriving (Eq, Show)

type Steps = NonEmpty Step

-- a step can have a label and the actual command.
-- a step is tagged to the image it is to be ran on.
data Step = Step {name :: StepName, commands :: NonEmpty Text, image :: Image} deriving (Eq, Show)

-- a build is a wrapper around the pipeline
-- a build reports the state the pipelins is currently in
data Build = Build {pipeline :: Pipeline, state :: BuildState, completedSteps :: Map StepName StepResult} deriving (Eq, Show)

-- wrapper type
newtype StepName = StepName Text deriving (Eq, Show, Ord)

-- wrapper type
-- an image points to an actual docker image
newtype Image = Image Text deriving (Eq, Show)

data BuildState = BuildReady | BuildRunning BuildRunningState | BuildFinished BuildResult deriving (Eq, Show)

data BuildResult = BuildSucceeded | BuildFailed deriving (Eq, Show)

data StepResult = StepFailed ContainerExitCode | StepSucceeded deriving (Eq, Show)

newtype BuildRunningState = BuildRunningState {step :: StepName} deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int deriving (Eq, Show)

-- boilerplate
stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

imageToText :: Image -> Text
imageToText (Image image) = image

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode c) = c

exitCodeToStepResult :: ContainerExitCode -> StepResult
exitCodeToStepResult exit =
  if exitCodeToInt exit == 0
    then StepSucceeded
    else StepFailed exit

progress :: Build -> IO Build
progress build = case build.state of
  BuildReady ->
    case buildHasNextStep build of
      Left result ->
        pure build{state = BuildFinished result}
      Right step ->
        pure build{state = (BuildRunning . BuildRunningState . StepName) $ stepNameToText step.name}
  BuildRunning state -> do
    let exit = ContainerExitCode 0
        result = exitCodeToStepResult exit
    pure build {state = BuildReady, completedSteps = M.insert state.step result build.completedSteps}
  BuildFinished br -> pure build

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep b =
  let p = b.pipeline.steps
      isFailureState = hasFailure b.completedSteps
   in if isFailureState
        then Left BuildFailed
        else case nextStep of
          Just step -> Right step
          Nothing -> Left BuildSucceeded
  where
    -- find the first new step (not part of the completed steps)
    nextStep = L.find isNewStep b.pipeline.steps
    isNewStep step = not $ M.member step.name b.completedSteps

-- equivalent to not allSucceeded
hasFailure :: Map b StepResult -> Bool
hasFailure m =
  M.size
    ( M.filter
        ( \case
            StepFailed _ -> True
            StepSucceeded -> False
        )
        m
    )
    == 0
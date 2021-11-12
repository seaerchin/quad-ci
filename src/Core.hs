module Core where

import qualified Codec.Serialise as Serialise
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock.POSIX as Time
import Docker
import RIO
import qualified RIO.List as L
import qualified RIO.Map as M (elems, filter, insert, mapWithKey, member, singleton, size, traverseWithKey)
import qualified RIO.NonEmpty as NonEmpty
import qualified RIO.Text as Text

-- | Models a log. This could be abstracted using writer
type LogCollection = Map StepName CollectionStatus

-- | State machine for collection status of logs
data CollectionStatus
  = CollectionReady
  | -- Stores the last log collected time for a particular container
    CollectingLogs Docker.ContainerId Time.POSIXTime
  | CollectionFinished
  deriving (Eq, Show)

-- | A log is tagged to each step in the pipeline and the output is a bytestring of raw textual data
data Log = Log {output :: ByteString, step :: StepName} deriving (Eq, Show, Generic, Serialise.Serialise)

-- a pipeline (i.e., a build) is modelled as a series of steps
newtype Pipeline = Pipeline {steps :: Steps} deriving (Eq, Show, Generic, Aeson.FromJSON, Serialise.Serialise)

type Steps = NonEmpty Step

-- a step can have a label and the actual command.
-- a step is tagged to the image it is to be ran on.
data Step = Step {name :: StepName, commands :: NonEmpty Text, image :: Image}
  deriving (Eq, Show, Generic, Aeson.FromJSON, Serialise.Serialise)

-- a build is a wrapper around the pipeline
-- a build reports the state the pipelins is currently in
-- a build is a series of commands, which leads to a result (in text)
data Build = Build
  { pipeline :: Pipeline,
    state :: BuildState,
    completedSteps :: Map StepName StepResult,
    volume :: Docker.Volume
  }
  deriving (Eq, Show, Generic, Serialise.Serialise)

newtype BuildNumber = BuildNumber Int deriving (Eq, Show, Generic, Serialise.Serialise, Ord)

buildNumberToInt :: BuildNumber -> Int
buildNumberToInt (BuildNumber i) = i

-- wrapper type
newtype StepName = StepName Text deriving (Eq, Show, Ord, Generic, Aeson.FromJSON, Serialise.Serialise)

data BuildState = BuildReady | BuildRunning BuildRunningState | BuildFinished BuildResult deriving (Eq, Show, Generic, Serialise.Serialise)

data BuildResult = BuildSucceeded | BuildFailed | BuildUnexpectedState Text deriving (Eq, Show, Generic, Serialise.Serialise)

data StepResult = StepFailed ContainerExitCode | StepSucceeded deriving (Eq, Show, Generic, Serialise.Serialise)

data BuildRunningState = BuildRunningState {step :: StepName, container :: ContainerId} deriving (Eq, Show, Generic, Serialise.Serialise)

-- boilerplate
stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

exitCodeToStepResult :: ContainerExitCode -> StepResult
exitCodeToStepResult exit =
  if exitCodeToInt exit == 0
    then StepSucceeded
    else StepFailed exit

-- | State transition of the build between ready/running/completed
-- NOTE: we could alternatively use typeclasses to implement this feature.
-- Typeclasses would be easier actually and more extensible.
progress :: Docker.Service -> Build -> IO Build
progress docker build = case build.state of
  BuildReady ->
    case buildHasNextStep build of
      Left result ->
        pure build{state = BuildFinished result}
      Right step -> do
        let script = Text.unlines $ ["set -ex"] <> NonEmpty.toList step.commands
            options = Docker.CreateContainerOptions step.image script build.volume
        docker.pullImage step.image
        containerId <- docker.createContainer options
        docker.startContainer containerId
        pure build{state = BuildRunning $ BuildRunningState step.name containerId}
  BuildRunning state -> do
    status <- docker.containerStatus state.container
    case status of
      ContainerRunning -> pure build
      ContainerExited exit ->
        let result = exitCodeToStepResult exit
         in pure build {state = BuildReady, completedSteps = M.insert state.step result build.completedSteps}
      ContainerOther txt -> do
        let s = BuildUnexpectedState txt
        pure build {state = BuildFinished s}
  BuildFinished br -> pure build

-- | Checks if the build has a next step and returns it.
-- Returns the result of the build if it is completed (no steps).
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
        > 0

-- | Initializes the starting state of the log status of each step in the pipeline.
-- Each step is CollectionReady
initLogCollection :: Pipeline -> LogCollection
initLogCollection p = foldMap f p.steps
  where
    f step = M.singleton step.name CollectionReady

-- | A log collection tracks the collection status of a particular step.
-- Log output is then obtained from the container and returned as a tuple of the log collection and the actual logs.
-- NOTE: There seems to be some redundancy as the Log datatype tracks the stepname, which is already stored in logCollection
collectLogs :: Docker.Service -> LogCollection -> Build -> IO (LogCollection, [Log])
collectLogs docker lc build = do
  now <- Time.getPOSIXTime
  logs <- runCollection docker now lc
  let newCollection = updateCollection build.state now lc
  return (newCollection, logs)

-- | State transition function for our log state machine
-- NOTE: We actually have 2 states here
-- 1. the state of our build
-- 2. the state of our log collection
updateCollection :: BuildState -> Time.POSIXTime -> LogCollection -> LogCollection
updateCollection state now = M.mapWithKey f
  where
    -- wrapper for fmap
    f step = \case
      -- We only transition into collecting logs once the build's step is the same as the log step
      CollectionReady -> update step 0 CollectionReady
      -- We are already collecting logs for this particular step
      -- If we are already collecting logs, just check if the current step is still running
      -- and proceed to update the timestamp accordingly.
      -- If the step id change, we know the previous step must have completed and we can transition.
      CollectingLogs _ _ -> update step now CollectionFinished
      -- We already finished collecting logs for this step
      CollectionFinished -> CollectionFinished

    -- state transition function
    update step since nextState =
      case state of
        BuildRunning brs ->
          if brs.step == step
            then CollectingLogs brs.container since
            else nextState
        _ -> nextState

-- given a docker instance and the current logs so far
-- collect and append the logs from the container to the current logs
runCollection :: Docker.Service -> Time.POSIXTime -> LogCollection -> IO [Log]
runCollection docker until collection = do
  -- collection is a mapping from the stepname to the status
  -- hence, we just check the state of the logs at the present moment
  -- if we can collect logs, then we do
  logs <- M.traverseWithKey f collection
  return $ concat (M.elems logs)
  where
    f step = \case
      CollectionReady -> pure []
      CollectionFinished -> pure []
      CollectingLogs container since -> do
        let options =
              Docker.FetchLogsOptions
                { container = container,
                  since = since,
                  until = until
                }
        output <- docker.fetchLogs options
        pure [Log {step = step, output = output}]
module JobHandler.Memory where

import qualified Agent
import qualified Control.Concurrent.STM as STM
import Control.Monad.Trans.Maybe
import Core
import qualified JobHandler
import RIO
import RIO.Map as Map

-- | This file holds the internal state (hence, memory) of the job handler.
-- It holds the logs, jobs and the next build to run.
data State = State
  { jobs :: Map BuildNumber JobHandler.Job,
    nextBuild :: Int,
    logs :: Map (BuildNumber, StepName) ByteString
  }
  deriving (Eq, Show)

createService :: IO JobHandler.Service
createService = do
  state <-
    STM.newTVarIO
      State
        { jobs = mempty,
          nextBuild = 1,
          logs = mempty
        }
  pure
    JobHandler.Service
      { -- an explanation is in order:
        -- atomically performs an atomic operation on an item wrapped within an STM monad
        -- we have a state transactional variable, named state
        -- this is passed into the queueJob_ function which alters it and returns
        -- a result + updated state
        -- The result is wrapped within the STM monad
        queueJob = \commitInfo pipeline -> STM.atomically do
          STM.stateTVar state $ queueJob_ commitInfo pipeline,
        findJob = \buildNumber -> do
          maybeJob <- liftIO $ STM.atomically do
            -- within STM monad
            stateIO <- readTVar state
            let j = findJob_ buildNumber stateIO
            pure j
          hoistMaybe maybeJob,
        dispatchCmd = STM.atomically do STM.stateTVar state dispatchCmd_,
        processMsg = \msg -> STM.atomically do
          STM.modifyTVar' state $ processMsg_ msg,
        fetchLogs = \number step -> STM.atomically do
          s <- STM.readTVar state
          pure $ fetchLogs_ number step s,
        latestJobs = STM.atomically do
          s <- STM.readTVar state
          pure $ latestJobs_ s
      }

fetchLogs_ :: BuildNumber -> StepName -> State -> Maybe ByteString
fetchLogs_ bn step state = state.logs !? (bn, step)

latestJobs_ :: State -> [(BuildNumber, JobHandler.Job)]
latestJobs_ state = RIO.reverse $ Map.toList state.jobs

-- takes the current pipeline and the state
-- executes it then returns the associated build number
queueJob_ :: JobHandler.CommitInfo -> Pipeline -> State -> (BuildNumber, State)
queueJob_ commitInfo pipeline state =
  let job = JobHandler.Job {pipeline = pipeline, state = JobHandler.JobQueued, info = commitInfo}
      updatedState =
        state
          { jobs = Map.insert (BuildNumber state.nextBuild) job state.jobs,
            nextBuild = state.nextBuild + 1
          }
   in (BuildNumber state.nextBuild, updatedState)

-- given the build number and the state,
-- return the job
findJob_ :: BuildNumber -> State -> Maybe JobHandler.Job
findJob_ number state = Map.lookup number state.jobs

-- finds the first job that is queued
-- issue a dispatch command together with an updated state
dispatchCmd_ :: State -> (Maybe Agent.Cmd, State)
dispatchCmd_ state =
  let updatedJob =
        Map.foldrWithKey f Nothing state.jobs
          >>= \(k, job) -> pure (k, job{state = JobHandler.JobAssigned})
   in case updatedJob of
        Nothing -> (Nothing, state)
        Just (buildNumber, updatedJob) ->
          let newMap = update (\_ -> pure updatedJob) (buildNumber) state.jobs
              updated = state{jobs = newMap}
           in (Just (Agent.StartBuild buildNumber updatedJob.pipeline), updated)
  where
    getQueued j = j.state == JobHandler.JobQueued
    f k job a = case job.state of
      JobHandler.JobQueued -> Just (k, job)
      _ -> a

-- function to process the messages that the agent sends back to us to update us on builds
processMsg_ :: Agent.Msg -> State -> State
processMsg_ msg state = case msg of
  Agent.LogCollected bn log ->
    let k = (bn, log.step)
        updatedLogs = Map.adjust (\x -> x <> " " <> log.output) k state.logs
     in state{logs = updatedLogs}
  -- when we know that the build is updated, we have to update our internal state accordingly
  Agent.BuildUpdated bn bu ->
    let f job = job{state = JobHandler.JobScheduled bu}
        updatedMap = Map.adjust f bn state.jobs
     in state{jobs = updatedMap}

hoistMaybe :: Applicative m => Maybe b -> MaybeT m b
hoistMaybe mb =
  MaybeT $ pure mb
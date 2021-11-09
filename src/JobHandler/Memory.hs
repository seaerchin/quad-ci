module JobHandler.Memory where

import qualified Agent
import qualified Control.Concurrent.STM as STM
import Control.Monad.Trans.Maybe
import Core
import qualified JobHandler
import RIO
import qualified RIO.List as List
import RIO.Map as Map
import RIO.Map.Partial as Map

data State = State {jobs :: Map BuildNumber JobHandler.Job, nextBuild :: Int} deriving (Eq, Show)

createService :: IO JobHandler.Service
createService = do
  state <-
    STM.newTVarIO
      State
        { jobs = mempty,
          nextBuild = 1
        }
  pure
    JobHandler.Service
      { -- an explanation is in order:
        -- atomically performs an atomic operation on an item wrapped within an STM monad
        -- we have a state transactional variable, named state
        -- this is passed into the queueJob_ function which alters it and returns
        -- a result + updated state
        -- The result is wrapped within the STM monad
        queueJob = \pipeline -> STM.atomically do
          STM.stateTVar state $ queueJob_ pipeline,
        findJob = \buildNumber -> do
          maybeJob <- liftIO $ STM.atomically do
            -- within STM monad
            stateIO <- readTVar state
            let j = findJob_ buildNumber stateIO
            pure j
          hoistMaybe maybeJob,
        dispatchCmd = STM.atomically do STM.stateTVar state dispatchCmd_,
        processMsg = \_ -> undefined
      }

-- takes the current pipeline and the state
-- executes it then returns the associated build number
queueJob_ :: Pipeline -> State -> (BuildNumber, State)
queueJob_ pipeline state =
  let job = JobHandler.Job {pipeline = pipeline, state = JobHandler.JobQueued}
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

processMsg_ :: Agent.Msg -> State -> State
processMsg_ msg state = case msg of
  Agent.LogCollected bn log -> _
  Agent.BuildUpdated bn bu -> _

hoistMaybe :: Applicative m => Maybe b -> MaybeT m b
hoistMaybe mb =
  MaybeT $ pure mb
module JobHandler.Memory where

import qualified Control.Concurrent.STM as STM
import Control.Monad.Trans.Maybe
import Core
import qualified JobHandler
import RIO
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
        dispatchCmd = pure undefined,
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

hoistMaybe :: Applicative m => Maybe b -> MaybeT m b
hoistMaybe mb =
  MaybeT $ pure mb
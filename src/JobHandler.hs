module JobHandler where

import qualified Agent
import Control.Monad.Trans.Maybe
import Core
import qualified Data.Aeson.Types as Aeson
import RIO

data Job = Job {pipeline :: Pipeline, state :: JobState, info :: CommitInfo} deriving (Eq, Show)

data JobState = JobQueued | JobAssigned | JobScheduled Build deriving (Eq, Show)

data Service = Service
  { queueJob :: CommitInfo -> Pipeline -> IO BuildNumber,
    dispatchCmd :: IO (Maybe Agent.Cmd),
    processMsg :: Agent.Msg -> IO (),
    findJob :: BuildNumber -> MaybeT IO Job,
    fetchLogs :: BuildNumber -> StepName -> IO (Maybe ByteString),
    latestJobs :: IO [(BuildNumber, Job)]
  }

-- Atomic representing a single git commit
data CommitInfo = CommitInfo
  { sha :: Text,
    repo :: Text,
    branch :: Text,
    message :: Text,
    author :: Text
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

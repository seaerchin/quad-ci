module Agent where

import qualified Codec.Serialise as Serialise
import Core
import qualified Docker
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified Runner

data Cmd = StartBuild BuildNumber Pipeline deriving (Eq, Show, Generic, Serialise.Serialise)

data Msg = LogCollected BuildNumber Log | BuildUpdated BuildNumber Build deriving (Eq, Show, Generic, Serialise.Serialise)

data Config = Config {endpoint :: String} deriving (Eq, Show)

run :: Config -> Runner.Service -> IO ()
run config runner = forever do
  -- gets a request from the endpoint
  endpoint <- HTTP.parseRequest config.endpoint
  let req = HTTP.setRequestPath "/agent/pull" $ HTTP.setRequestMethod "POST" endpoint
  -- use lazy bytestring for better memory representation rather than a strict bytestring in httpBS
  res <- HTTP.httpLBS req
  let cmd = Serialise.deserialise (HTTP.getResponseBody res) :: Maybe Cmd
  traverse_ (runCommand runner) cmd
  threadDelay (1 * 1000 * 1000)

runCommand :: Runner.Service -> Cmd -> IO ()
runCommand runner = \case
  StartBuild buildNum pipeline -> do
    let hooks = Runner.Hooks {logCollected = traceShowIO}
    build <- runner.prepareBuild pipeline
    void $ runner.runBuild hooks build
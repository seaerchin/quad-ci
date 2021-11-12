module Agent where

import qualified Codec.Serialise as Serialise
import Core
import qualified Docker
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified Runner
import System.IO (print)
import System.Log.Logger as Logger

data Cmd = StartBuild BuildNumber Pipeline deriving (Eq, Show, Generic, Serialise.Serialise)

data Msg = LogCollected BuildNumber Log | BuildUpdated BuildNumber Build deriving (Eq, Show, Generic, Serialise.Serialise)

data Config = Config {endpoint :: String} deriving (Eq, Show)

run :: Config -> Runner.Service -> IO ()
run config runner =
  forever
    -- whole chunk is 1 action
    do
      -- gets a request from the endpoint
      endpoint <- HTTP.parseRequest config.endpoint
      let req = HTTP.setRequestPath "/agent/pull" $ HTTP.setRequestMethod "POST" endpoint
      -- use lazy bytestring for better memory representation rather than a strict bytestring in httpBS
      -- sub monadic action
      do
        res <- HTTP.httpLBS req
        let cmd = Serialise.deserialise (HTTP.getResponseBody res) :: Maybe Cmd
        traverse_ (runCommand config runner) cmd
        -- the catch here is only evaluating for the do block it's enclosed in
        `catch` \e -> do
          Logger.warningM "quad.agent" "Server offline, waiting..."
          Logger.warningM "quad.agent" $ show (e :: HTTP.HttpException)
      threadDelay (3 * 1000 * 1000)

runCommand :: Config -> Runner.Service -> Cmd -> IO ()
runCommand config runner = \case
  StartBuild buildNum pipeline -> do
    let hooks =
          Runner.Hooks
            { logCollected = \log -> do
                sendMessage config (LogCollected buildNum log),
              buildUpdated = \build -> do
                sendMessage config (BuildUpdated buildNum build)
            }
    build <- runner.prepareBuild pipeline
    void $ runner.runBuild hooks build

sendMessage :: Config -> Msg -> IO ()
sendMessage config msg = do
  base <- HTTP.parseRequest config.endpoint
  let body = Serialise.serialise msg
      req = HTTP.setRequestBodyLBS body $ HTTP.setRequestPath "/agent/send" $ HTTP.setRequestMethod "POST" base
  void $ HTTP.httpBS req
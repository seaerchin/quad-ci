module Docker where

-- Refer here: https://hackage.haskell.org/package/aeson-1.5.6.0/docs/Data-Aeson-Types.html#v:.:
-- In essence, this retrieves the value of a specific key from a given object.
-- This is appropriate only if the key is guaranteed to exist.
-- If the key is not guaranteed, an alternative .:? could be used instead, which encodes it as Maybe a
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified Socket
import System.IO (putStrLn)

dockerApi = "/var/run/docker.sock"

-- Refer to here: https://hackage.haskell.org/package/aeson-1.5.6.0/docs/Data-Aeson.html for simple toJSON instances

-- wrapper type
-- an image points to an actual docker image
newtype Image = Image Text deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int deriving (Eq, Show)

newtype CreateContainerOptions = CreateContainerOptions {image :: Image} deriving (Eq, Show)

newtype ContainerId = ContainerId Text deriving (Eq, Show)

-- | Represents the status of a container.
-- ContainerOther represents an unknown state and should be taken as a failure state.
data ContainerStatus = ContainerRunning | ContainerExited ContainerExitCode | ContainerOther Text

-- TODO: refactor this to be a typeclass
data Service = Service
  { createContainer :: CreateContainerOptions -> IO ContainerId,
    startContainer :: ContainerId -> IO (),
    containerStatus :: ContainerId -> IO ContainerStatus
  }

type RequestBuilder = Text -> HTTP.Request

imageToText :: Image -> Text
imageToText (Image image) = image

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode c) = c

containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId id) = id

-- | NOTE: we are not passing in the name as a parameter here, so that is a future extension which we could implement.
-- It would not be required because the consumer requesting the job would not require knowledge of the container's name.
createContainer_ :: RequestBuilder -> CreateContainerOptions -> IO ContainerId
createContainer_ makeReq options = do
  -- refer to dockerd reference here: https://docs.docker.com/engine/reference/commandline/dockerd/#daemon-socket-option
  manager <- Socket.newManager dockerApi
  let image = imageToText options.image
      parser = Aeson.withObject "create-container" $ \obj -> do
        containerId <- obj .: "Id"
        pure $ ContainerId containerId
      body =
        Aeson.object
          [ ("Image", Aeson.toJSON image),
            ("Tty", Aeson.toJSON True),
            ("Labels", Aeson.object [("quad", "")]),
            ("Cmd", "echo hello"),
            ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
          ]
      -- NOTE: different from book
      -- Get the default request
      -- Set the parameters, method and body
      req =
        HTTP.setRequestBodyJSON body $
          -- HTTP.setRequestQueryString [("name", Just $ encodeUtf8 "Testing")] $
          HTTP.setRequestMethod "POST" $
            makeReq "/containers/create"
  resp <- HTTP.httpBS req
  -- Dump response to check
  traceShowIO resp
  -- convert into valid json using aeson parser
  parseResponse resp parser

parseResponse :: HTTP.Response ByteString -> (Aeson.Value -> Aeson.Types.Parser a) -> IO a
parseResponse res parser = do
  -- this is within the either monad
  let result = do
        value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
        Aeson.Types.parseEither parser value
  case result of
    -- either cannot decode the response or parser failed
    Left s -> throwString s
    Right status -> pure status

-- NOTE: This could be further improved by having the status code as a type
startContainer_ :: RequestBuilder -> ContainerId -> IO ()
startContainer_ mkReq containerId = do
  -- refer here: https://docs.docker.com/engine/api/v1.40/#operation/ContainerStart
  -- issue a POST request to the endpoint of the docker
  manager <- Socket.newManager dockerApi
  let id = containerIdToText containerId
      createEndpoint = encodeUtf8 $ "/v1.40/containers/" <> id <> "/start"
      req = HTTP.setRequestMethod "POST" $ mkReq $ "/containers/" <> id <> "/start"
  resp <- HTTP.httpBS req
  putStrLn $ "container with id: " <> show id <> " has been started"

-- | Requests for the status of a container from the docker daemon
containerStatus_ :: RequestBuilder -> ContainerId -> IO ContainerStatus
containerStatus_ mkReq id = do
  manager <- Socket.newManager dockerApi
  -- issue a GET request to the endpoint and use aeson to extract it
  let strId = containerIdToText id
      inspectEndpoint = encodeUtf8 $ "/containers/" <> strId <> "/json"
      req = HTTP.setRequestMethod "GET" $ mkReq $ "/containers/" <> strId <> "/json"
      parser = Aeson.withObject "container-status" $ \obj -> do
        exitCode <- obj .: "State" >>= \state -> state .: "ExitCode"
        pure $ ContainerId exitCode
  resp <- HTTP.getResponseBody <$> HTTP.httpBS req
  pure ContainerRunning

createService :: IO Service
createService = do
  manager <- Socket.newManager dockerApi
  let mkReq text =
        let path = encodeUtf8 $ "/v1.40" <> text
         in HTTP.setRequestManager manager $ HTTP.setRequestPath path HTTP.defaultRequest
  pure Service {createContainer = createContainer_ mkReq, startContainer = startContainer_ mkReq, containerStatus = containerStatus_ mkReq}

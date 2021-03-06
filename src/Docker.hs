module Docker where

-- Refer here: https://hackage.haskell.org/package/aeson-1.5.6.0/docs/Data-Aeson-Types.html#v:.:
-- In essence, this retrieves the value of a specific key from a given object.
-- This is appropriate only if the key is guaranteed to exist.
-- If the key is not guaranteed, an alternative .:? could be used instead, which encodes it as Maybe a

import qualified Codec.Serialise as Serialise
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Time.Clock.POSIX as Time
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Simple as HTTP
import RIO
import RIO.List.Partial (init)
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as Text.Partial
import qualified Socket
import System.IO (putStrLn)

--  This module abstracts over communication with the docker daemon.

dockerApi = "/var/run/docker.sock"

-- Refer to here: https://hackage.haskell.org/package/aeson-1.5.6.0/docs/Data-Aeson.html for simple toJSON instances

-- NOTE: Newtypes are used instead of type because we do not want type synonyms.
-- For example: we define a signature Image -> IO (); this allows also Volume -> IO () if
-- they were both defined using type.

newtype Volume = Volume Text deriving (Eq, Show, Generic, Serialise.Serialise)

-- wrapper type
-- an image points to an actual docker image
data Image = Image {name :: Text, tag :: Text} deriving (Eq, Show, Generic, Serialise.Serialise)

instance Aeson.FromJSON Image where
  parseJSON = Aeson.withText "parse-image" $ \image -> do
    case Text.Partial.splitOn ":" image of
      [name] -> pure $ Image {name = name, tag = "latest"}
      [name, tag] -> pure $ Image {name = name, tag = tag}
      _ -> fail $ "Image has incorrect number of colons" <> Text.unpack image

newtype ContainerExitCode = ContainerExitCode Int deriving (Eq, Show, Generic, Serialise.Serialise)

data CreateContainerOptions = CreateContainerOptions {image :: Image, script :: Text, volume :: Volume} deriving (Eq, Show)

newtype ContainerId = ContainerId Text deriving (Eq, Show, Generic, Serialise.Serialise)

-- | Represents the status of a container.
-- ContainerOther represents an unknown state and should be taken as a failure state.
data ContainerStatus = ContainerRunning | ContainerExited ContainerExitCode | ContainerOther Text deriving (Show)

-- TODO: refactor this to be a typeclass
data Service = Service
  { createContainer :: CreateContainerOptions -> IO ContainerId,
    startContainer :: ContainerId -> IO (),
    containerStatus :: ContainerId -> IO ContainerStatus,
    createVolume :: IO Volume,
    fetchLogs :: FetchLogsOptions -> IO ByteString,
    pullImage :: Image -> IO ByteString
  }

-- | Data encoding the possible options for a log config
data FetchLogsOptions = FetchLogsOptions
  { container :: ContainerId,
    since :: Time.POSIXTime,
    until :: Time.POSIXTime
  }

type RequestBuilder = Text -> HTTP.Request

-- Small utilities/helpers
imageToText :: Image -> Text
imageToText image = image.name <> ":" <> image.tag

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode c) = c

containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId id) = id

volumeToText :: Volume -> Text
volumeToText (Volume t) = t

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

-- Helper for easier piping ._.
parseResponse' :: (Aeson.Types.Value -> Aeson.Types.Parser a) -> HTTP.Response ByteString -> IO a
parseResponse' = flip parseResponse

-- | NOTE: we are not passing in the name as a parameter here, so that is a future extension which we could implement.
-- It would not be required because the consumer requesting the job would not require knowledge of the container's name.
createContainer_ :: RequestBuilder -> CreateContainerOptions -> IO ContainerId
createContainer_ makeReq options = do
  -- refer to dockerd reference here: https://docs.docker.com/engine/reference/commandline/dockerd/#daemon-socket-option
  let image = imageToText options.image
      parser = Aeson.withObject "create-container" $ \obj -> do
        containerId <- obj .: "Id"
        pure $ ContainerId containerId
      mountPoint = volumeToText options.volume <> ":/app"
      bind = Aeson.object [("Binds", Aeson.toJSON [mountPoint])]
      body =
        Aeson.object
          [ ("Image", Aeson.toJSON image),
            ("Tty", Aeson.toJSON True),
            ("Labels", Aeson.object [("quad", "")]),
            ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"]),
            -- NOTE: we do a trick to run arbitrary scripts in our docker shell
            -- First, we let the command to run be the env var $QUAD_SCRIPT.
            -- Next, we provide the script and then we set the env var (within docker) to be what we provided.
            -- Hence, on entry into the docker container, the container runs what we have supplied
            ("Cmd", "echo \"$QUAD_SCRIPT\" | /bin/sh"),
            ( "Env",
              Aeson.toJSON
                [ "QUAD_SCRIPT=" <> options.script
                ]
            ),
            ("WorkingDir", "/app"),
            ("HostConfig", bind)
          ]
      -- NOTE: different from book
      -- Get the default request
      -- Set the parameters, method and body
      req =
        HTTP.setRequestBodyJSON body $
          -- HTTP.setRequestQueryString [("name", Just $ encodeUtf8 "Testing")] $
          HTTP.setRequestMethod "POST" $
            makeReq "/containers/create"
  -- convert into valid json using aeson parser
  HTTP.httpBS req >>= parseResponse' parser

-- NOTE: This could be further improved by having the status code as a type
startContainer_ :: RequestBuilder -> ContainerId -> IO ()
startContainer_ mkReq containerId = do
  -- refer here: https://docs.docker.com/engine/api/v1.40/#operation/ContainerStart
  -- issue a POST request to the endpoint of the docker
  let id = containerIdToText containerId
      req = HTTP.setRequestMethod "POST" $ mkReq $ "/containers/" <> id <> "/start"
  resp <- HTTP.httpBS req
  putStrLn $ "container with id: " <> show id <> " has been started"

-- | Requests for the status of a container from the docker daemon
containerStatus_ :: RequestBuilder -> ContainerId -> IO ContainerStatus
containerStatus_ mkReq id = do
  -- issue a GET request to the endpoint and use aeson to extract it
  let strId = containerIdToText id
      req = HTTP.setRequestMethod "GET" $ mkReq $ "/containers/" <> strId <> "/json"
      parser = Aeson.withObject "container-status" $ \obj -> do
        state <- obj .: "State"
        status <- state .: "Status"
        case status of
          "running" -> pure ContainerRunning
          "exited" -> do
            exitCode <- state .: "ExitCode"
            pure $ ContainerExited $ ContainerExitCode exitCode
          unknown -> pure $ ContainerOther unknown
  HTTP.httpBS req >>= parseResponse' parser

createVolume_ :: RequestBuilder -> IO Volume
createVolume_ mkReq = do
  let body =
        Aeson.object
          [ ("Labels", Aeson.object [("quad", "")])
          ]
      req = HTTP.setRequestBodyJSON body $ HTTP.setRequestMethod "POST" $ mkReq "/volumes/create"
      parser = Aeson.withObject "create-volume" $ \o -> do
        Volume <$> o .: "Name"
  HTTP.httpBS req >>= parseResponse' parser

createService :: IO Service
createService = do
  manager <- Socket.newManager dockerApi
  let mkReq text =
        let path = encodeUtf8 $ "/v1.40" <> text
         in HTTP.setRequestManager manager $ HTTP.setRequestPath path HTTP.defaultRequest
  pure
    Service
      { createContainer = createContainer_ mkReq,
        startContainer = startContainer_ mkReq,
        containerStatus = containerStatus_ mkReq,
        createVolume = createVolume_ mkReq,
        fetchLogs = fetchLogs_ mkReq,
        pullImage = pullImage_ mkReq
      }

fetchLogs_ :: RequestBuilder -> FetchLogsOptions -> IO ByteString
fetchLogs_ mkReq options = do
  let path = "/containers/" <> containerIdToText options.container <> "/logs"
      req =
        HTTP.setRequestQueryString
          [ ("since", Just $ timestampToText options.since),
            ("stdout", Just "true"),
            ("stderr", Just "true"),
            ("until", Just $ timestampToText options.until)
          ]
          $ HTTP.setRequestMethod "GET" $ mkReq path
  HTTP.getResponseBody <$> HTTP.httpBS req
  where
    timestampToText :: Time.POSIXTime -> ByteString
    timestampToText = fromString . init . show

-- pulls the specified image from docker repo
-- refer here: https://docs.docker.com/engine/api/v1.41/#operation/ImageCreate
pullImage_ :: RequestBuilder -> Image -> IO ByteString
-- NOTE: instead of the weird destructuring below, i can instead use
-- the namedfieldpuns extension to allow haskell to do it automagically.
pullImage_ mkReq (Image {name = name, tag = tag}) = do
  let path = "/images/create"
      req =
        HTTP.setRequestMethod "POST" $
          HTTP.setQueryString
            [ ("fromImage", Just (textToBs name)),
              ("tag", Just (textToBs tag))
            ]
            $ mkReq path
  HTTP.getResponseBody <$> HTTP.httpBS req

bsToText :: ByteString -> Text
bsToText b = case Text.decodeUtf8' b of
  Left ue -> ""
  Right txt -> txt

textToBs :: Text -> ByteString
textToBs = Text.encodeUtf8
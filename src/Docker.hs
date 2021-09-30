module Docker where

import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified Socket

-- wrapper type
-- an image points to an actual docker image
newtype Image = Image Text deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int deriving (Eq, Show)

newtype CreateContainerOptions = CreateContainerOptions {image :: Image}

imageToText :: Image -> Text
imageToText (Image image) = image

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode c) = c

createContainer :: CreateContainerOptions -> IO ()
createContainer options = do
  -- refer to dockerd reference here: https://docs.docker.com/engine/reference/commandline/dockerd/#daemon-socket-option
  manager <- Socket.newManager "/var/run/docker.sock"
  let image = imageToText options.image
      body = Aeson.object [("Image", Aeson.toJSON image)]
      -- NOTE: different from book
      -- Get the default request
      -- Set the parameters, method and body
      req =
        HTTP.setRequestManager manager $
          HTTP.setRequestBodyJSON body $
            -- HTTP.setRequestQueryString [("name", Just $ encodeUtf8 "Testing")] $
            HTTP.setRequestMethod "POST" $
              HTTP.setRequestPath "/v1.40/containers/create" HTTP.defaultRequest
  resp <- HTTP.httpBS req
  -- Dump response to check
  traceShowIO resp
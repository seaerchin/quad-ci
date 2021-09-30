module Docker where

import qualified Network.HTTP.Simple as HTTP
import RIO

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
  let name = imageToText options.image
      body = ()
  -- NOTE: different from book
  req <- HTTP.setRequestBodyJSON body . HTTP.setRequestQueryString [("name", Just (encodeUtf8 name))] . HTTP.setRequestMethod "POST" <$> HTTP.parseRequest "/v1.40/containers/create"
  resp <- HTTP.httpBS req
  -- Dump response to check
  traceShowIO resp
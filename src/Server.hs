module Server where

import Codec.Serialise (deserialise, serialise)
import Core
import qualified JobHandler
import RIO
import qualified Web.Scotty as Scotty

data Config = Config {port :: Int} deriving (Eq, Show)

run :: Config -> JobHandler.Service -> IO ()
run config handler =
  Scotty.scotty config.port do
    Scotty.post "/agent/pull" do
      cmd <- Scotty.liftAndCatchIO do
        handler.dispatchCmd
      Scotty.raw $ serialise cmd
    Scotty.post "/agent/send" do
      msg <- deserialise <$> Scotty.body
      Scotty.liftAndCatchIO $ handler.processMsg msg
      Scotty.json ("message procesed" :: Text)
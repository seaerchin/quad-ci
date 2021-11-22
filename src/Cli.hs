module Cli where

import qualified Agent
import qualified Docker
import Flow
import qualified JobHandler.Memory as M
import RIO
import qualified Runner
import qualified Server
import qualified System.Log.Logger as Logger
import Text.Read (read)
import qualified UI.Butcher.Monadic as Butcher

data Command = StartServer Server.Config | StartAgent Agent.Config

defaultPort :: Int
defaultPort = 9000

defaultEndpoint :: String
defaultEndpoint = "http://localhost:" <> show defaultPort

runCommand :: Command -> IO ()
runCommand = \case
  StartServer config -> do
    Logger.infoM "quad server" "server starting..."
    handler <- M.createService
    Server.run config handler
  StartAgent config -> do
    Logger.infoM "quad agent" "agent starting"
    docker <- Docker.createService
    runner <- Runner.createService docker
    Agent.run config runner

main :: IO ()
main = Butcher.mainFromCmdParserWithHelpDesc $
  \helpDesc -> do
    Butcher.addCmdSynopsis "Quad CI command line utility "
    Butcher.addHelpCommand2 helpDesc

    Butcher.addCmd "start-server" do
      Butcher.addCmdSynopsis "Start server node"
      port <-
        Butcher.addParamString "PORT" $
          Butcher.paramHelpStr "Server port"
            <> Butcher.paramDefault (show defaultPort)
      Butcher.addCmdImpl
        case readMaybe port of
          Nothing -> throwString "Port must be a number"
          Just p -> do
            let config = Server.Config {port = p}
            runCommand $ StartServer config

    Butcher.addCmd "start-agent" do
      Butcher.addCmdSynopsis "Start agent node"
      endpoint <-
        Butcher.addParamString "ENDPOINT" $
          Butcher.paramHelpStr "endpoint"
            <> Butcher.paramSuggestions [defaultEndpoint]
            <> Butcher.paramDefault defaultEndpoint
      Butcher.addCmdImpl do
        -- we don't parse it as a structure and instead accept it as a string
        -- we could be more strict about it but the assumption is that this failure will crash early enough
        -- so that serious errors are avoided
        let config = Agent.Config {endpoint = endpoint}
        runCommand $ StartAgent config

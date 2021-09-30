module Socket where

import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.Internal as Client.Internal
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS
import RIO

newManager :: FilePath -> IO Client.Manager
newManager fp =
  -- create a new manager with the default settings
  Client.newManager $
    -- wrap the raw connection using the manager
    Client.defaultManagerSettings {Client.managerRawConnection = pure makeSocket}
  where
    makeSocket _ _ _ = do
      -- socket protocols and buffer size; this is a TCP socket (S.Stream) that is a unix socket
      -- refer to this: https://stackoverflow.com/questions/21032562/example-to-explain-unix-domain-socket-af-inet-vs-af-unix
      -- essentially, a unix socket is bound to a fd whereas inet is bound to (ip, port)
      s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
      S.connect s (S.SockAddrUnix fp)
      Client.Internal.makeConnection (SBS.recv s 8096) (SBS.sendAll s) (S.close s)
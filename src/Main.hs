module Main (main) where

import           Control.Concurrent (forkIO)
import           Control.Exception  (IOException, catch)
import           Control.Monad      (forM, forever, when)

import           System.Exit        (exitSuccess)

import qualified Network.Socket     as Socket
import qualified Network.STUN       as STUN


main :: IO ()
main = do
  stunMain 3478
  exitSuccess

stunMain :: Int -> IO ()
stunMain port = do
  tcpAddrs <- Socket.getAddrInfo (Just tcpHints) Nothing (Just . show $ port)
  udpAddrs <- Socket.getAddrInfo (Just udpHints) Nothing (Just . show $ port)

  let addrs = tcpAddrs ++ udpAddrs

  -- Leave one listener on foreground and fork rest of the listeners
  -- to their own threads
  _ <- forM (tail addrs) $ \addr -> forkIO $ listenOn addr
  listenOn (head addrs)

  where
    listenOn :: Socket.AddrInfo -> IO ()
    listenOn addr = do
      let sockType = Socket.addrSocketType addr
      sock <- bindSocket addr

      case sockType of
        Socket.Stream -> do
          Socket.listen sock 5
          putStrLn $ "Listening on TCP " ++ show (Socket.addrAddress addr)
        Socket.Datagram ->
          putStrLn $ "Listening on UDP " ++ show (Socket.addrAddress addr)
        _ -> error $ "Invalid socket type of " ++ show sockType

      stunLoop sock sockType


bindSocket :: Socket.AddrInfo -> IO Socket.Socket
bindSocket addr = do
  let family = Socket.addrFamily addr
      sockType = Socket.addrSocketType addr
      protocol = Socket.addrProtocol addr

  sock <- Socket.socket family sockType protocol
  Socket.setSocketOption sock Socket.ReuseAddr 1
  Socket.setSocketOption sock Socket.ReusePort 1

  when (family == Socket.AF_INET6) $
    Socket.setSocketOption sock Socket.IPv6Only 1

  Socket.bind sock (Socket.addrAddress addr)
  return sock


stunLoop :: Socket.Socket -> Socket.SocketType -> IO ()
stunLoop sock Socket.Stream = forever $ do
  (accepted, from) <- Socket.accept sock
  catch (STUN.recvBinding accepted)
    (\e -> do let err = show (e :: IOException)
              Socket.close accepted
              putStrLn $ "ERROR: " ++ err ++ " with " ++ show from)

stunLoop sock Socket.Datagram = forever $ STUN.recvBinding sock
stunLoop _ sockType = error $ "Invalid socket type of " ++ show sockType


defHints, tcpHints, udpHints :: Socket.AddrInfo
defHints = Socket.defaultHints
           { Socket.addrFlags = [ Socket.AI_ADDRCONFIG, Socket.AI_PASSIVE ] }
tcpHints = defHints { Socket.addrSocketType = Socket.Stream }
udpHints = defHints { Socket.addrSocketType = Socket.Datagram }

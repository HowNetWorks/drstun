{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent        (forkIO)
import           Control.Exception         (IOException, catch)
import           Control.Monad             (forM, forever, when)

import           Data.Text                 (Text)

import           System.Environment        (getArgs)
import           System.Exit               (exitSuccess)

import           Network.Socket            (Socket)
import qualified Network.Socket            as Socket hiding (recv, recvFrom,
                                                      send, sendTo)
import qualified Network.Socket.ByteString as Socket

import           Network.STUN

software :: STUNAttribute
software = Software "Dr. Stun"

nonce :: STUNAttribute
nonce = Nonce "This is fine nonce-nse"

realm, username, credential :: Text
realm      = "Ankh-Morpork"
username   = "donotuseme"
credential = "notasecret"


main :: IO ()
main = do
  ips <- getArgs
  stunMain ips 3478
  exitSuccess

defHints, tcpHints, udpHints :: Socket.AddrInfo
defHints = Socket.defaultHints
           { Socket.addrFlags = [ Socket.AI_ADDRCONFIG, Socket.AI_PASSIVE ] }
tcpHints = defHints { Socket.addrSocketType = Socket.Stream }
udpHints = defHints { Socket.addrSocketType = Socket.Datagram }

mapAddrs :: [String] -> Int -> IO [Socket.AddrInfo]
mapAddrs [] port = do
  tcpAddrs <- Socket.getAddrInfo (Just tcpHints) Nothing (Just . show $ port)
  udpAddrs <- Socket.getAddrInfo (Just udpHints) Nothing (Just . show $ port)
  return $! tcpAddrs `mappend` udpAddrs
mapAddrs ips port = do
  let getAddrInfo hints ip = Socket.getAddrInfo (Just hints) (Just ip) (Just . show $ port)
  tcpAddrs <- mconcat <$> mapM (getAddrInfo tcpHints) ips
  udpAddrs <- mconcat <$> mapM (getAddrInfo udpHints) ips
  return $! tcpAddrs ++ udpAddrs

stunMain :: [String] -> Int -> IO ()
stunMain ips port = do
  addrs <- mapAddrs ips port

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
          Socket.listen sock Socket.maxListenQueue
          putStrLn $ "*** Listening on TCP " ++ show (Socket.addrAddress addr)
        Socket.Datagram ->
          putStrLn $ "*** Listening on UDP " ++ show (Socket.addrAddress addr)
        _ -> error $ "ERROR: Invalid socket type of " ++ show sockType

      stunLoop sock sockType


bindSocket :: Socket.AddrInfo -> IO Socket
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


stunLoop :: Socket -> Socket.SocketType -> IO ()
stunLoop sock Socket.Stream = forever $ do
  (accepted, from) <- Socket.accept sock
  catch (handleMessage accepted)
    (\e -> do let err = show (e :: IOException)
              Socket.close accepted
              putStrLn $ "ERROR: " ++ err ++ " with " ++ show from)

stunLoop sock Socket.Datagram = forever $ handleMessage sock
stunLoop _ sockType = error $ "ERROR: Invalid socket type of " ++ show sockType


------------------------------------------------------------------------
-- STUN/TURN Request/Response dialogs

handleMessage :: Socket -> IO ()
handleMessage sock = do
  (request, from) <- recvRequest sock
  case request of
    Just req@(STUNMessage BindingRequest _ attrs) -> do
      putStrLn $ "BindingRequest from " ++ show from ++ " " ++ show attrs
      -- sendBindingResponse sock from transId
      handleBinding sock req from
    Just req@(STUNMessage AllocateRequest _ attrs) -> do
      putStrLn $ "AllocateRequest from " ++ show from ++ " " ++ show attrs
      handleAllocate sock req from
    _ -> return ()


-- | Receive STUN/TURN Request
recvRequest :: Socket -> IO (Maybe STUNMessage, Socket.SockAddr)
recvRequest sock = do
  (packet, from) <- Socket.recvFrom sock 65536
  let request = parseSTUNMessage packet
  case request of
    Right req -> return (Just req, from)
    Left _    -> return (Nothing, from)


handleBinding :: Socket -> STUNMessage -> Socket.SockAddr -> IO ()
handleBinding sock (STUNMessage _ transId _) from = do
  sendMessage sock from message
  handleMessage sock
  where
    mappedAddress = addrToXorMappedAddress from transId
    attrs = [ mappedAddress, software, Fingerprint Nothing ]
    message = STUNMessage BindingResponse transId attrs

handleAllocate :: Socket -> STUNMessage -> Socket.SockAddr -> IO ()
handleAllocate sock (STUNMessage _ transId attrs) from = do
  serverAddr <- Socket.getSocketName sock
  case getUsername attrs of
    Just _ -> sendAllocateSuccess sock serverAddr from transId
    _      -> sendAllocateError sock from transId
  handleMessage sock

sendMessage :: Socket -> Socket.SockAddr -> STUNMessage -> IO ()
sendMessage sock from message = do
  let packet = produceSTUNMessage message
      debug = parseSTUNMessage packet
  _ <- Socket.sendTo sock packet from
  putStrLn $ "DEBUG: " ++ show from ++ " " ++ show debug
  return ()

-- https://tools.ietf.org/html/rfc5766#section-6.2
sendAllocateSuccess :: Socket -> Socket.SockAddr -> Socket.SockAddr -> TransactionID -> IO ()
sendAllocateSuccess sock serverAddr from transId = sendMessage sock from message
  where
    relayedAddress = addrToXorRelayedAddress serverAddr transId
    mappedAddress = addrToXorMappedAddress from transId
    lifetime = Lifetime 1
    key = longTermKey realm username credential
    messageIntegrity = MessageIntegrity (Key key)
    attrs = [ relayedAddress, mappedAddress, lifetime, software,
              messageIntegrity, Fingerprint Nothing ]
    message = STUNMessage AllocateResponse transId attrs


-- https://tools.ietf.org/html/rfc5766#section-6.2
sendAllocateError :: Socket -> Socket.SockAddr -> TransactionID -> IO ()
sendAllocateError sock from transId = sendMessage sock from message
  where
    errorCode = ErrorCode 401 "Unauthorized"
    attrs = [errorCode, nonce, Realm realm, software, Fingerprint Nothing]
    message = STUNMessage AllocateError transId attrs

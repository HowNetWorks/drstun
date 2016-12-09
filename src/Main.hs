{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent        (forkIO)
import           Control.Exception         (IOException, catch)
import           Control.Monad             (forM, forever, when)

import           System.Exit               (exitSuccess)

import qualified Network.Socket            as Socket hiding (recv, recvFrom,
                                                      send, sendTo)
import qualified Network.Socket.ByteString as Socket

import           Network.STUN

software :: STUNAttribute
software = Software "Dr. Stun"


main :: IO ()
main = do
  stunMain 3478
  exitSuccess

stunMain :: Int -> IO ()
stunMain port = do
  let justPort = Just . show $ port
  tcpAddrs <- Socket.getAddrInfo (Just tcpHints) Nothing justPort
  udpAddrs <- Socket.getAddrInfo (Just udpHints) Nothing justPort

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
  catch (handleMessage accepted)
    (\e -> do let err = show (e :: IOException)
              Socket.close accepted
              putStrLn $ "ERROR: " ++ err ++ " with " ++ show from)

stunLoop sock Socket.Datagram = forever $ handleMessage sock
stunLoop _ sockType = error $ "Invalid socket type of " ++ show sockType


defHints, tcpHints, udpHints :: Socket.AddrInfo
defHints = Socket.defaultHints
           { Socket.addrFlags = [ Socket.AI_ADDRCONFIG, Socket.AI_PASSIVE ] }
tcpHints = defHints { Socket.addrSocketType = Socket.Stream }
udpHints = defHints { Socket.addrSocketType = Socket.Datagram }



------------------------------------------------------------------------
-- STUN/TURN Request/Response dialogs

isRealm :: STUNAttribute -> Bool
isRealm (Realm _) = True
isRealm _ = False

hasRealm :: STUNAttributes -> Bool
hasRealm = any isRealm

handleMessage :: Socket.Socket -> IO ()
handleMessage sock = do
  (request, from) <- recvRequest sock
  case request of
    Just (STUNMessage BindingRequest transId _) ->
      sendBindingResponse sock from transId
    Just req@(STUNMessage AllocateRequest _ _) ->
      handleAllocate sock req from
    _ -> return ()

-- | Receive STUN/TURN Request
recvRequest :: Socket.Socket -> IO (Maybe STUNMessage, Socket.SockAddr)
recvRequest sock = do
  (packet, from) <- Socket.recvFrom sock 65536
  putStrLn $ "Request from " ++ show from
  let request = parseSTUNMessage packet
  print $ "<<< " ++ show request
  case request of
    Right req -> return (Just req, from)
    Left _ -> return (Nothing, from)


handleAllocate :: Socket.Socket -> STUNMessage -> Socket.SockAddr -> IO ()
handleAllocate sock (STUNMessage _ transId attrs) sockAddr = do
  if hasRealm attrs
    then sendAllocateResponse sock sockAddr transId
    else sendAllocateError sock sockAddr transId
  handleMessage sock

-- https://tools.ietf.org/html/rfc5766#section-6.2
sendAllocateResponse :: Socket.Socket -> Socket.SockAddr
                     -> TransactionID -> IO ()
sendAllocateResponse sock from transId = do
  print $ ">>> " ++ show message
  let packet = produceSTUNMessage message
  _ <- Socket.sendTo sock packet from
  return ()
  where
    localhost = Socket.SockAddrInet 65535 (Socket.tupleToHostAddress (127,0,0,1))
    relayedAddress = addrToXorRelayedAddress localhost transId
    mappedAddress = addrToXorMappedAddress localhost transId
    lifetime = Lifetime 3600
    attrs = [relayedAddress, mappedAddress, lifetime, software]
    message = STUNMessage AllocateResponse transId attrs


-- https://tools.ietf.org/html/rfc5766#section-6.2
sendAllocateError :: Socket.Socket -> Socket.SockAddr
                     -> TransactionID -> IO ()
sendAllocateError sock from transId = do
  print $ ">>> " ++ show message
  let packet = produceSTUNMessage message
  _ <- Socket.sendTo sock packet from
  return ()
  where
    errorCode = ErrorCode 401 "Unauthorized"
    realm = Realm "Ankh-Morpork"
    nonce = Nonce "This is fine nonce-nse"
    attrs = [errorCode, realm, nonce, software]
    message = STUNMessage AllocateError transId attrs

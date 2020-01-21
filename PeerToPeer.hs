{-# LANGUAGE OverloadedStrings #-}


module PeerToPeer where

import           BlockChain
import           Control.Concurrent             (forkFinally, forkIO)
import           Control.Concurrent.STM.TVar
import qualified Control.Exception              as E
import           Control.Monad                  (forever, unless, void)
import           Control.Monad.STM
import qualified Data.ByteString.Lazy           as S
import qualified Data.ByteString.Lazy.Char8     as C
import           Network.Socket
import           Network.Socket.ByteString.Lazy (recv, sendAll)


type Port = String

messageHandler tVarBlockChain sock = do
    msg <- recv sock 1024
    blockChain <- atomically (readTVar tVarBlockChain)
    case (deSerializeMessage msg) of
        RequestLatestBlock -> sendLatestBlock blockChain
        RequestLatestBlockChain -> sendLatestBlockChain blockChain
        ReceiveLatestBlock block -> addLatestBlock blockChain block
        ReceiveLatestBlockChain receivedBlockChain -> replaceWithNewBlockChain blockChain receivedBlockChain
    messageHandler tVarBlockChain sock

    where
      sendLatestBlock blockChain = do
          let latestBlock = last blockChain
          sendAll sock (serializeMessage (ReceiveLatestBlock latestBlock))

      sendLatestBlockChain blockChain = do
          sendAll sock (serializeMessage (ReceiveLatestBlockChain blockChain))

      addLatestBlock blockChain block = do
          newBlockChain <- addBlockToBlockChain blockChain block
          case newBlockChain of
              Just newChain -> do
                  atomically (writeTVar tVarBlockChain newChain)
              Nothing       -> return ()

      replaceWithNewBlockChain blockChain receivedBlockChain = do
          let res = replaceBlockChain blockChain receivedBlockChain
          case res of
              Just newChain -> do
                  atomically (writeTVar tVarBlockChain newChain)
              Nothing       -> return ()


peerCommunicator tVarBlockChain sock = do
  mapM_ putStrLn requestList
  request <- getLine
  blockChain <- atomically (readTVar tVarBlockChain)
  case request of
    "1" -> getLatestBlock blockChain

  where
    getLatestBlock blockChain = do
        sendAll sock (serializeMessage RequestLatestBlock)

        msg <- recv sock 1024

        case (deSerializeMessage msg) of
            ReceiveLatestBlock block -> do
                newBlockChain <- addBlockToBlockChain blockChain block
                case newBlockChain of
                    Just newChain -> do
                        atomically (writeTVar tVarBlockChain newChain)
                    Nothing       -> return ()


    requestList = ["1. Request Latest Block", "2. Request Latest BlockChain"]


-- openPort :: Port -> IO ()
openPort port tVarBlockChain = void $ forkIO $ runTCPServer Nothing port (messageHandler tVarBlockChain)

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock $ setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ do
        (conn, _peer) <- accept sock
        void $ forkFinally (server conn) (const $ gracefulClose conn 5000)


--connectToPeer :: Maybe HostName -> Port -> IO ()
connectToPeer host port blockChain = case host of
    Just hostAddr -> runTCPClient hostAddr port $ (peerCommunicator blockChain)
    Nothing       -> runTCPClient "127.0.0.1" port $ (peerCommunicator blockChain)

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock

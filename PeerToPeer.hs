{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module PeerToPeer where

import           BlockChain
import           Control.Concurrent             (forkFinally, forkIO)
import           Control.Concurrent.STM.TVar
import qualified Control.Exception              as E
import           Control.Monad                  (forever, unless, void)
import           Control.Monad.STM
import           Data.Binary                    as B
import qualified Data.ByteString.Lazy           as S
import qualified Data.ByteString.Lazy.Char8     as C
import qualified Data.Set                       as Set
import           GHC.Generics                   (Generic)
import           Network.Socket                 hiding (recv, recvFrom, send,
                                                 sendTo)
import           Network.Socket.ByteString.Lazy (recv, sendAll)
import Data.Maybe (fromMaybe)


type Port = String
type Ip = String


openPort port tVarBlockChain tVarPeers = void $ forkIO $ runTCPServer Nothing port tVarPeers (inputMessageHandler tVarBlockChain tVarPeers)

runTCPServer mhost port tVarPeers server = withSocketsDo $ do
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


requestPeers host port tVarPeers = case host of
    Just hostAddr -> do
        runTCPClient hostAddr port $ (getPeerList tVarPeers)
        atomically (readTVar tVarPeers) >>= (\peers -> atomically (writeTVar tVarPeers (Set.insert (hostAddr,port)  peers)))

    Nothing       -> do
        runTCPClient "127.0.0.1" port $ (getPeerList tVarPeers)
        atomically (readTVar tVarPeers) >>= (\peers -> atomically (writeTVar tVarPeers (Set.insert ("127.0.0.1",port)  peers)))

    where
        getPeerList tVarPeers sock = do
            sendAll sock (serializeMessage RequestPeerList)
            msg <- recv sock 1024
            let ReceivePeerList peers = deSerializeMessage msg
            currentPeers <- atomically (readTVar tVarPeers)
            atomically (writeTVar tVarPeers (Set.union currentPeers (Set.fromList peers)))



runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = E.bracket (getClientSocket host port) close client


getClientSocket host port = withSocketsDo $ do
    addr <- resolve
    sock <- open addr
    return sock
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock

inputMessageHandler tVarBlockChain tVarPeers sock = do

    msg <- recv sock 1024
    putStrLn ("Message:  " ++ (show (deSerializeMessage msg)))

    blockChain <- atomically (readTVar tVarBlockChain)
    case (deSerializeMessage msg) of
        RequestLatestBlock -> sendLatestBlock blockChain
        RequestLatestBlockChain -> sendLatestBlockChain blockChain
        ReceiveLatestBlock block -> addLatestBlock blockChain block
        ReceiveLatestBlockChain receivedBlockChain -> replaceWithNewBlockChain blockChain receivedBlockChain
        RequestPeerList -> sendPeerList tVarPeers
        ReceivePeerList receivedPeers -> addPeers tVarPeers receivedPeers
    inputMessageHandler tVarBlockChain tVarPeers sock

    where
      sendLatestBlock blockChain = do
          let latestBlock = last blockChain
          sendAll sock (serializeMessage (ReceiveLatestBlock latestBlock))

      sendLatestBlockChain blockChain = do
          sendAll sock (serializeMessage (ReceiveLatestBlockChain blockChain))

      addLatestBlock blockChain block = do
          let newBlockChain = addBlockToBlockChain blockChain block
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

      sendPeerList tVarPeers = do
          peers <- atomically (readTVar tVarPeers)
          sendAll sock (serializeMessage (ReceivePeerList (Set.toList peers)))

      addPeers tVarPeers receivedPeers = do
          currentPeers <- atomically (readTVar tVarPeers)
          atomically (writeTVar tVarPeers (Set.union currentPeers (Set.fromList receivedPeers)))

broadCastBlock tVarBlockChain tVarPeers = do
    blockChain <- atomically (readTVar tVarBlockChain)
    peers <- atomically (readTVar tVarPeers)
    mapM_ (\(ip,port) -> fn ip port blockChain) (Set.toList peers)
    where
        fn ip port blockChain = do
            sock <- getClientSocket ip port
            sendAll sock (serializeMessage (ReceiveLatestBlock (last blockChain)))


broadCastBlockChain tVarBlockChain tVarPeers = do
    blockChain <- atomically (readTVar tVarBlockChain)
    peers <- atomically (readTVar tVarPeers)
    mapM_ (\(ip,port) -> fn ip port blockChain) (Set.toList peers)
    where
        fn ip port blockChain = do
            sock <- getClientSocket ip port
            sendAll sock (serializeMessage (ReceiveLatestBlockChain blockChain))


discoverPeers tVarPeers tVarSelfAddr = do
    currentPeers <- atomically (readTVar tVarPeers)
    selfAddr <- atomically (readTVar tVarSelfAddr)
    discoveredPeers <- fn (Set.toList currentPeers) (selfAddr : [])
    let peers = Set.delete selfAddr (Set.fromList discoveredPeers)
    putStrLn ("discovered peers: " ++ (show peers))
    atomically (writeTVar tVarPeers peers)
    return ()


    where
        fn unvistedPeers visitedPeers = case unvistedPeers of
            [] -> return visitedPeers
            (p:ps) -> do
                newPeers <- getConncetedPeers p
                let newUnvistedPeers = Set.toList (Set.difference (Set.fromList newPeers) (Set.fromList visitedPeers))
                fn (ps ++ newUnvistedPeers) (p : visitedPeers)

        getConncetedPeers (ip,port) = do
            sock <- getClientSocket ip port
            sendAll sock (serializeMessage RequestPeerList)
            msg <- recv sock 1024
            let ReceivePeerList peers = deSerializeMessage msg
            return peers





requestAndAddLatestBlock tVarBlockChain peerIp port = do
    sock <- getClientSocket peerIp port
    sendAll sock (serializeMessage RequestLatestBlock)
    msg <- recv sock 1024 
    let ReceiveLatestBlock latestBlock = deSerializeMessage msg
    blockChain <- atomically (readTVar tVarBlockChain)
    let updatedBlockChain = addBlockToBlockChain blockChain latestBlock
    atomically (writeTVar tVarBlockChain (fromMaybe blockChain updatedBlockChain))


requestAndAddLatestBlockFromAllPeer tVarBlockChain tVarPeers = do
    peers <- atomically (readTVar tVarPeers)
    mapM_ (\(ip,port) -> requestAndAddLatestBlock tVarBlockChain ip port) peers



requestAndUpdateLatestBlockChain tVarBlockChain peerIp port = do
    sock <- getClientSocket peerIp port
    sendAll sock (serializeMessage RequestLatestBlockChain)
    msg <- recv sock 1024 
    let ReceiveLatestBlockChain latestBlockChain = deSerializeMessage msg
    currentBlockChain <- atomically (readTVar tVarBlockChain)
    let updatedBlockChain = replaceBlockChain currentBlockChain latestBlockChain
    atomically (writeTVar tVarBlockChain (fromMaybe currentBlockChain updatedBlockChain))


requestAndUpdateLatestBlockChainFromAllPeer tVarBlockChain tVarPeers = do
    peers <- atomically (readTVar tVarPeers)
    mapM_ (\(ip,port) -> requestAndUpdateLatestBlockChain tVarBlockChain ip port) peers
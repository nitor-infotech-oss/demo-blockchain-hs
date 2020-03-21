{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where


import           BlockChain
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import           Control.Monad.Trans         (liftIO)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Maybe                  (fromJust, fromMaybe)
import qualified Data.Set                    as Set
import qualified Data.Text                   as T
import           GHC.Generics                (Generic)
import           Network.Socket              (HostName, ServiceName)
import qualified Network.Wai                 as W
import           PeerToPeer
import           Web.Firefly


main :: IO ()
main = do
  blockChain <- initBlockChain
  tVarBlockChain <- atomically (newTVar blockChain)
  tVarPeers <- atomically (newTVar Set.empty)
  tVarSelfAddr <- atomically (newTVar ("",""))
  run 5000 (app tVarBlockChain tVarPeers tVarSelfAddr)


app :: TVar BlockChain -> TVar (Set.Set (HostName, ServiceName)) -> TVar (HostName, ServiceName) -> App ()
app tVarBlockChain tVarPeers tVarSelfAddr = do
  route "/showBlockChain" (showBlockChainHandler tVarBlockChain)
  route "/openPort" (openPortHandler tVarBlockChain tVarPeers tVarSelfAddr)
  route "/mine" (mineNewBlockHandler tVarBlockChain)
  route "/broadCastBlock" (broadCastBlockHandler tVarBlockChain tVarPeers)
  route "/broadCastBlockChain" (broadCastBlockChainHandler tVarBlockChain tVarPeers)
  route "/peerList" (showPeerListHandler tVarPeers)
  route "/addPeer" (addPeerHandler tVarPeers)
  route "/discoverPeers" (discoverPeersHandler tVarPeers tVarSelfAddr)
  route "/requestBlock" (requestBlockHandler tVarBlockChain tVarPeers)
  route "/requestBlockChain" (requestBlockChainHandler tVarBlockChain tVarPeers)
  route "/requestPeers" (requestPeersHandler tVarPeers)


showBlockChainHandler :: TVar BlockChain -> Handler W.Response
showBlockChainHandler tVarBlockChain = liftIO $ do
    blockChain <- atomically (readTVar tVarBlockChain)
    return (toResponse $ Json blockChain)


openPortHandler :: TVar BlockChain -> TVar (Set.Set (HostName, ServiceName)) -> TVar (HostName, ServiceName) -> Handler W.Response
openPortHandler tVarBlockChain tVarPeers tVarSelfAddr = do
  port <- fromMaybe "3000" <$> getQuery "port"
  liftIO $ do
    openPort (T.unpack port) tVarBlockChain tVarPeers
    atomically (writeTVar tVarSelfAddr (localhost, (T.unpack port)))
    return (toResponse ("TCP server running on port: " `mappend` port :: T.Text, status200))


mineNewBlockHandler :: TVar BlockChain -> Handler W.Response
mineNewBlockHandler tVarBlockChain = do
    mineData <- fromMaybe "" <$> getQuery "data"
    liftIO $ do
      case mineData of
        "" -> return (toResponse ("No data to mine: " :: T.Text, status200))
        _  -> do
          currentBlockChain <- atomically (readTVar tVarBlockChain)
          newBlockChain <- mine currentBlockChain (T.unpack mineData)
          case newBlockChain of
            Just blockChain -> do
              atomically (writeTVar tVarBlockChain blockChain)
              return (toResponse $ Json blockChain)


broadCastBlockChainHandler :: TVar BlockChain -> TVar (Set.Set (HostName, ServiceName)) -> Handler W.Response
broadCastBlockChainHandler tVarBlockChain tVarPeers = liftIO $ do
  broadCastBlockChain tVarBlockChain tVarPeers
  return (toResponse ("Done broadCasting blockChain" :: T.Text, status200))


broadCastBlockHandler :: TVar BlockChain -> TVar (Set.Set (HostName, ServiceName)) -> Handler W.Response
broadCastBlockHandler tVarBlockChain tVarPeers = liftIO $ do
  broadCastBlock tVarBlockChain tVarPeers
  return (toResponse ("Done broadCasting latest block" :: T.Text, status200))


showPeerListHandler :: TVar (Set.Set (HostName, ServiceName)) -> Handler W.Response
showPeerListHandler tVarPeers = liftIO $ do
    peerList <- atomically (readTVar tVarPeers)
    return (toResponse $ Json peerList)


addPeerHandler :: TVar (Set.Set (HostName, ServiceName)) -> Handler W.Response
addPeerHandler tVarPeers = do
    ip <- fromMaybe (T.pack localhost) <$> getQuery "ip"
    port <- getQuery "port"
    case port of
      Just port -> liftIO $ do
        currentPeers <- atomically (readTVar tVarPeers)
        atomically (writeTVar tVarPeers (Set.insert (T.unpack ip,T.unpack port) currentPeers ))
        return (toResponse ("Added peer: " <> ip <> ":" <> port:: T.Text, status200))
      Nothing -> return (toResponse ("Failed to add peer, invalid or blank port" :: T.Text, status200))


discoverPeersHandler :: TVar (Set.Set (HostName, ServiceName)) -> TVar (HostName, ServiceName) -> Handler W.Response
discoverPeersHandler tVarPeers tVarSelfAddr = liftIO $ do
  peers <- discoverPeers tVarPeers tVarSelfAddr
  return (toResponse ("discoverd peers " <> T.pack (show peers) :: T.Text, status200))


requestBlockHandler :: TVar BlockChain -> TVar (Set.Set (HostName, ServiceName)) -> Handler W.Response
requestBlockHandler tVarBlockChain tVarPeers = liftIO $ do
  requestAndAddLatestBlockFromAllPeer tVarBlockChain tVarPeers
  blockhChain <- atomically (readTVar tVarBlockChain)
  return $ toResponse $ Json blockhChain


requestBlockChainHandler :: TVar BlockChain -> TVar (Set.Set (HostName, ServiceName)) -> Handler W.Response
requestBlockChainHandler tVarBlockChain tVarPeers = liftIO $ do
  requestAndUpdateLatestBlockChainFromAllPeer tVarBlockChain tVarPeers
  blockhChain <- atomically (readTVar tVarBlockChain)
  return $ toResponse $ Json blockhChain


requestPeersHandler :: TVar (Set.Set (HostName, ServiceName)) -> Handler W.Response
requestPeersHandler tVarPeers = do
    ip <- getQuery "ip"
    port <- getQuery "port"
    case port of
      Just port -> liftIO $ do
        requestPeers (fmap T.unpack ip) (T.unpack port) tVarPeers
        return $ toResponse $ ("Peer added from IP: " <> fromJust ip <> ":" <> port :: T.Text, status200)
      Nothing -> return $ toResponse $ ("Invalid port" :: T.Text, status200)

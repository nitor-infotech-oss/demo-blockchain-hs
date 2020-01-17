{-# LANGUAGE OverloadedStrings #-}


module PeerToPeer where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString.Lazy as S
import Network.Socket
import Network.Socket.ByteString.Lazy (recv, sendAll)
import qualified Data.ByteString.Lazy.Char8 as C
import BlockChain


type Port = String

peerHandler sock = do
    msg <- recv sock 1024
    putStrLn (show (getDeserializedBlockChain msg))
    unless (S.null msg) $ do
        sendAll sock msg
        peerHandler sock
    
peer = \s -> do
    blockchain <- initBlockChain
    putStrLn (show blockchain)
    let sBlockchain = getSerializedBlockChain blockchain
    sendAll s sBlockchain
    msg <- recv s 1024
    putStr "Received: "
    C.putStrLn msg


openPort :: Port -> IO ()
openPort port = runTCPServer Nothing port peerHandler

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


connectToPeer :: Maybe HostName -> Port -> IO ()
connectToPeer host port = case host of Just hostAddr -> runTCPClient hostAddr port $ peer
                                       Nothing -> runTCPClient "127.0.0.1" port $ peer


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

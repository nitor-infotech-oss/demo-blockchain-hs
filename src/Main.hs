module Main where


import           Control.Concurrent.STM.TVar
import           Control.Monad.STM

import           BlockChain
import           Data.Maybe                  (fromMaybe)
import qualified Data.Set                    as Set
import           PeerToPeer
import           Text.Read                   (readMaybe)


data Request = LatestBlock | LatestBlockChain deriving (Eq, Show)

main :: IO ()
main = do
    putStrLn greeting
    blockChain <- initBlockChain
    tVarBlockChain <- atomically (newTVar blockChain)
    tVarPeers <- atomically (newTVar Set.empty)
    tVarSelfAddr <- atomically (newTVar ("",""))
    runApp tVarBlockChain tVarPeers tVarSelfAddr
      where
        greeting = "\n\nWelcome to blockchain CLI App"


runApp tVarBlockChain tVarPeers tVarSelfAddr = do
    putStrLn "\n\nEnter command to process"
    mapM_ putStrLn commandList
    command <- getLine
    case command of
        "q" -> return ()
        _   -> commandProcessor command tVarBlockChain tVarPeers tVarSelfAddr >> runApp tVarBlockChain tVarPeers tVarSelfAddr


      where
        commandList = [
            "0. Show blockchain",
            "1. Open port for incoming connection",
            "2. Mine block",
            "3. Broadcast block",
            "4. Broadcast blockchain",
            "5. Show peer list",
            "6. Discover peers from connected peers",
            "7. Get peer list from a peer",
            "8. Add peer",
            "9. Request Latest block",
            "10. Request Latest blockchain",
            "11. Show self IP and port",
            "q. Quit"
            ]



commandProcessor cmd tVarBlockChain tVarPeers tVarSelfAddr= do
    case cmd of
        "0"  -> showBlockChain tVarBlockChain
        "1"  -> open tVarBlockChain tVarPeers tVarSelfAddr
        "2"  -> mineNewBlock tVarBlockChain
        "3"  -> broadCastBlock tVarBlockChain tVarPeers
        "4"  -> broadCastBlockChain tVarBlockChain tVarPeers
        "5"  -> showPeerList tVarPeers
        "6"  -> discoverPeers tVarPeers tVarSelfAddr
        "7"  -> requestListOfConnectedPeers tVarPeers
        "8"  -> addPeer tVarPeers
        "9"  -> requestLatest LatestBlock tVarBlockChain tVarPeers
        "10" -> requestLatest LatestBlockChain tVarBlockChain tVarPeers
        "11" -> showSelfAddr tVarSelfAddr
        "q"  -> putStrLn "Quit"
        _    -> putStrLn "Not yet defined"


showSelfAddr tVarSelfAddr = do
    (ip,port) <- atomically (readTVar tVarSelfAddr)
    putStrLn ("IP   : " ++ ip)
    putStrLn ("Port : " ++ port)


requestLatest request tVarBlockChain tVarPeers = case request of
    LatestBlock      -> requestLatestBlock'
    LatestBlockChain -> requestLatestBlockChain'

    where
        requestLatestBlock' = do
            mapM_ putStrLn blockRequestList
            input <- getLine
            case input of
                "1" -> requestLatestBlockFromPeer
                "2" -> requestAndAddLatestBlockFromAllPeer tVarBlockChain tVarPeers
            where
                blockRequestList = [
                    "1. Request latest block from a peer",
                    "2. Request latest block from all connected peer"
                    ]

                requestLatestBlockFromPeer = do
                    (ip,port) <- getPeerIpAndPort
                    case ip of
                        "" -> requestAndAddLatestBlock tVarBlockChain localhost port
                        _  -> requestAndAddLatestBlock tVarBlockChain ip port

        requestLatestBlockChain' = do
            mapM_ putStrLn blockchainRequestList
            input <- getLine
            case input of
                "1" -> requestLatestBlockChainFromPeer
                "2" -> requestAndUpdateLatestBlockChainFromAllPeer tVarBlockChain tVarPeers
            where
                blockchainRequestList = [
                    "1. Request latest blockchain from a peer",
                    "2. Request latest blockchain from all connected peer"
                    ]

                requestLatestBlockChainFromPeer = do
                    (ip,port) <- getPeerIpAndPort
                    case ip of
                        "" -> requestAndUpdateLatestBlockChain tVarBlockChain localhost port
                        _  -> requestAndUpdateLatestBlockChain tVarBlockChain ip port

        getPeerIpAndPort = do
            putStrLn "Enter Peer's IP address (default: 127.0.0.1): "
            ip <- getLine
            putStrLn "Enter Peer's Port "
            port <- getLine
            return (ip, port)

open tVarBlockChain tVarPeers tVarSelfAddr = do
    putStrLn "Enter port for incoming connection (default 3000)"
    port <- getLine
    case port of
        "" -> open' "3000"
        _  -> open' port
    where
      open' port = do
          putStrLn ("Running TCP server on port " ++ port)
          openPort port tVarBlockChain tVarPeers
          atomically (writeTVar tVarSelfAddr (localhost, port))


addPeer tVarPeers = do
    putStrLn "Enter peer's IP (default '127.0.0.1')"
    peerIp <- getLine
    putStrLn "Enter peer's port"
    port <- getLine
    case peerIp of
        "" -> addPeerAddr localhost port
        _  -> addPeerAddr peerIp port
    where
        addPeerAddr ip port = do
            currentPeers <- atomically (readTVar tVarPeers)
            atomically (writeTVar tVarPeers (Set.insert (ip,port) currentPeers ))


requestListOfConnectedPeers tVarPeers = do
    putStrLn "Enter peer's IP address to connect (default '127.0.0.1')"
    addr <- getLine
    putStrLn "Enter peer's port to connect :"
    port <- getLine
    case addr of
        "" -> requestPeers Nothing port tVarPeers
        _  -> requestPeers (Just addr) port tVarPeers


mineNewBlock tVarBlockChain = do
    putStrLn "Enter data to mine"
    inputData <- getLine
    blockChain <- atomically (readTVar tVarBlockChain)
    newBlockChain <- mine blockChain inputData
    case newBlockChain of
        Just chain -> do
            atomically (writeTVar tVarBlockChain chain)
        Nothing -> return ()


showPeerList tVarPeers = do
    peerList <- atomically (readTVar tVarPeers)
    putStrLn (show peerList)



showBlockChain tVarBlockChain = do
    blockChain <- atomically (readTVar tVarBlockChain)
    putStrLn (show blockChain)

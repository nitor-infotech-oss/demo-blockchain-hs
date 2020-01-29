module Main where


import           Control.Concurrent.STM.TVar
import           Control.Monad.STM

import           BlockChain
import           PeerToPeer

import qualified Data.Set as Set


main :: IO ()
main = do
    putStrLn greeting
    blockChain <- initBlockChain
    tVarBlockChain <- atomically (newTVar blockChain)
    tVarPeers <- atomically (newTVar Set.empty)
    runApp tVarBlockChain tVarPeers
      where
        greeting = "\n\nWelcome to blockchain CLI App"


runApp tVarBlockChain tVarPeers = do
    putStrLn "\n\nEnter command to process"
    mapM_ putStrLn commandList
    command <- getLine
    commandProcessor command tVarBlockChain tVarPeers
    runApp tVarBlockChain tVarPeers

      where
        commandList = [
            "0. Show BlockChain",
            "1. Open port for incoming connection",
            "2. Add peer",
            "3. Get peer list from connected peer",
            "4. Show peer list",
            "5. Mine block",
            "6. Broadcast blockChain",
            "7. Broadcast block",
            "7. Quit"
            ]



commandProcessor cmd tVarBlockChain tVarPeers= do
    case cmd of
        "0" -> showBlockChain tVarBlockChain
        "1" -> open tVarBlockChain tVarPeers
        "2" -> addPeer tVarPeers
        "3" -> requestListOfConnectedPeers tVarPeers
        "4" -> showPeerList tVarPeers
        "5" -> mineNewBlock tVarBlockChain
        "6" -> broadCastBlockChain tVarBlockChain tVarPeers
        _   -> putStrLn "Not yet defined"


open tVarBlockChain tVarPeers = do
    putStrLn "Enter port for incoming connection (default 3000)"
    port <- getLine
    case port of
        "" -> open' "3000"
        _  -> open' port
    where
      open' port = putStrLn ("Running TCP server on port " ++ port) >> openPort port tVarBlockChain tVarPeers


addPeer tVarPeers = do
    putStrLn "Enter peer's IP (default '127.0.0.1')"
    peerIp <- getLine
    putStrLn "Enter peer's port"
    port <- getLine
    case peerIp of
        "" -> addPeerAddr "127.0.0.1" port
        _  -> addPeerAddr peerIp port
    where
        addPeerAddr ip port = do
            currentPeers <- atomically (readTVar tVarPeers)
            atomically (writeTVar tVarPeers (Set.insert (ip,port) currentPeers ))
            --atomically (writeTVar tVarPeers (currentPeers ++ [(ip,port)]))


requestListOfConnectedPeers tVarPeers = do
    putStrLn "Enter peer's IP address to connect (default '127.0.0.1')"
    addr <- getLine
    putStrLn "Enter peer's port to connect :"
    port <- getLine
    case addr of
        "" -> requestPeers Nothing port tVarPeers
        _  -> requestPeers (Just addr) port tVarPeers


mineNewBlock tVarBlockChain = do
    putStrLn "Enter data to mine (default 'Hello World')"
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

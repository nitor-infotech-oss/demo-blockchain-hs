module Main where


import           Control.Concurrent.STM.TVar
import           Control.Monad.STM

import           BlockChain
import           PeerToPeer

main :: IO ()
main = do
    putStrLn greeting
    blockChain <- initBlockChain
    tVarBlockChain <- atomically (newTVar blockChain)
    tVarPeerList <- atomically (newTVar [])
    runApp tVarBlockChain tVarPeerList
      where
        greeting = "\n\nWelcome to blockchain CLI App"


runApp tVarBlockChain tVarPeerList = do
    mapM_ putStrLn commandList
    command <- getLine
    commandProcessor command tVarBlockChain tVarPeerList
    runApp tVarBlockChain tVarPeerList

      where
        commandList = ["\n\nEnter command to process", "1. Open port for incoming connection", "2. Connect to peers", "3. Show connected peer list", "4. Mine block", "5. Broadcast chain", "6. Show BlockChain", "7. Quit"]


commandProcessor cmd tVarBlockChain tVarPeerList= do
    case cmd of
        "1" -> open tVarBlockChain tVarPeerList
        "2" -> connect tVarBlockChain
        "3" -> showPeerList tVarPeerList
        "4" -> mineNewBlock tVarBlockChain
        "6" -> showBlockChain tVarBlockChain
        _   -> putStrLn "Not yet defined"


open tVarBlockChain tVarPeerList = do
    putStrLn "Enter port for incoming connection (default 3000)"
    port <- getLine
    case port of
        "" -> open' "3000"
        _  -> open' port
    where
      open' port = putStrLn ("Running TCP server on port " ++ port) >> openPort port tVarBlockChain tVarPeerList


connect tVarBlockChain = do
    putStrLn "Enter peer's IP address to connect (default '127.0.0.1')"
    addr <- getLine
    putStrLn "Enter peer's port to connect :"
    port <- getLine
    case addr of
        "" -> connectToPeer Nothing port tVarBlockChain
        _  -> connectToPeer (Just addr) port tVarBlockChain


mineNewBlock tVarBlockChain = do
    putStrLn "Enter data to mine (default 'Hello World')"
    inputData <- getLine
    blockChain <- atomically (readTVar tVarBlockChain)
    newBlockChain <- mine blockChain inputData
    case newBlockChain of
        Just chain -> do
            atomically (writeTVar tVarBlockChain chain)
        Nothing -> return ()
    

showPeerList tVarPeerList = do
    peerList <- atomically (readTVar tVarPeerList)
    putStrLn (show peerList)



showBlockChain tVarBlockChain = do
    blockChain <- atomically (readTVar tVarBlockChain)
    putStrLn (show blockChain)

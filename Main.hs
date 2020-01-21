module Main where


import           Control.Concurrent.STM.TVar
import           Control.Monad.STM

import           BlockChain
import           PeerToPeer

main :: IO ()
main = do
    blockChain <- initBlockChain
    tVarBlockChain <- atomically (newTVar blockChain)
    putStrLn greeting
    runApp tVarBlockChain
      where
        greeting = "\n\nWelcome to blockchain CLI App"

runApp tVarBlockChain = do
    mapM_ putStrLn commandList
    command <- getLine
    commandProcessor command tVarBlockChain
    runApp tVarBlockChain

      where
        commandList = ["\n\nEnter command to process", "1. Open port for incoming connection", "2. Connect to peers", "3. Mine block", "4. Broadcast chain", "5. Show BlockChain", "6. Quit"]


commandProcessor cmd tVarBlockChain = do
    case cmd of
        "1" -> open tVarBlockChain
        "2" -> connect tVarBlockChain
        "3" -> mineNewBlock tVarBlockChain
        "5" -> showBlockChain tVarBlockChain
        _   -> putStrLn "Not yet defined"


open tVarBlockChain = do
    putStrLn "Enter port for incoming connection (default 3000)"
    port <- getLine
    case port of
        "" -> open' "3000"
        _  -> open' port
    where
      open' port = putStrLn ("Running TCP server on port " ++ port) >> openPort port tVarBlockChain


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
    

showBlockChain tVarBlockChain = do
    blockChain <- atomically (readTVar tVarBlockChain)
    putStrLn (show blockChain)

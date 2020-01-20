module Main where

import           BlockChain
import           PeerToPeer

main :: IO ()
main = do
    putStrLn greeting
    runApp
      where
        greeting = "\n\nWelcome to blockchain CLI App"

runApp = do
    mapM_ putStrLn commandList
    command <- getLine
    commandProcessor command
    runApp

      where
        commandList = ["\n\nEnter command to process", "1. Open port for incoming connection", "2. Connect to peers", "3. Mine block", "4. Broadcast chain", "5. Quit"]


commandProcessor cmd = do
    case cmd of
        "1" -> open
        "2" -> connect
        _   -> putStrLn "Not yet defined"


open = do
    putStrLn "Enter port for incoming connection (default 3000)"
    port <- getLine
    case port of
        "" -> open' "3000"
        _  -> open' port
    where
      open' port = putStrLn ("Running TCP server on port " ++ port) >> openPort port


connect = do
    putStrLn "Enter peer's IP address to connect (default '127.0.0.1')"
    addr <- getLine
    putStrLn "Enter peer's port to connect (default '127.0.0.1')"
    port <- getLine
    case addr of
        "" -> connectToPeer Nothing port
        _  -> connectToPeer (Just addr) port



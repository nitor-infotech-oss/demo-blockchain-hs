module Main where

import BlockChain
import PeerToPeer

main :: IO ()
main = do
    putStrLn greeting
    mapM_ putStrLn commandList
      where
        greeting = "\n\nWelcome to blockchain CLI App\n\n"
        commandList = ["Enter command to process", "1. Connect to peers", "2. Open port for incoming connection", "3. Mine block", "4. Broadcast chain", "5 Quit"]

module BlockChain where
import Data.Time.Clock (getCurrentTime)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.ByteString.Char8 as BS


type Index = Int
type PrevHash = String
type TimeStamp = IO String
type BlockData = String
type Hash = String
type Nonce = Int

data Block = Block { index :: Index
                    ,prevHash :: PrevHash
                    ,timestamp :: TimeStamp
                    ,blockData :: BlockData
                    ,hash :: Hash
                    ,nonce :: Nonce
                   }

demoBlock = Block 1 "prevHash" getCurrentTimeStamp "blockData" "000" 1

calculateHashForBlock :: Block -> IO ByteString
calculateHashForBlock block = do
	time <- timestamp block
	let hashData = ((show (BlockChain.index block)) ++ (prevHash block) ++ time ++ (blockData block) ++ (show (nonce block)))
	return (createHash hashData)

createHash :: String -> ByteString
createHash sdata = SHA256.hash ( BS.pack sdata)

getCurrentTimeStamp :: IO String
getCurrentTimeStamp = show <$> getCurrentTime

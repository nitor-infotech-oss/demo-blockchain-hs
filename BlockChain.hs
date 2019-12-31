module BlockChain where
import Data.Time.Clock (getCurrentTime)
import Data.ByteString.Lazy
import Data.ByteString.Lazy.UTF8 as BLU
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Text as DT (pack)
import Data.Text.Encoding (encodeUtf8)


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


-- createDemoBlock = Block 0 "0" 
--
--calculateHashForBlock :: Index -> PrevHash -> TimeStamp -> BlockData -> Nonce -> IO String
calculateHashForBlock index prevHash timeStamp blockData nonce = 

	do
	time <- timeStamp
	let hashData = ((show index) ++ prevHash ++ time ++ blockData ++ (show nonce))
	let dataHash = BLU.fromString hashData
	return (createHash dataHash)
	-- let hash = 
	
	

createHash sdata = SHA256.hash $ encodeUtf8 (DT.pack sdata)


getCurrentTimeStamp :: IO String
getCurrentTimeStamp = show <$> getCurrentTime

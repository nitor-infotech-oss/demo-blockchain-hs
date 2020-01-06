module BlockChain where
import Data.Time.Clock
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Char8 as BS


type Index = Int
type PrevHash = BS.ByteString
type Timestamp = UTCTime
type BlockData = String
type Hash = BS.ByteString
type Nonce = Int


data Block = Block { index :: Index
                    ,prevHash :: PrevHash
                    ,timestamp :: Timestamp
                    ,blockData :: BlockData
                    ,hash :: Hash
                    ,nonce :: Nonce
                   } deriving (Show, Eq)

data BlockChain = BlockChain [Block] deriving (Show, Eq)
data BlockValidationStatus = IndexError String | PrevHashError String | HashError String | Valid deriving (Show, Eq)


createHash :: String -> BS.ByteString
createHash sdata = SHA256.hash ( BS.pack sdata)


calculateHashForBlock :: Block -> BS.ByteString
calculateHashForBlock block = calculateHashForBlock' (index block) (prevHash block) (timestamp block) (blockData block) (nonce block)


calculateHashForBlock' :: Index -> PrevHash -> Timestamp -> BlockData -> Nonce -> BS.ByteString
calculateHashForBlock' index prevHash timestamp blockData nonce =
    createHash hashData
      where
        hashData =  (show index) ++ (show prevHash) ++ (show timestamp) ++ blockData ++ (show nonce )


generateNextBlock :: Block -> BlockData -> IO Block
generateNextBlock currentBlock bdata = do
    timestamp <- getCurrentTime
    let nextIndex = (index currentBlock) + 1
    let prevHash = hash currentBlock
    let nonce = 0
    let nextHash = calculateHashForBlock' nextIndex prevHash timestamp bdata nonce
    return (Block nextIndex prevHash timestamp bdata nextHash nonce)


isValidNewBlock :: Block -> Block -> BlockValidationStatus
isValidNewBlock currentBlock newBlock = validateNextBlock currentBlock newBlock (calculateHashForBlock newBlock)
      where
        validateNextBlock cBlock nBlock newBlockHash
          | (hash cBlock) /= (prevHash nBlock) = PrevHashError ("cBlock : " ++ (show (hash cBlock)) ++ ", \nnBlock : " ++ (show (prevHash nBlock)))
          | (index cBlock) + 1 /= (index nBlock) = IndexError ""
          | newBlockHash /= (hash nBlock) = HashError ""
          | otherwise = Valid

isValidHashDifficulty :: Hash -> Int -> Bool
isValidHashDifficulty hash df = (length $ takeWhile (=='0') (BS.unpack hash)) > df

genesisBlock :: IO Block
genesisBlock = do
    now <- getCurrentTime
    let prevHash = BS.pack "0"
    let blockData = "Welcome to demo blockchain"
    let hash = calculateHashForBlock' 1 prevHash now blockData 0
    return (Block 1 prevHash now blockData hash 0)

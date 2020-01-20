{-# LANGUAGE DeriveGeneric #-}

module BlockChain where

import           Crypto.Hash
import           Data.Binary
import           Data.ByteArray.Encoding
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.UTF8    as UTF8
import           Data.Time.Clock
import           GHC.Generics            (Generic)


type Index = Int
type Difficulty = Int
type PrevHash = String
type Timestamp = String
type BlockData = String
type Hash = String
type Nonce = Int
type BlockChain = [Block]


data Block = Block
    { index      :: Index
    , prevHash   :: PrevHash
    , timestamp  :: Timestamp
    , blockData  :: BlockData
    , blockHash  :: Hash
    , nonce      :: Nonce
    , difficulty :: Difficulty
    }
    deriving (Show, Eq, Generic)

instance Binary Block


data Messages = RequestLatestBlock | ReceiveLatestBlock Block | RequestLatestBlockChain | ReceiveLatestBlockChain BlockChain deriving (Show, Eq, Generic)
instance Binary Messages

serializeMessage msg = Data.Binary.encode msg
deSerializeMessage msg = Data.Binary.decode msg :: Messages


sha256 :: String -> String
sha256 input = result
  where
    bytes  = UTF8.fromString input :: ByteString
    digest = hashWith SHA256 bytes :: Digest SHA256
    hex    = convertToBase Base16 digest :: ByteString
    result = UTF8.toString hex :: String


calculateHashForBlock :: Block -> (String, Nonce)
calculateHashForBlock block = calculateHashForBlock' (index block)
                                                     (prevHash block)
                                                     (timestamp block)
                                                     (blockData block)
                                                     (nonce block)
                                                     (difficulty block)


calculateHashForBlock' :: Index -> PrevHash -> Timestamp -> BlockData -> Nonce -> Difficulty -> (String, Nonce)
calculateHashForBlock' index prevHash timestamp blockData nonce difficulty =
    let hash = createHash (hashData nonce)
    in  if (isValidHashDifficulty hash difficulty)
            then (hash, nonce)
            else calculateHashForBlock' index prevHash timestamp blockData (nonce + 1) difficulty
  where
    hashData nonce' = (show nonce') ++ (show index) ++ (show prevHash) ++ (show timestamp) ++ blockData
    createHash sdata = sha256 sdata


generateNextBlock :: Block -> BlockData -> IO Block
generateNextBlock currentBlock bdata = do
    timestamp <- getCurrentTime
    let nextIndex          = (index currentBlock) + 1
    let prevHash           = blockHash currentBlock
    let nonce              = 0
    let hashDifficulty     = difficulty currentBlock
    let (nextHash, uNonce) = calculateHashForBlock' nextIndex prevHash (show timestamp) bdata nonce hashDifficulty
    return (Block nextIndex prevHash (show timestamp) bdata nextHash uNonce hashDifficulty)


isValidNextBlock :: Block -> Block -> Bool
isValidNextBlock currentBlock newBlock =
    let (newBlockHash, nonce) = (calculateHashForBlock newBlock)
    in  validateNextBlock currentBlock newBlock newBlockHash
  where
    validateNextBlock cBlock nBlock newBlockHash | (blockHash cBlock) /= (prevHash nBlock) = False
                                                 | (index cBlock) + 1 /= (index nBlock)    = False
                                                 | newBlockHash /= (blockHash nBlock)      = False
                                                 | otherwise                               = True


isValidHashDifficulty :: Hash -> Int -> Bool
isValidHashDifficulty hash df = (length $ takeWhile (== '0') hash) >= df


isValidBlockChain :: BlockChain -> Bool
isValidBlockChain (b : bs) = isValidBlockChain' b bs
  where
    isValidBlockChain' _ [] = True
    isValidBlockChain' b0 (b1 : bs) | (isValidNextBlock b0 b1) = isValidBlockChain' b1 bs
                                    | otherwise                = False

isChainLonger :: BlockChain -> BlockChain -> Bool
isChainLonger currentBlockChain newBlockChain = length currentBlockChain > length newBlockChain


genesisBlock :: IO Block
genesisBlock = do
    let now = "2020-01-20 18:19:57.288924435 UTC" 
    let prevHash       = "0"
    let blockData      = "Welcome to demo blockchain"
    let difficulty     = 2
    let (hash, uNonce) = calculateHashForBlock' 1 prevHash (show now) blockData 0 difficulty
    return (Block 1 prevHash now blockData hash uNonce difficulty)


mine :: BlockChain -> BlockData -> IO (Maybe BlockChain)
mine blockChain blockData = do
    let currentBlock = (last blockChain)
    newBlock <- generateNextBlock currentBlock blockData
    return (validateNewBlock currentBlock newBlock)
  where
    validateNewBlock currentBlock newBlock | (isValidNextBlock currentBlock newBlock) = Just (blockChain ++ [newBlock])
                                           | otherwise = Nothing


getSerializedBlockChain chain = Data.Binary.encode chain
getDeserializedBlockChain chain = Data.Binary.decode chain :: BlockChain



initBlockChain :: IO BlockChain
initBlockChain = do
    genBlock <- genesisBlock
    return [genBlock]


run = do
    b1 <- genesisBlock
    b2 <- generateNextBlock b1 "data_1"
    let chain = [b1, b2]
    newChain <- mine chain "data_3"
    return newChain

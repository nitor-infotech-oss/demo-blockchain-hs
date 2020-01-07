module BlockChain where
import Data.Time.Clock
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Char8 as BS


type Index = Int
type Difficulty = Int
type PrevHash = BS.ByteString
type Timestamp = UTCTime
type BlockData = String
type Hash = BS.ByteString
type Nonce = Int
type BlockChain = [Block]


data Block = Block { index :: Index
                    ,prevHash :: PrevHash
                    ,timestamp :: Timestamp
                    ,blockData :: BlockData
                    ,hash :: Hash
                    ,nonce :: Nonce
                    ,difficulty :: Difficulty
                   } deriving (Show, Eq)



data BlockValidationStatus = IndexError String | PrevHashError String | HashError String | Valid deriving (Show, Eq)


calculateHashForBlock :: Block -> (BS.ByteString, Nonce)
calculateHashForBlock block = calculateHashForBlock' (index block) (prevHash block) (timestamp block) (blockData block) (nonce block) (difficulty block)


calculateHashForBlock' :: Index -> PrevHash -> Timestamp -> BlockData -> Nonce -> Difficulty -> (BS.ByteString, Nonce)
calculateHashForBlock' index prevHash timestamp blockData nonce difficulty = 
    let hash = createHash (hashData nonce) in
        if (isValidHashDifficulty hash difficulty) then (hash, nonce)
                                      else calculateHashForBlock' index prevHash timestamp blockData (nonce + 1) difficulty
    where
      hashData nonce' =  (show nonce') ++ (show index) ++ (show prevHash) ++ (show timestamp) ++ blockData
      createHash sdata = SHA256.hash ( BS.pack sdata)


generateNextBlock :: Block -> BlockData -> IO Block
generateNextBlock currentBlock bdata = do
    timestamp <- getCurrentTime
    let nextIndex = (index currentBlock) + 1
    let prevHash = hash currentBlock
    let nonce = 0
    let hashDifficulty = difficulty currentBlock
    let (nextHash,uNonce) = calculateHashForBlock' nextIndex prevHash timestamp bdata nonce hashDifficulty 
    return (Block nextIndex prevHash timestamp bdata nextHash uNonce hashDifficulty)


isValidNewBlock :: Block -> Block -> BlockValidationStatus
isValidNewBlock currentBlock newBlock = let (newBlockHash, nonce) = (calculateHashForBlock newBlock) in
                                            validateNextBlock currentBlock newBlock newBlockHash
      where
        validateNextBlock cBlock nBlock newBlockHash
          | (hash cBlock) /= (prevHash nBlock) = PrevHashError ("cBlock : " ++ (show (hash cBlock)) ++ ", \nnBlock : " ++ (show (prevHash nBlock)))
          | (index cBlock) + 1 /= (index nBlock) = IndexError ""
          | newBlockHash /= (hash nBlock) = HashError ""
          | otherwise = Valid


isValidHashDifficulty :: Hash -> Int -> Bool
isValidHashDifficulty hash df = (length $ takeWhile (=='0') (BS.unpack hash)) >= df


genesisBlock :: IO Block
genesisBlock = do
    now <- getCurrentTime
    let prevHash = BS.pack "0"
    let blockData = "Welcome to demo blockchain"
    let difficulty = 2
    let (hash, uNonce) = calculateHashForBlock' 1 prevHash now blockData 0 difficulty
    return (Block 1 prevHash now blockData hash uNonce difficulty)


mine :: BlockChain -> BlockData -> IO (Maybe BlockChain)
mine  blockChain blockData = do
    let currentBlock = (last blockChain)
    newBlock <- generateNextBlock currentBlock blockData
    return (validateNewBlock currentBlock newBlock)
      where
        validateNewBlock currentBlock newBlock | (isValidNewBlock currentBlock newBlock) == Valid = Just (blockChain ++ [newBlock])
                                 | otherwise = Nothing

run = do
    b1 <- genesisBlock
    b2 <- generateNextBlock b1 "data_1"
    b3 <- generateNextBlock b2 "data_2"
    let chain = [b1,b2,b3]
    newChain <- mine chain "data_3"
    return newChain

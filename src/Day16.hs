{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Day16 where
import           Control.Monad.Cont    (replicateM_)
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Prelude               hiding (until)

newtype Version = Version BS.ByteString deriving (Show, Eq)
newtype TypeId = TypeId BS.ByteString deriving (Show, Eq)
data LengthType = TotalLength Char BS.ByteString Int
                | NumberOfSubPackets Char BS.ByteString Int
                deriving (Show, Eq)

data Value = Value Char BS.ByteString
           | LastValue Char BS.ByteString
           deriving (Show, Eq)

data Packet = Literal Version TypeId [Value]
            | Operator Version TypeId LengthType [Packet]
            deriving (Show, Eq)

newtype Parser a = Parser (BS.ByteString -> Maybe (a, BS.ByteString))


eval :: Packet -> Int
eval (Operator _ (TypeId "000") _ packets) = sum $ map eval packets
eval (Operator _ (TypeId "001") _ packets) = product $ map eval packets
eval (Operator _ (TypeId "010") _ packets) = minimum $ map eval packets
eval (Operator _ (TypeId "011") _ packets) = maximum $ map eval packets
eval (Operator _ (TypeId "101") _ [pkt1, pkt2]) = if eval pkt1 > eval pkt2 then 1 else 0
eval (Operator _ (TypeId "110") _ [pkt1, pkt2]) = if eval pkt1 < eval pkt2 then 1 else 0
eval (Operator _ (TypeId "111") _ [pkt1, pkt2]) = if eval pkt1 == eval pkt2 then 1 else 0
eval (Literal _ _ values) = toInt $ BS.concat $ map helper values
    where
        helper (Value _ s) = s
        helper (LastValue _ s) = s
eval _ = error "eval error"


sumVersion  :: Packet -> Int
sumVersion pkt = sum $ map f $ collectVersion pkt
    where
        f (Version v) = toInt v

        collectVersion :: Packet -> [Version]
        collectVersion (Literal version _ _) = [version]
        collectVersion (Operator version _ _ packets) = version : concatMap collectVersion packets


versionParser :: Parser Version
versionParser = Parser (\s -> Just (Version $ BS.take 3 s, BS.drop 3 s))

typeIdParser :: Parser TypeId
typeIdParser = Parser (\s -> Just (TypeId $ BS.take 3 s, BS.drop 3 s))

lengthTypeParser :: Parser LengthType
lengthTypeParser = Parser $ \s -> let
                                    (c, s1) = (BS.head s, BS.tail s)
                                  in
                                    if c == '0' then
                                        let
                                            (s2, s3) = (BS.take 15 s1, BS.drop 15 s1)
                                        in
                                            Just (TotalLength c s2 (toInt s2), s3)
                                    else
                                        let
                                            (s2, s3) = (BS.take 11 s1, BS.drop 11 s1)
                                        in
                                            Just (NumberOfSubPackets c s2 (toInt s2), s3)


valueParser :: Parser Value
valueParser = Parser $ \s -> let
                                (c, s') = (BS.head s, BS.tail s)
                             in
                                if c == '1' then Just (Value c (BS.take 4 s'), BS.drop 4 s')
                                else Just (LastValue c (BS.take 4 s'), BS.drop 4 s')


packetParser = do
    version <- versionParser
    typeId <- typeIdParser
    case typeId of
        TypeId "100" -> do
                        vs <- until isLastValue valueParser
                        return $ Literal version typeId vs
        _ -> do
            lengthType <- lengthTypeParser
            packets <- case lengthType of
                            (TotalLength _ _ n)        -> readBitsN n packetParser
                            (NumberOfSubPackets _ _ n) -> readPacketN n packetParser
            return $ Operator version typeId lengthType packets

    where
        isLastValue (LastValue _ _) = True
        isLastValue _               = False



runParser :: Parser a -> BS.ByteString -> Maybe (a, BS.ByteString)
runParser (Parser p) = p

until :: (a -> Bool) -> Parser a -> Parser [a]
until pred p = go pred p []
    where
        go pre p acc = do
            v <- p
            if pre v then return $ acc ++ [v]
            else go pre p $ acc ++ [v]

readBitsN :: (HasLength a) => Int -> Parser a -> Parser [a]
readBitsN n parser = go n parser []
    where
        go :: (HasLength a) => Int -> Parser a -> [a] -> Parser [a]
        go limit parser acc = if limit <= sum (map bitLength acc) then return acc
                       else do
                                pk <- parser
                                go limit parser (acc ++ [pk])


readPacketN :: Int -> Parser a -> Parser [a]
readPacketN n parser = go n parser []
    where
        go :: Int -> Parser a -> [a] -> Parser [a]
        go n parser acc = if length acc >= n then return acc
                          else do
                                pk <- parser
                                go n parser (acc ++ [pk])

ignore :: Parser ()
ignore = Parser $ \s -> Just ((), BS.tail s)

ignoreN :: Int -> Parser ()
ignoreN n = replicateM_ n ignore

ignoreBitsCount :: Int -> Int
ignoreBitsCount b = head [4 * n | n <- [1..], 4 * n >= b] - b

toInt :: BS.ByteString -> Int
toInt s = bitToInt $ map digitToInt $ BS.unpack s
    where
        bitToInt :: [Int] -> Int
        bitToInt bs = go 0 0 $ reverse bs
        go ex acc []     = acc
        go ex acc (b:bs) = go (ex + 1) (acc + b * (2 ^ ex)) bs

hexToBits :: BS.ByteString -> BS.ByteString
hexToBits = BS.concatMap hTob
    where
        hTob '0' = "0000"
        hTob '1' = "0001"
        hTob '2' = "0010"
        hTob '3' = "0011"
        hTob '4' = "0100"
        hTob '5' = "0101"
        hTob '6' = "0110"
        hTob '7' = "0111"
        hTob '8' = "1000"
        hTob '9' = "1001"
        hTob 'A' = "1010"
        hTob 'B' = "1011"
        hTob 'C' = "1100"
        hTob 'D' = "1101"
        hTob 'E' = "1110"
        hTob 'F' = "1111"
        hTob _   = error "Error!"

class HasLength a where
    bitLength :: a -> Int

instance HasLength Version where
    bitLength (Version s) = BS.length s

instance HasLength TypeId where
    bitLength (TypeId s) = BS.length s

instance HasLength LengthType where
    bitLength (TotalLength _ s _)        = 1 + BS.length s
    bitLength (NumberOfSubPackets _ s _) = 1 + BS.length s

instance HasLength Value where
    bitLength (Value _ s)     = 1 + BS.length s
    bitLength (LastValue _ s) = 1 + BS.length s

instance HasLength Packet where
    bitLength (Literal version typeId vs) = bitLength version + bitLength typeId + sum (map bitLength vs)
    bitLength (Operator version typeId lengthType packets) = bitLength version + bitLength typeId + bitLength lengthType + sum (map bitLength packets)


instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ \s -> case runParser p s of
                                Just (a, s') -> Just (f a, s')
                                Nothing      -> Nothing

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = Parser $ \s -> Just (a,s)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> pa = Parser $ \s -> case runParser pf s of
                                    Just (f, s') -> runParser (fmap f pa) s'
                                    Nothing      -> Nothing

instance Monad Parser where
    return :: a -> Parser a
    return = pure
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = Parser $ \s -> case runParser p s of
                                Just (a, s') -> runParser (f a) s'
                                Nothing      -> Nothing

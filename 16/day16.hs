import Numeric (readBin, readHex, showBin)

type Bits = [Char]

data Packet =   LiteralPacket { version :: Int, value :: Integer } |
                OperatorPacket { version :: Int, typeID :: Int, subpackets :: [Packet] } deriving (Show)

main = do
    input <- fmap (head . lines) $ readFile "input.txt"
    let bits = readBits input
    let (packet, _) = readPacket bits
    putStrLn $ "The answer to Part 1 is " ++ (show $ sumVersions [packet])
    putStrLn $ "The answer to Part 2 is " ++ (show $ getValue packet)


readBits :: String -> Bits
readBits str =  let bits = showBin (fst $ head $ readHex str) ""
                    -- We lose any leading zeros during the conversion to and from a nummber. We must add them back
                    lostZerosCount = (4 * length str) - length bits
                in (take lostZerosCount ['0','0'..]) ++ bits

parseBits :: Integral a => Bits -> a
parseBits = fst . head . readBin


sumVersions :: [Packet] -> Int
sumVersions [] = 0
sumVersions ((LiteralPacket ver _) : otherPackets) = ver + sumVersions otherPackets
sumVersions ((OperatorPacket ver _ subpackets) : otherPackets) = ver + sumVersions subpackets + sumVersions otherPackets

getValue :: Packet -> Integer
getValue (LiteralPacket _ value) = value
getValue (OperatorPacket _ 0 subpackets) = sum $ map getValue subpackets
getValue (OperatorPacket _ 1 subpackets) = product $ map getValue subpackets
getValue (OperatorPacket _ 2 subpackets) = minimum $ map getValue subpackets
getValue (OperatorPacket _ 3 subpackets) = maximum $ map getValue subpackets
getValue (OperatorPacket _ 5 [sub1, sub2]) = if getValue sub1 > getValue sub2 then 1 else 0
getValue (OperatorPacket _ 6 [sub1, sub2]) = if getValue sub1 < getValue sub2 then 1 else 0
getValue (OperatorPacket _ 7 [sub1, sub2]) = if getValue sub1 == getValue sub2 then 1 else 0


readAllPackets :: Bits -> [Packet]
readAllPackets bits
    | null bits = []
    | all (=='0') bits = []
    | otherwise = let (packet, rest) = readPacket bits in packet : readAllPackets rest

readNPackets :: Int -> Bits -> ([Packet], Bits)
readNPackets n bits
    | n == 0 = ([], bits)
    | otherwise =   let (packet, rest) = readPacket bits
                        (packets, unused) = readNPackets (n - 1) rest
                    in (packet:packets, unused)

readPacket :: Bits -> (Packet, Bits)
readPacket (a:b:c:d:e:f:rest) = let version = parseBits [a,b,c]
                                    typeID = parseBits [d,e,f]
                                in if typeID == 4 then readLiteral version rest else readOperator version typeID rest

readLiteral :: Int -> Bits -> (Packet, Bits)
readLiteral v rest = let (value, unused) = readGroups rest in (LiteralPacket v (parseBits value), unused)
    where   readGroups ('0':a:b:c:d:unused) = ([a,b,c,d], unused)
            readGroups ('1':a:b:c:d:rest) = let (value, unused) = readGroups rest in ([a,b,c,d] ++ value, unused)

readOperator :: Int -> Int -> Bits -> (Packet, Bits)
readOperator v t ('0':rest) = readLenT0Operator v t (splitAt 15 rest)
readOperator v t ('1':rest) = readLenT1Operator v t (splitAt 11 rest)

readLenT0Operator :: Int -> Int -> (Bits, Bits) -> (Packet, Bits)
readLenT0Operator v t (n, rest) =   let subpacketBitCount = parseBits n
                                        (subpacketBits, unused) = splitAt subpacketBitCount rest
                                        subpackets = readAllPackets subpacketBits
                                    in (OperatorPacket v t subpackets, unused)

readLenT1Operator :: Int -> Int -> (Bits, Bits) -> (Packet, Bits)
readLenT1Operator v t (n, rest) =   let subpacketCount = parseBits n
                                        (subpackets, unused) = readNPackets subpacketCount rest
                                    in (OperatorPacket v t subpackets, unused)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString as B
import Data.Word
import Data.Binary.Strict.Get
import Data.Bits
import qualified Data.ByteString.Char8 as BChar8
import Control.Applicative
import Data.Maybe

main = do startListening "11337"

startListening :: String -> IO()
startListening port = withSocketsDo $
    do
        addrinfos <- getAddrInfo 
                     (Just defaultHints {addrFlags = [AI_PASSIVE]})
                     Nothing (Just port)
        let serveraddr = head ( filter (\x -> (addrFamily x) == AF_INET)  addrinfos)
        listenSocket <- socket (addrFamily serveraddr) Datagram defaultProtocol
        bindSocket listenSocket (addrAddress serveraddr)
        
        procMessages listenSocket
    where procMessages sock = do
            putStrLn "Recieving"
            (msg, addr) <- recvFrom sock 1024
            putStrLn "Recieved"
            putStrLn $ show (readDhcpPacket msg)
            procMessages sock

readDhcpPacket :: B.ByteString -> Maybe DhcpV4Packet
readDhcpPacket rawData = readDhcpPacket' (runGet deserializePacket rawData)
    where readDhcpPacket' (Right a, _) = a  -- Unpack the correct result
          readDhcpPacket' _ = Nothing       -- Throw away the faulty result

deserializePacket :: Get (Maybe DhcpV4Packet)
deserializePacket = do
        opCode <- deserializeOpCode <$> getWord8
        htype <- deserializeHtype <$> getWord8
        hlen <- getWord8
        hops <- getWord8
        xid <- getWord32be
        secs <- getWord16be
        flags <- deserializeFlags <$> getWord16be 
        ciaddr <- getWord32be
        yiaddr <- getWord32be
        giaddr <- getWord32be
        siaddr <- getWord32be        
        chaddr <- B.unpack . B.take (fromIntegral hlen) <$> getByteString 16
        sname <- toString <$> getByteString 64
        file <- toString <$> getByteString 128
        magic <- getWord32be
        options <- deserializeOptions
        return $ deserializePacket' opCode 
                                    htype 
                                    hlen 
                                    hops 
                                    xid 
                                    secs 
                                    flags 
                                    ciaddr 
                                    yiaddr 
                                    giaddr 
                                    siaddr 
                                    chaddr 
                                    sname 
                                    file 
                                    magic
                                    options
    where deserializePacket' (Just op') 
                             (Just htype') 
                             hlen' 
                             hops' 
                             xid' 
                             secs' 
                             flags' 
                             ciaddr' 
                             yiaddr' 
                             siaddr' 
                             giaddr' 
                             chaddr' 
                             sname' 
                             file' 
                             0x63825363
                             options' = Just DhcpV4Packet { 
                op = op', 
                htype = htype',
                hlen = hlen',
                hops = hops',
                xid = xid',
                secs = secs',
                flags = flags',
                ciaddr = ciaddr', 
                yiaddr = yiaddr',
                giaddr = giaddr',
                siaddr = siaddr',
                chaddr = chaddr',
                sname = sname',
                file = file',
                options = options' }
          deserializePacket' _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = Nothing 

data DhcpV4_option = OPT_SUBNET_MASK Word32 
                   | OPT_TIME_OFFSET Word32
                   | OPT_ROUTER [Word32]
                   | OPT_TIME_SERVER [Word32]
                   | OPT_NAME_SERVER [Word32]
                   | OPT_DOMAIN_NAME_SERVER [Word32]
                   | OPT_LOG_SERVER [Word32]
                   | OPT_UNKNOWN Word8 [Word8] deriving Show 

deserializeOptions :: Get([DhcpV4_option])
deserializeOptions = do 
    optionCode <- getWord8
    case optionCode of 0 -> deserializeOptions
                       255 -> return []
                       _ -> do optionLength <- getWord8
                               optionData <- B.unpack <$> getByteString (fromIntegral optionLength)
                               let option = deserializeOption optionCode optionData
                               options <- deserializeOptions
                               return $ option:options

deserializeOption :: Word8 -> [Word8] -> DhcpV4_option
deserializeOption 1 x@(_:_:_:_:[]) = OPT_SUBNET_MASK (toWord32 x) 
deserializeOption 2 x@(_:_:_:_:[]) = OPT_TIME_OFFSET (toWord32 x)
deserializeOption optionCode optionData
    | optionCode >= 3 && optionCode <= 7 && isJust ctor = deserializeWord32Option optionCode optionData (fromJust ctor)
    where ctor = find4OctetsConstructor optionCode
deserializeOption optionCode optionData = OPT_UNKNOWN optionCode optionData

find4OctetsConstructor :: Word8 -> Maybe ([Word32] -> DhcpV4_option)
find4OctetsConstructor 3 = Just OPT_ROUTER
find4OctetsConstructor 4 = Just OPT_TIME_SERVER
find4OctetsConstructor 5 = Just OPT_NAME_SERVER
find4OctetsConstructor 6 = Just OPT_DOMAIN_NAME_SERVER
find4OctetsConstructor 7 = Just OPT_LOG_SERVER
find4OctetsConstructor _ = Nothing

deserializeWord32Option :: Word8 -> [Word8] -> ([Word32] -> DhcpV4_option) -> DhcpV4_option
deserializeWord32Option code content makeOption = if length content `mod` 4 == 0 
                            then makeOption (toWords32 content)
                            else OPT_UNKNOWN code content 

deserializeFlags :: Word16 -> [DhcpV4_flag]
deserializeFlags f = if (f .&. 0x8000) /= 0 then [FLAG_BROADCAST] else []

toWords32 :: [Word8] -> [Word32]
toWords32 [] = []
toWords32 x = (toWord32 $ take 4 x):(toWords32 $ drop 4 x)

toWord32 :: [Word8] -> Word32
toWord32 = foldr1 (\b acc -> ((acc `shiftL` 8) .|. b)) . map fromIntegral

toString :: B.ByteString -> String
toString = BChar8.unpack . head . B.split 0

data DhcpV4_op = BOOTREQUEST | BOOTREPLY deriving Show

deserializeOpCode :: Word8 -> Maybe DhcpV4_op
deserializeOpCode 1 = Just BOOTREPLY
deserializeOpCode 2 = Just BOOTREQUEST
deserializeOpCode _ = Nothing

data DhcpV4_htype = HTYPE_ETHER | HTYPE_IEEE802 | HTYPE_FDDI deriving Show

deserializeHtype :: Word8 -> Maybe DhcpV4_htype
deserializeHtype 1 = Just HTYPE_ETHER
deserializeHtype 6 = Just HTYPE_IEEE802
deserializeHtype 8 = Just HTYPE_FDDI
deserializeHtype _ = Nothing

data DhcpV4_flag = FLAG_BROADCAST deriving Show

data DhcpV4Packet = DhcpV4Packet { op :: DhcpV4_op
                                 , htype :: DhcpV4_htype 
                                 , hlen :: Word8
                                 , hops :: Word8
                                 , xid :: Word32
                                 , secs :: Word16
                                 , flags :: [DhcpV4_flag]
                                 , ciaddr :: Word32
                                 , yiaddr :: Word32
                                 , siaddr :: Word32
                                 , giaddr :: Word32
                                 , chaddr :: [Word8]
                                 , sname :: String
                                 , file :: String
                                 , options :: [DhcpV4_option]
                                 } deriving (Show)
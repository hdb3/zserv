{-# LANGUAGE MultiWayIf,RecordWildCards,FlexibleInstances #-}
module ZMsgBinary where

import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString as BS
import Data.Word
import Data.IP
import Data.Bits
import Data.Monoid((<>))

import ZMsg
import ZSpec

-- TODO!!!!
-- make all of the implicit puword16/32 explicit for endianess!!!

-- Entry points / public interface

instance Binary ZMsg where
    get = undefined
    put ( ZMHello w8 ) = put _ZEBRA_HELLO <> put w8
    put ( ZMQRouterIdAdd ) = put _ZEBRA_ROUTER_ID_ADD
    put ( ZMQInterfaceAdd ) = put _ZEBRA_INTERFACE_ADD
    put ( ZMRouterIDUpdate prefix ) = put _ZEBRA_ROUTER_ID_UPDATE <> putZPrefix8 prefix
    put ( ZMNextHopRegister reg ) = put _ZEBRA_NEXTHOP_REGISTER <> put reg
    put ( ZMNextHopUnregister reg ) = put _ZEBRA_NEXTHOP_UNREGISTER <> put reg
    put ( ZMInterfaceAdd interface ) = put _ZEBRA_INTERFACE_ADD <> put interface
    put ( ZMInterfaceAddressAdd intAddr ) = put _ZEBRA_INTERFACE_ADDRESS_ADD <> put intAddr
    put z = error $ "put ZMsg failed for ZMsg: " ++ show z 

instance Binary ZInterfaceAddress where
    get = undefined
    put ZInterfaceAddressV4{..} = put ifindex <> put flags <> put _AF_INET <> put addressA <> put plen <> put addressB
    put ZInterfaceAddressV6{..} = put ifindex <> put flags <> put _AF_INET6 <> put v6addressA <> put plen <> put v6addressB

{-
zInterfaceAddressParser :: Parser ZInterfaceAddress
zInterfaceAddressParser = do
    ifindex <- anyWord32be
    flags <- anyWord8
    afi  <- anyWord8
    if | afi == _AF_INET  -> zInterfaceAddressParserV4 ifindex flags
       | afi == _AF_INET6 -> zInterfaceAddressParserV6 ifindex flags

zInterfaceAddressParserV4 ifindex flags = do
    addressA <- zIPv4
    plen <- anyWord8
    addressB <- zIPv4
    return ZInterfaceAddressV4{..}

zInterfaceAddressParserV6 ifindex flags = do
    v6addressA <- zIPv6
    plen <- anyWord8
    v6addressB <- zIPv6
    return ZInterfaceAddressV6{..}

-}
-- **********************************************************************************

instance Binary IPv4 where

    get = undefined
    put = putWord32le . toHostAddress

instance Binary IPv6 where

    get = undefined
    -- this would be the most direct form but Data.IP hides this constructor so we have to be indirect for simplicity
    -- put (IP6 (w1, w2, w3, w4)) = put w1 <> put w2 <> put w3 <> put w4
    put ipV6 = mapM_ putWord8 (map fromIntegral $ fromIPv6b ipV6)


-- this puts 16 bit AFI, fixed length prefix, prefix last
putZPrefix16 :: ZPrefix -> Put
putZPrefix16 ZPrefixV4{..} = put (fromIntegral _AF_INET :: Word16 ) <> put plen <> put v4address
putZPrefix16 ZPrefixV6{..} = put (fromIntegral _AF_INET6 :: Word16 ) <> put plen <> put v6address

-- this puts 8 bit AFI, fixed length prefix, prefix length last
putZPrefix8 :: ZPrefix -> Put
putZPrefix8 ZPrefixV4{..} = put _AF_INET  <> put v4address <> put plen
putZPrefix8 ZPrefixV6{..} = put _AF_INET6 <> put v6address <> put plen

-- placeholder for fixing ZRoute to hold v4 and v6...
putzvPrefix ZPrefixV4{..} = do
    put plen
    let address = toHostAddress v4address
    if | plen == 0  -> return ()
       | plen < 9   -> putWord8 (unsafeShiftR (fromIntegral address) 24)
       | plen < 17  -> putWord16be (unsafeShiftR (fromIntegral address) 16)
       | plen < 25  -> putWord16be (unsafeShiftR (fromIntegral address) 16) >> putWord8 (unsafeShiftR (fromIntegral address) 8)
       | plen < 33  -> put v4address
       | otherwise -> error $ "putzvPrefix: invalid plen - " ++ show plen
{-
readPrefix1Byte = do
    b0 <- anyWord8
    return (unsafeShiftL (fromIntegral b0) 24)

readPrefix2Byte = do
    b0 <- anyWord16be
    return (unsafeShiftL (fromIntegral b0) 16) 

readPrefix3Byte = do
    b0 <- anyWord16be
    b1 <- anyWord8
    return (fromIntegral b1 .|. unsafeShiftL (fromIntegral b0) 16)
    
readPrefix4Byte = anyWord32be
    
zvPrefixIPv4Parser :: Parser ZPrefix
zvPrefixIPv4Parser = do
    plen <- anyWord8
    prefix' <- 
        if | plen == 0  -> return 0
           | plen < 9   -> readPrefix1Byte
           | plen < 17  -> readPrefix2Byte
           | plen < 25  -> readPrefix3Byte
           | plen < 33  -> readPrefix4Byte 
    let v4address = fromHostAddress $ byteSwap32 prefix'
    return ZPrefixV4{..}


-}

instance {-# OVERLAPPING #-} Binary [ZNextHop] where
    get = undefined
    put nexthops = put (fromIntegral (length nexthops) :: Word8 ) <> mapM_ put nexthops

instance Binary ZNextHop where
    get = undefined
    put ZNHBlackhole = put _ZEBRA_NEXTHOP_BLACKHOLE
    put ( ZNHIPv4 ip) = put _ZEBRA_NEXTHOP_IPV4 <> put ip
    put ( ZNHIfindex ifindex) = put _ZEBRA_NEXTHOP_IFINDEX <> put ifindex
    --TODO .....
    -- ZNHIPv4Ifindex IPv4 Word32
    -- ZNHIPv6 IPv6
    -- ZNHIPv6Ifindex IPv6 Word32


instance Binary ZNextHopRegister where
    get = undefined
    put ZNextHopRegister{..} = putWord8 connectedW8 <> putZPrefix16 prefix where
        connectedW8 = if connected  then 0x01 else 0x00
        -- arbitraryW8 = 0x01

-- data ZNextHopUpdate = ZNextHopUpdate {flags :: Word8 , metric :: Word32 ,  prefix :: ZPrefix , nexthops :: [ZNextHop] } deriving (Eq,Show,Read)
instance Binary ZNextHopUpdate where
    get = undefined
    put ZNextHopUpdate{..} = put flags <> put metric <> putZPrefix16 prefix <> put (fromIntegral (length nexthops) :: Word8 )<> mapM_ put nexthops
    -- put flags <> put metric <> put prefix <> put (fromIntegral (length nexthops) :: Word8 )<> mapM_ put nexthops


instance Binary ZInterface where
    get = undefined
    -- put ZInterface{..} = putByteString ifname -- <> put ifindex <> put status <> put if_flags <> put metric <> put ifmtu <> put ifmtu6 <> put bandwidth <> put linkLayerType <> put hardwareAddress
    put ZInterface{..} = putByteString (pad 20 ifname) <> put ifindex <> put status <> put if_flags <> put metric <> put ifmtu <> put ifmtu6 <> put bandwidth <> put linkLayerType <> putCountedByteString hardwareAddress <> putWord8 0x00
putCountedByteString bs = putWord32be (fromIntegral (BS.length bs)) <> putByteString bs
pad n bs = BS.take n (BS.append bs (BS.replicate n 0x00))

{-
zInterfaceParser :: Int -> Parser ZInterface
zInterfaceParser n = do
    ifname' <- DAB.take 20
    let ifname = BS.takeWhile ( 0 /= ) ifname'
    ifindex <- anyWord32be
    status <- anyWord8
    if_flags <- anyWord64be
    metric <- anyWord32be
    ifmtu <- anyWord32be
    ifmtu6 <- anyWord32be
    bandwidth <- anyWord32be
    linkLayerType <- anyWord32be
    hardwareAddressLength <- anyWord32be
    hardwareAddress <- DAB.take (fromIntegral hardwareAddressLength)
    word8 0x00
    return $ --assert (n == 58 + fromIntegral hardwareAddressLength)

data ZInterface = ZInterface { ifname :: ByteString
                             , ifindex :: Word32
                             , status :: Word8
                             , if_flags :: Word64
                             , metric :: Word32
                             , ifmtu :: Word32
                             , ifmtu6 :: Word32
                             , bandwidth :: Word32
                             , linkLayerType :: Word32
                             , hardwareAddress :: ByteString
                             -- there is a placeholder here for 'link params'
                             -- which is for TE - but it is really longwinded so won't bother doing it now
                             } deriving (Eq,Show,Read)
-}


instance Binary ZRoute where
    get = undefined
    put ZRoute{..} = put zrType <> put zrFlags <> put zrMsg <> put zrSafi <> putzvPrefix zrPrefix <> put zrNextHops <> put zrDistance <> put zrMetric <> put zrMtu <> put zrTag

{-
-- *** TODO ***
data ZRoute = ZRoute { zrType :: Word8
                     , zrFlags :: Word8
                     , zrMsg :: Word8
                     , zrSafi :: Word16
                     , zrPrefix :: ZPrefix
                     , zrNextHops :: [ZNextHop]
                     , zrDistance :: Maybe Word8
                     , zrMetric :: Maybe Word32
                     , zrMtu :: Maybe Word32
                     , zrTag :: Maybe Word32
                     } deriving (Eq,Show,Read)

data ZInterfaceAddress = ZInterfaceAddressV4 { ifindex :: Word32
                                             , flags :: Word8
                                             , addressA :: IPv4
                                             , plen :: Word8
                                             , addressB :: IPv4
                                             }  |
                         ZInterfaceAddressV6 { ifindex :: Word32
                                             , flags :: Word8
                                             , v6addressA :: IPv6
                                             , plen :: Word8
                                             , v6addressB :: IPv6
                                             } deriving (Eq,Show,Read)
-}

{-# LANGUAGE RecordWildCards,MultiWayIf,OverloadedStrings #-}
module WireFormat where
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString -- from package attoparsec
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.Binary -- from package attoparsec-binary
import Control.Applicative
import Control.Monad(when)
import Control.Exception -- only if assert is used...
import Data.IP
import Data.Bits
import Data.Word

import ZMsg
import ZSpec

zFlowParser :: Parser [ZMsg]
zFlowParser = many1 zMessageParser'


zRawFlowParser :: Parser [(Word16,BS.ByteString)]
zRawFlowParser = many1 zRawMessageParser
zRawMessageParser :: Parser (Word16,BS.ByteString)
zRawMessageParser = do
    msgLen <- anyWord16be
    word8 0xff
    word8 0x03
    word16be 0x0000
    when (msgLen > 4096 || msgLen < 8) ( fail "invalid message length")
    -- cmd <- anyWord16be
    -- pl <- DAB.take (fromIntegral msgLen - 8)
    -- return (cmd,pl)

    -- this version replaces the command word in the bytestring to allow another parser to be run over it
    (cmdBS,cmd) <- match anyWord16be
    pl <- DAB.take (fromIntegral msgLen - 8)
    return (cmd,BS.append cmdBS pl)



zMessageParser :: Parser (Maybe ZMsg)
zMessageParser = ( zMessageParser'' <|> return Nothing ) <?> "zserv wire format parser"
zMessageParser'' = do
    tmp <- zMessageParser'
    return $ Just tmp

zMessageParser' = do
    msgLen <- anyWord16be
    word8 0xff -- 'marker'
    word8 0x03 -- version - earlier than three has incompatible format - later is unknown
    word16be 0x0000 -- VRF ID - for now we would not want to know about multiple VRFs - the default value is zero
    when (msgLen > 4096 || msgLen < 8) ( fail "invalid message length")
    -- msg <- Data.Attoparsec.ByteString.take $ fromIntegral (msgLen - 6) 
    -- return $ Just msg
    zParser $ fromIntegral (msgLen - 6)


zParser :: Int -> Parser ZMsg
zParser n' = do
   let n = n'-2
   cmd' <- anyWord16be
   let cmd = cmd'
   --let cmd = assert (cmd `elem` zKnownCommands) cmd'
   if | cmd == _ZEBRA_HELLO && n == 1 -> do protocol <- anyWord8
                                            return $ ZHello protocol

      | cmd == _ZEBRA_INTERFACE_ADD ->
          do interface <- zInterfaceParser n
             return $ ZInterfaceAdd interface

      | cmd == _ZEBRA_INTERFACE_ADDRESS_ADD ->
          do zia <- zInterfaceAddressParser
             return $ ZInterfaceAddressAdd zia

      | cmd == _ZEBRA_ROUTER_ID_UPDATE ->
              do prefix <- zPrefixIPv4Parser n
                 return $ ZRouterIDUpdate prefix

      | cmd == _ZEBRA_IPV4_ROUTE_DELETE ->
          do route <- zRouteParser n
             return $ ZIPV4RouteDelete route

      | cmd == _ZEBRA_NEXTHOP_UNREGISTER ->
          do up <- zNextHopUpdateParser -- n
             return $ ZNexthopUnregister up

      | cmd == _ZEBRA_NEXTHOP_UPDATE ->
          do route <- zRouteParser n
             return $ ZNexthopUpdate route

      | otherwise -> do
            payload <- DAB.take n
            return $ ZUnknown cmd payload

zNextHopParser :: Parser ZNextHop
zNextHopParser = do
    nextHopType <- anyWord8
    if | nextHopType == _ZEBRA_NEXTHOP_BLACKHOLE -> return ZNHBlackhole
       | nextHopType == _ZEBRA_NEXTHOP_IPV4 -> do
             w32 <- anyWord32be
             return $ ZNHIPv4 (fromHostAddress w32)
       | nextHopType == _ZEBRA_NEXTHOP_IFINDEX -> do
             w32 <- anyWord32be
             return $ ZNHBIfindex w32

zNextHopUpdateParser :: Parser ZNextHopUpdate
zNextHopUpdateParser = do
    flags <- anyWord8
    afi16  <- anyWord16be
    let afi = fromIntegral afi16 -- yes - in thi smmessage the AFI is 16bits!!
                                 -- and, also, the prefix is not compressed....
    plen <- anyWord8
    if | afi == _AF_INET  -> do v4address <- zIPv4
                                return ZNextHopVUpdate4{..}
       | afi == _AF_INET6 -> do v6address <- zIPv6
                                return ZNextHopVUpdate6{..}

--data ZNextHopUpdate = ZNextHopVUpdate4 {flags :: Word8 ,  plen :: Word8, v4address :: IPv4 } |
--                      ZNextHopVUpdate6 {flags :: Word8 ,  plen :: Word8, v6address :: IPv6 } deriving (Eq,Show,Read)


zInterfaceAddressParser :: Parser ZInterfaceAddress
zInterfaceAddressParser = do
    ifindex <- anyWord32be
    flags <- anyWord8
    afi  <- anyWord8
    if | afi == _AF_INET  -> zInterfaceAddressParserV4 ifindex flags
       | afi == _AF_INET6 -> zInterfaceAddressParserV6 ifindex flags

zInterfaceAddressParserV4 ifindex flags = do
    --addressA' <- anyWord32le
    --let addressA = fromHostAddress addressA'
    addressA <- zIPv4
    plen <- anyWord8
    addressB <- zIPv4
    --addressB' <- anyWord32le
    --let addressB = fromHostAddress addressB'
    return ZInterfaceAddressV4{..}

zInterfaceAddressParserV6 ifindex flags = do
    --v6addressA' <- DAB.take 16
    --let v6addressA = bsToIPv6 v6addressA'
    v6addressA <- zIPv6
    plen <- anyWord8
    v6addressB <- zIPv6
    --v6addressB' <- DAB.take 16
    --let v6addressB = bsToIPv6 v6addressB'
    return ZInterfaceAddressV6{..}
    -- where
    -- bsToIPv6 = toIPv6b . map fromIntegral . BS.unpack

zIPv4 = zIPv4Parser
zIPv4Parser :: Parser IPv4
zIPv4Parser = do
    v4address <- anyWord32le
    return $ fromHostAddress v4address

zIPv6 = zIPv6Parser
zIPv6Parser :: Parser IPv6
zIPv6Parser = do
    v6address <- DAB.take 16
    return $ (toIPv6b . map fromIntegral . BS.unpack) v6address



zRouteParser :: Int -> Parser ZRoute
zRouteParser n = do
    zrType <- anyWord8
    zrFlags <- anyWord8
    zrMsg <- anyWord8
    zrSafi <- anyWord16be
    zrPrefix <-  zvPrefixIPv4Parser
    zrNextHops <- if testBit zrMsg _ZAPI_MESSAGE_NEXTHOP then do nextHopCount <- anyWord8
                                                                 count (fromIntegral nextHopCount) zNextHopParser
                                                         else return []
    zrDistance <- if testBit zrMsg _ZAPI_MESSAGE_DISTANCE then fmap Just anyWord8 else return Nothing
    zrMetric <- if testBit zrMsg _ZAPI_MESSAGE_METRIC then fmap Just anyWord32be else return Nothing
    zrMtu <- if testBit zrMsg _ZAPI_MESSAGE_MTU then fmap Just anyWord32be else return Nothing
    zrTag <- if testBit zrMsg _ZAPI_MESSAGE_TAG then fmap Just anyWord32be else return Nothing
    return ZRoute{..}

-- refer to zclient.c for the specification of this structure

zInterfaceParser :: Int -> Parser ZInterface
zInterfaceParser n = do
    ifname' <- DAB.take 20
    let ifname = BS.takeWhile ( 0 /= ) ifname'
    -- ifname <- DAB.takeWhile ( 0 /= )
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
             ZInterface {..} 

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
    let prefix = fromHostAddress $ byteSwap32 prefix'
    return ZPrefix{..}

zPrefixIPv4Parser :: Int -> Parser ZPrefix
zPrefixIPv4Parser n = do

    word8 _AF_INET
    prefix' <- anyWord32le
    -- why this is anyWord32le not anyWord32be i have no idea...
    let prefix = fromHostAddress prefix'
    plen <- anyWord8
    return ZPrefix{..}

{- NOTE - there are alternative forms for prefix i.e.:

AF_INET =2 - size 4
AF_INET6 = 10 size 16
AF_ETHERNET = AF_PACKET = 17 size 6
-}

{-# LANGUAGE MultiWayIf,RecordWildCards,FlexibleInstances #-}
module ZMsgBinary where

import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString as BS
import Data.Word
import Data.IP
import Data.Bits
import Data.Monoid((<>))
import Data.Maybe(isJust,fromJust)
import Control.Monad(when)

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
    put ( ZMIPV4RouteAdd route ) = put _ZEBRA_IPV4_ROUTE_ADD <> put route
    put ( ZMIPV4RouteDelete route ) = put _ZEBRA_IPV4_ROUTE_DELETE <> put route
    put z = error $ "put ZMsg failed for ZMsg: " ++ show z 

-- **********************************************************************************

instance Binary IPv4 where

    get = undefined
    put = putWord32le . toHostAddress -- note le not be because of the way Data.IP represents IPv4 internally!

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
    let address = byteSwap32 $ toHostAddress v4address
    -- note byteSwap because of the way Data.IP represents IPv4 internally!
    if | plen == 0  -> return ()
       | plen < 9   -> putWord8 (fromIntegral (unsafeShiftR address 24))
       | plen < 17  -> putWord16be ( fromIntegral (unsafeShiftR address 16))
       | plen < 25  -> putWord16be ( fromIntegral (unsafeShiftR address 16)) >> putWord8 (fromIntegral (unsafeShiftR address 8))
       | plen < 33  -> put v4address
       | otherwise -> error $ "putzvPrefix: invalid plen - " ++ show plen

putRoutePrefixV4 pfx@ZPrefixV4{..} = putWord16be 0x01 <> -- 'SAFI' for IPv4!?
                                 --putWord8 plen <> put v4address
                                 putzvPrefix pfx 

instance Binary ZInterfaceAddress where
    get = undefined
    put ZInterfaceAddressV4{..} = put ifindex <> put flags <> put _AF_INET <> put addressA <> put plen <> put addressB
    put ZInterfaceAddressV6{..} = put ifindex <> put flags <> put _AF_INET6 <> put v6addressA <> put plen <> put v6addressB

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

instance Binary ZNextHopUpdate where
    get = undefined
    put ZNextHopUpdate{..} = put flags <> put metric <> putZPrefix16 prefix <> put (fromIntegral (length nexthops) :: Word8 )<> mapM_ put nexthops


instance Binary ZInterface where
    get = undefined
    put ZInterface{..} = putByteString (pad 20 ifname) <> put ifindex <> put status <> put if_flags <> put metric <> put ifmtu <> put ifmtu6 <> put bandwidth <> put linkLayerType <> putCountedByteString hardwareAddress <> putWord8 0x00
putCountedByteString bs = putWord32be (fromIntegral (BS.length bs)) <> putByteString bs
pad n bs = BS.take n (BS.append bs (BS.replicate n 0x00))


instance Binary ZRoute where
    get = undefined
    put ZRoute{..} =
        do
        let zrMsg      = if null zrNextHops then 0 else bit _ZAPI_MESSAGE_NEXTHOP :: Word8
            zrMsg'     = zrMsg      .|. if isJust zrDistance then bit _ZAPI_MESSAGE_DISTANCE else 0
            zrMsg''    = zrMsg'     .|. if isJust zrMetric then bit _ZAPI_MESSAGE_METRIC else 0
            zrMsg'''   = zrMsg''    .|. if isJust zrMtu then bit _ZAPI_MESSAGE_MTU else 0
            zrMsg''''  = zrMsg'''   .|. if isJust zrTag then bit _ZAPI_MESSAGE_TAG else 0
        put zrType <> put zrFlags <> put zrMsg'''' <> putRoutePrefixV4 zrPrefix
        when (not (null zrNextHops)) (put zrNextHops)
        when (isJust zrDistance) (put $ fromJust zrDistance)
        when (isJust zrMetric) (put $ fromJust zrMetric)
        when (isJust zrMtu) (put $ fromJust zrMtu)
        when (isJust zrTag) (put $ fromJust zrTag)


{-
    zrNextHops <- if testBit zrMsg _ZAPI_MESSAGE_NEXTHOP then do nextHopCount <- anyWord8
                                                                 count (fromIntegral nextHopCount) zNextHopParser
                                                         else return []
    zrDistance <- if testBit zrMsg _ZAPI_MESSAGE_DISTANCE then fmap Just anyWord8 else return Nothing
    zrMetric <- if testBit zrMsg _ZAPI_MESSAGE_METRIC then fmap Just anyWord32be else return Nothing
    zrMtu <- if testBit zrMsg _ZAPI_MESSAGE_MTU then fmap Just anyWord32be else return Nothing
    zrTag <- if testBit zrMsg _ZAPI_MESSAGE_TAG then fmap Just anyWord32be else return Nothing

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

-}

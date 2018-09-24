{-# LANGUAGE RecordWildCards #-}
module ZMsgBinary where

import Data.Binary
import Data.Binary.Put
import Data.ByteString
import Data.Word
import Data.IP
import Data.Monoid((<>))

import ZMsg
import ZSpec


-- Entry points / public interface

instance Binary ZMsg where
    get = undefined
    put ( ZMHello w8 ) = put _ZEBRA_HELLO <> put w8
    put ( ZMQRouterIdAdd ) = put _ZEBRA_ROUTER_ID_ADD
    put ( ZMQInterfaceAdd ) = put _ZEBRA_INTERFACE_ADD
    put ( ZMRouterIDUpdate zPrefix ) = put _ZEBRA_ROUTER_ID_UPDATE <> put' zPrefix
    put ( ZMNextHopRegister nh ) = put _ZEBRA_NEXTHOP_REGISTER <> put nh
    put z = error $ "put ZMsg failed for ZMsg: " ++ show z 

-- **********************************************************************************

instance Binary IPv4 where

    get = undefined
    put = putWord32le . toHostAddress

instance Binary IPv6 where

    get = undefined
    -- this is the most direct form but Data.IP hides this constructor so we have to be indirect for simplicity
    -- put (IP6 (w1, w2, w3, w4)) = put w1 <> put w2 <> put w3 <> put w4
    put ipV6 = mapM_ put (fromIPv6b ipV6)

instance Binary ZPrefix where
    get = undefined
    put ZPrefixV4{..} = put _AF_INET <> put plen <> put v4address
    put ZPrefixV6{..} = put _AF_INET6 <> put plen <> put v6address

put' ZPrefixV4{..} = put _AF_INET <> put v4address <> put plen
put' ZPrefixV6{..} = put _AF_INET6 <> put v6address <> put plen

instance Binary ZNextHop where
    get = undefined
    put ZNHBlackhole = put _ZEBRA_NEXTHOP_BLACKHOLE
    put ( ZNHIPv4 ip) = put _ZEBRA_NEXTHOP_IPV4 <> put ip
    put ( ZNHIfindex ifindex) = put _ZEBRA_NEXTHOP_IFINDEX <> put ifindex
    --TODO .....
    -- ZNHIPv4Ifindex IPv4 Word32
    -- ZNHIPv6 IPv6
    -- ZNHIPv6Ifindex IPv6 Word32



-- data ZNextHopUpdate = ZNextHopUpdate {flags :: Word8 , metric :: Word32 ,  prefix :: ZPrefix , nexthops :: [ZNextHop] } deriving (Eq,Show,Read)
instance Binary ZNextHopUpdate where
    get = undefined
    put ZNextHopUpdate{..} = put flags <> put metric <> put prefix <> put (fromIntegral (Prelude.length nexthops) :: Word8 )<> mapM_ put nexthops
    -- put flags <> put metric <> put prefix <> put (fromIntegral (Prelude.length nexthops) :: Word8 )<> mapM_ put nexthops


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

{-# LANGUAGE DuplicateRecordFields,RecordWildCards #-}
module ZMsg where
import Data.ByteString
import Data.Word
import Data.IP

data ZMsg = ZHello
            | ZInterfaceAdd ZInterface
            | ZInterfaceAddressAdd ZInterfaceAddress
            | ZRouterIDUpdate ZPrefix
            | ZIPV4RouteDelete ZRoute
            | ZNexthopUnregister { payload :: ByteString }
            | ZUnknown { cmd :: Word16 , payload :: ByteString }
    deriving (Eq,Show,Read)

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


data ZPrefix = ZPrefix { prefix :: IPv4
                       , plen :: Word8
                       } deriving (Eq,Show,Read)

data ZNextHop = ZNHBlackhole | ZNHIPv4 IPv4 | ZNHBIfindex Word32  deriving (Eq,Show,Read)

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

{-
reference is zclient.c, function zapi_ipv4_route

wire format is type,flags,msg,safi : 8,8,8,16
followed by variable length prefux (length, pfx bytes)
followed by nexthops, also varaiable length based on first byte, with a one buye count of next hops
the codes for next hops are ZEBRA_NEXTHOP_IPV4 / ZEBRA_NEXTHOP_IFINDEX / ZEBRA_NEXTHOP_BLACKHOLE
nexthops are only present when the flag ZAPI_MESSAGE_NEXTHOP is set in flags
blackholes are zero length the other two are 32
there are 4 more optional fields which are present when they have a bit set in 'message':
 if (CHECK_FLAG (api->message, ZAPI_MESSAGE_DISTANCE))
    stream_putc (s, api->distance);
  if (CHECK_FLAG (api->message, ZAPI_MESSAGE_METRIC))
    stream_putl (s, api->metric);
  if (CHECK_FLAG (api->message, ZAPI_MESSAGE_MTU))
    stream_putl (s, api->mtu);
  if (CHECK_FLAG (api->message, ZAPI_MESSAGE_TAG))
    stream_putl (s, api->tag);
the codes are defined in zebra.h

#define ZAPI_MESSAGE_NEXTHOP  0x01
#define ZAPI_MESSAGE_IFINDEX  0x02
#define ZAPI_MESSAGE_DISTANCE 0x04
#define ZAPI_MESSAGE_METRIC   0x08
#define ZAPI_MESSAGE_MTU      0x10
#define ZAPI_MESSAGE_TAG      0x20


/* Zebra IPv4 route message API. */
struct zapi_ipv4
{
  u_char type;
  u_char flags;
  u_char message;
  safi_t safi; -- typedef u_int8_t safi_t;
  u_char nexthop_num;
  struct in_addr **nexthop;
  u_char ifindex_num;
  ifindex_t *ifindex;
  u_char distance;
  route_tag_t tag;
  u_int32_t metric;
  u_int32_t mtu;
  vrf_id_t vrf_id;
};
-}

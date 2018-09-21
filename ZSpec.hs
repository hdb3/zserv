module ZSpec where
import Data.Word
default(Int)

_AF_INET6 = 10 :: Word8
_AF_INET  =  2 :: Word8

_ZEBRA_INTERFACE_ADD = 1 :: Word16
-- _ZEBRA_INTERFACE_DELETE = 2 :: Word16
_ZEBRA_INTERFACE_ADDRESS_ADD = 3 :: Word16
-- _ZEBRA_INTERFACE_ADDRESS_DELETE = 4 :: Word16
-- _ZEBRA_INTERFACE_UP = 5 :: Word16
-- _ZEBRA_INTERFACE_DOWN = 6 :: Word16
_ZEBRA_IPV4_ROUTE_ADD = 7 :: Word16
_ZEBRA_IPV4_ROUTE_DELETE = 8 :: Word16
-- _ZEBRA_IPV6_ROUTE_ADD = 9 :: Word16
-- _ZEBRA_IPV6_ROUTE_DELETE = 10 :: Word16
-- _ZEBRA_REDISTRIBUTE_ADD = 11 :: Word16
-- _ZEBRA_REDISTRIBUTE_DELETE = 12 :: Word16
-- _ZEBRA_REDISTRIBUTE_DEFAULT_ADD = 13 :: Word16
-- _ZEBRA_REDISTRIBUTE_DEFAULT_DELETE = 14 :: Word16
-- _ZEBRA_IPV4_NEXTHOP_LOOKUP = 15 :: Word16
-- _ZEBRA_IPV6_NEXTHOP_LOOKUP = 16 :: Word16
-- _ZEBRA_IPV4_IMPORT_LOOKUP = 17 :: Word16
-- _ZEBRA_IPV6_IMPORT_LOOKUP = 18 :: Word16
-- _ZEBRA_INTERFACE_RENAME = 19 :: Word16
_ZEBRA_ROUTER_ID_ADD = 20 :: Word16
-- _ZEBRA_ROUTER_ID_DELETE = 21 :: Word16
_ZEBRA_ROUTER_ID_UPDATE = 22 :: Word16
_ZEBRA_HELLO = 23 :: Word16
-- _ZEBRA_IPV4_NEXTHOP_LOOKUP_MRIB = 24 :: Word16
-- _ZEBRA_VRF_UNREGISTER = 25 :: Word16
-- _ZEBRA_INTERFACE_LINK_PARAMS = 26 :: Word16
_ZEBRA_NEXTHOP_REGISTER = 27 :: Word16
_ZEBRA_NEXTHOP_UNREGISTER = 28 :: Word16
_ZEBRA_NEXTHOP_UPDATE = 29 :: Word16
-- _ZEBRA_MESSAGE_MAX = 30 :: Word16

--zKnownCommands = [_ZEBRA_INTERFACE_ADD,_ZEBRA_IPV4_ROUTE_DELETE,_ZEBRA_ROUTER_ID_UPDATE,_ZEBRA_HELLO,_ZEBRA_NEXTHOP_REGISTER,_ZEBRA_NEXTHOP_UNREGISTER]
_ZAPI_MESSAGE_NEXTHOP  = 0
_ZAPI_MESSAGE_IFINDEX  = 1
_ZAPI_MESSAGE_DISTANCE = 2
_ZAPI_MESSAGE_METRIC   = 3
_ZAPI_MESSAGE_MTU      = 4
_ZAPI_MESSAGE_TAG      = 5

_ZEBRA_NEXTHOP_IFINDEX      = 1 :: Word8
_ZEBRA_NEXTHOP_IFNAME       = 2 :: Word8
_ZEBRA_NEXTHOP_IPV4         = 3 :: Word8
_ZEBRA_NEXTHOP_IPV4_IFINDEX = 4 :: Word8
_ZEBRA_NEXTHOP_IPV4_IFNAME  = 5 :: Word8
_ZEBRA_NEXTHOP_IPV6         = 6 :: Word8
_ZEBRA_NEXTHOP_IPV6_IFINDEX = 7 :: Word8
_ZEBRA_NEXTHOP_IPV6_IFNAME  = 8 :: Word8
_ZEBRA_NEXTHOP_BLACKHOLE    = 9 :: Word8

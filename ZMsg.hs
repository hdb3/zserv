module ZMsg where
import Data.ByteString
import Data.Word
import Data.IP

data ZMsg = ZHello
            | ZInterfaceAdd ZInterface
            | ZInterfaceAddressAdd { payload :: ByteString }
            | ZRouterIDUpdate ZPrefix
            | ZIPV4RouteDelete { payload :: ByteString }
            | ZNexthopUnregister { payload :: ByteString }
            | ZUnknown { cmd :: Word16 , payload :: ByteString }
    deriving (Eq,Show,Read)

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

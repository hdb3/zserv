module ZMsg where
import Data.ByteString
import Data.Word

data ZMsg = ZHello
            | ZInterfaceAdd { payload :: ByteString }
            | ZInterfaceAddressAdd { payload :: ByteString }
            | ZRouterIDUpdate { payload :: ByteString }
            | ZIPV4RouteDelete { payload :: ByteString }
            | ZNexthopUnregister { payload :: ByteString }
            | ZUnknown { cmd :: Word16 , payload :: ByteString }

    deriving (Eq,Show,Read)

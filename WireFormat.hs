{-# LANGUAGE MultiWayIf #-}
module WireFormat where
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString -- from package attoparsec
import Data.Attoparsec.Binary -- from package attoparsec-binary
import Control.Applicative
import Control.Monad(when,unless,liftM)
import ZMsg
import ZSpec

type RawZMessage = BS.ByteString

-- zMessageParser :: Parser (Maybe RawZMessage)
zMessageParser :: Parser (Maybe ZMsg)
zMessageParser = ( zMessageParser' <|> return Nothing ) <?> "zserv wire format parser"
zMessageParser' = do
    msgLen <- anyWord16be
    word8 0xff -- 'marker'
    word8 0x03 -- version - earlier than three has incompatible format - later is unknown
    word16be 0x0000 -- VRF ID - for now we would not want to know about multiple VRFs - the default value is zero
    when (msgLen > 4096 || msgLen < 8) ( fail "invalid message length")
    -- msg <- Data.Attoparsec.ByteString.take $ fromIntegral (msgLen - 6) 
    -- return $ Just msg
    zParser $ fromIntegral (msgLen - 6)

zParser :: Int -> Parser (Maybe ZMsg)
zParser n = do
   cmd <- anyWord16be
   if | cmd == _ZEBRA_HELLO -> return $ Just ZHello
      | cmd == _ZEBRA_INTERFACE_ADD ->
          do pl <- Data.Attoparsec.ByteString.take (n-2)
             return $ Just $ ZInterfaceAdd pl
      | cmd == _ZEBRA_INTERFACE_ADDRESS_ADD ->
          do pl <- Data.Attoparsec.ByteString.take (n-2)
             return $ Just $ ZInterfaceAddressAdd pl
      | cmd == _ZEBRA_ROUTER_ID_UPDATE ->
          do pl <- Data.Attoparsec.ByteString.take (n-2)
             return $ Just $ ZRouterIDUpdate pl
      | cmd == _ZEBRA_IPV4_ROUTE_DELETE ->
          do pl <- Data.Attoparsec.ByteString.take (n-2)
             return $ Just $ ZIPV4RouteDelete pl
      | cmd == _ZEBRA_NEXTHOP_UNREGISTER ->
          do pl <- Data.Attoparsec.ByteString.take (n-2)
             return $ Just $ ZNexthopUnregister pl
      | otherwise -> do
            payload <- Data.Attoparsec.ByteString.take (n-2)
            return $ Just $ ZUnknown cmd payload

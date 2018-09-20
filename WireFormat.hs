{-# LANGUAGE RecordWildCards,MultiWayIf #-}
module WireFormat where
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString -- from package attoparsec
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.Binary -- from package attoparsec-binary
import Control.Applicative
import Control.Monad(when,unless,liftM)
import Control.Exception
import ZMsg
import ZSpec

type RawZMessage = BS.ByteString

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
zParser n = do
   cmd <- anyWord16be
   if | cmd == _ZEBRA_HELLO -> return ZHello

      | cmd == _ZEBRA_INTERFACE_ADD ->
          do interface <- zInterfaceParser (n-2)
             return $ ZInterfaceAdd interface

      | cmd == _ZEBRA_INTERFACE_ADDRESS_ADD ->
          do pl <- DAB.take (n-2)
             return $ ZInterfaceAddressAdd pl

      | cmd == _ZEBRA_ROUTER_ID_UPDATE ->
          do pl <- DAB.take (n-2)
             return $ ZRouterIDUpdate pl

      | cmd == _ZEBRA_IPV4_ROUTE_DELETE ->
          do pl <- DAB.take (n-2)
             return $ ZIPV4RouteDelete pl

      | cmd == _ZEBRA_NEXTHOP_UNREGISTER ->
          do pl <- DAB.take (n-2)
             return $ ZNexthopUnregister pl

      | otherwise -> do
            payload <- DAB.take (n-2)
            return $ ZUnknown cmd payload


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
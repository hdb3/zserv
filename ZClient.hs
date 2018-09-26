{-#LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where
import System.Environment
import Network.Socket
import System.IO
import Data.IP
import Control.Monad(unless)
import Control.Concurrent
import qualified System.IO.Streams as Streams
import System.IO.Streams.Attoparsec.ByteString
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString
import Text.Read

import ZServ

main :: IO ()
main = do
    args <- getArgs
    let s = args !! 0
        (address,family) = maybe (SockAddrUnix s, AF_UNIX)
                           ( \target -> ( SockAddrInet 2600 (toHostAddress target),AF_INET))
                           ( readMaybe s :: Maybe IPv4)
    putStrLn $ "connecting to: " ++ show address
    sock <- socket family Stream defaultProtocol
    connect sock address
    putStrLn "connected"
    handle <- socketToHandle sock ReadWriteMode
    inputStream <- Streams.handleToInputStream handle
    zStream <- parserToInputStream zMessageParser inputStream
    outputStream <- Streams.makeOutputStream $ \m -> case m of
            Just zmsg -> L.hPut handle $ encode (ZMsgRaw 0 zmsg)
            Nothing -> return () -- could close the handle/socket?
    let put = flip Streams.write outputStream . Just
    put (ZMHello 9)
    put ZMQRouterIdAdd
    put ZMQInterfaceAdd
    -- forkIO (console put)
    -- loop zStream where
    forkIO (loop zStream)
    console put
    where
    loop stream = do
        msg <- Streams.read stream
        maybe (putStrLn "end of messages")
              ( \zMsg -> do 
                              print zMsg
                              loop stream )
              msg

console put = do
    prompt
    input <- getLine
    let (command,p1,p2) = parseInput input
    case command of
        'a' -> maybe (do putStrLn "couldn't parse a route")
                     (\route -> maybe ( putStrLn "couldn't parse a next-hop")
                                      ( addRoute put route )
                                      (parseAddress p2))
                     (parsePrefix p1)

        'd' -> maybe (do putStrLn "couldn't parse a route")
                     (delRoute put)
                     (parsePrefix p1)
        'q' -> putStrLn "goodbye"
        'z' -> putStrLn "say hello, wave goodbye"
        otherwise -> (do putStrLn "couldn't parse a command")

    unless (command == 'q') (console put)
    where prompt = hPutStr stdout ">>> " >> hFlush stdout
          parseInput = parseInput' . words
          parseInput' wx | null wx = (' ',"","")
                         | otherwise  = (head (wx !! 0) , wx !! 1, wx !! 2)
          parsePrefix s = readMaybe s :: Maybe (AddrRange IPv4)
          -- parsePrefix s = readMaybe s :: Maybe IPRange
          parseAddress s  = readMaybe s :: Maybe IPv4
          addRoute put pfx nh = let route = routeBase { zrPrefix = fromIPv4Range pfx, zrNextHops = [ZNHIPv4 nh] }
              in do putStrLn $ "add " ++ show ( ZMIPV4RouteAdd route) 
                    put $ ZMIPV4RouteAdd route 
          delRoute put pfx = let route = routeBase { zrPrefix = fromIPv4Range pfx }
              in do putStrLn $ "del " ++ show ( ZMIPV4RouteDelete route) 
                    put $ ZMIPV4RouteDelete route 

          -- fromIPv4Range  ZPrefixV4{..} = makeAddrRange v4address (fromIntegral plen) 
          fromIPv4Range ipv4range = let (v4address, plen') = addrRangePair ipv4range 
                                        plen = fromIntegral plen' in ZPrefixV4{..}

          routeBase = ZRoute { zrType = 9
                             , zrFlags = 9
                             , zrSafi = 1
                             , zrPrefix = undefined
                             , zrNextHops = []
                             , zrDistance = Nothing
                             , zrMetric = Nothing
                             , zrMtu = Nothing
                             , zrTag = Nothing
                             }

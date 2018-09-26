{-#LANGUAGE OverloadedStrings #-}
module Main where
import System.Environment
import Network.Socket
import System.IO
import Data.IP
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
        (address,family) = maybe ((SockAddrUnix s, AF_UNIX))
                           ( \target -> ( SockAddrInet 2600 (toHostAddress $ target),AF_INET))
                           ( readMaybe s :: Maybe IPv4)
   
    print address

    putStrLn $ "connecting to: " ++ (show address)
    sock <- socket family Stream defaultProtocol
    connect sock address
    putStrLn "connected"
    handle <- socketToHandle sock ReadWriteMode
    inputStream <- Streams.handleToInputStream handle
    zStream <- parserToInputStream zMessageParser inputStream
    let enc zmsg = encode (ZMsgRaw 0 zmsg)
    L.hPut handle (enc (ZMHello 9))
    L.hPut handle (enc ZMQRouterIdAdd)
    L.hPut handle (enc ZMQInterfaceAdd)
    loop zStream where
    loop stream = do
        msg <- Streams.read stream
        maybe (putStrLn "end of messages")
              ( \zMsg -> do 
                              print zMsg
                              loop stream )
              msg

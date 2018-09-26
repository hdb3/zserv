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
import Control.Concurrent
import Control.Monad (forever)

import ZServ

main :: IO ()
main = do
    args <- getArgs
    let s = args !! 0

    let listenAddress = SockAddrInet 2600 0
    listenSocket <- socket AF_INET Stream defaultProtocol 
    setSocketOption listenSocket ReuseAddr 1
    bind listenSocket listenAddress
    listen listenSocket 100
    forever $ do
        (sock, peer) <- accept listenSocket
        putStrLn $ "Client connection from " ++ show peer
        handle <- socketToHandle sock ReadWriteMode
        forkIO $ proxy handle s


getStreams handle = do
    inputStream <- Streams.handleToInputStream handle
    inStream <- parserToInputStream zMessageParser inputStream
    outStream <- Streams.makeOutputStream $ \m -> case m of
            Just zmsg -> L.hPut handle $ encode (ZMsgRaw 0 zmsg)
            Nothing -> return () -- could close the handle/socket?
    return (inStream, outStream)

proxy clientHandle server = do
    putStrLn $ "connecting to: " ++ server
    sock <- socket AF_UNIX Stream defaultProtocol
    connect sock ( SockAddrUnix server)
    serverHandle <- socketToHandle sock ReadWriteMode
    putStrLn "server connected"

    ( serverInput , serverOutput ) <- getStreams serverHandle
    ( clientInput , clientOutput ) <- getStreams clientHandle

    forkIO $ loop "Client: " (clientInput,serverOutput)
    loop "Server: "(serverInput,clientOutput)
    where
    loop str (inStr,outStr) = do
        msg <- Streams.read inStr
        Streams.write msg outStr
        maybe (do putStrLn $ str ++ "end of messages")
              ( \zMsg -> do 
                              putStrLn $ str ++ show zMsg
                              loop str (inStr,outStr) )
              msg

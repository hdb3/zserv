{-# LANGUAGE RecordWildCards,MultiWayIf,OverloadedStrings #-}
module Main where
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
--import Data.Attoparsec.ByteString -- from package attoparsec
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.Binary -- from package attoparsec-binary
import Control.Applicative
import Control.Monad(when,unless,liftM)
import Control.Exception
import Data.IP
import Data.Bits
import Data.Word
import Data.Char
import Data.Binary
--import Data.Either

import Debug
import ZMsg
import ZMsgBinary
import ZSpec
import WireFormat

parse'' p bs = (toHex bs,DAB.parseOnly (p n) bs) where
    n  = BS.length bs

parse' p s = (C8.unpack s,DAB.parseOnly (p n) bs) where
    bs = fromHex s
    n  = BS.length bs

-- zParse needs the length of the supplied byteString, zParse' wraps this away
zParser' :: BS.ByteString -> Either String ZMsg
zParser' bs = DAB.parseOnly (zParser (BS.length bs)) bs 

print' = putStrLn . show'
--print' (s,res) = putStrLn $ show'
show' :: (String, Either String ZMsg) -> String
show' (s,Right res) = s ++ " success " ++ show res
--show' (s,Right res) = show res
show' (s,Left res) = s ++ " fail " ++ show res

main = do
    putChar (chr 27)
    putChar 'c'
    putStrLn "\n\n\n"
    --print' $ parse' zParser "001709"
    verify "001709"           -- ZMHello
    verify "0014"           -- ZMRouterIdAdd
    verify "0001"           -- ZMQInterfaceAdd
    verify "001602c0a87a1d20" -- ZMRouterIDUpdate
    -- parseFlowFile "flow5"

    -- TODO ask why this requires relaex test in zStartUpdateParse - must be some parse error....
    -- print' $ parse' zParser "001c00000220c0a87a01"

    --print $ parse' zPrefixIPv4Parser "02c0a87a1d20"
    --print $ parse' zParser "001602c0a87a1d20"
    --print $ parse' zParser "00080909080001100b0500000000"
    --print $ parse' zParser "00080909080001180c000200000000"
    --print $ parse' zParser "00080909080001180c000100000000"
    --parseFlowFile "flow1"
    --parseFlowFile "flow2"
    --parseFlowFile "flow3"
    --parseFlowFile "flow4"
    --parseFlowFile "flow5"

verify :: BS.ByteString -> IO ()
verify c8 = do
    let bs = fromHex c8
        zmsg = zParser' bs
    either (\errString -> putStrLn $ "decode failed for string " ++ toHex bs ++ " (" ++ errString ++ ")")
           (\rightZMsg -> do let bs' = encode' rightZMsg
                             if bs == bs' then putStrLn $ "OK (" ++ toHex bs ++ " = " ++ show rightZMsg ++ ")"
                             else putStrLn $ "decode failed for string " ++ toHex bs ++ " /= " ++ toHex bs'  ++ " (" ++ show rightZMsg ++ ")")
           zmsg
    where encode' = L.toStrict . encode

parseFlowFile path = do
    flow <- BS.readFile path
    let zmsgs = DAB.parseOnly zFlowParser flow
    let raw = DAB.parseOnly zRawFlowParser flow
    -- let zmsgs = parseOnly (zFlowParser <* endOfInput) flow
    putStrLn $ "\nraw  " ++ path
    either putStrLn
           ( mapM_ ( \(cmd,pl) -> putStrLn $ "cmd " ++ show cmd ++ " len " ++ show (BS.length pl-2) ++ " : " ++ (toHex pl) ))
           raw
    let parsed = either Left  (Right . map eparse) raw
        eparse = parse'' zParser . snd

    putStrLn $ "\neparsed"
    putStrLn $ either show
                      (unlines . map show')
                      -- ( unlines . map (\(bs,res) -> toHex bs ++ " / " ++ show res ))
                      parsed
{-
    putStrLn $ "\nparsed"
    putStrLn $ either show
                      ( unlines . map show )
                      zmsgs
    putStrLn ""
-}

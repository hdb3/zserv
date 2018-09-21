{-# LANGUAGE RecordWildCards,MultiWayIf,OverloadedStrings #-}
module Main where
import qualified Data.ByteString as BS
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
--import Data.Either

import Debug
import ZMsg
import ZSpec
import WireFormat

k=99
parse'' p bs = (toHex bs,DAB.parseOnly (p n) bs) where
    n  = BS.length bs

parse' p s = (C8.unpack s,DAB.parseOnly (p n) bs) where
    bs = fromHex s
    n  = BS.length bs

print' = putStrLn . show'
--print' (s,res) = putStrLn $ show'
show' (s,res) = s ++ " " ++ show res

main = do
    putChar (chr 27)
    putChar 'c'
    putStrLn "\n\n\n"
    print' $ parse' zParser "001d000220c0a87a010000000000"
    --print' $ parse' zParser "000165746830000000000000000000000000000000000000000205000000000001104300000000000005dc000005dc0000000000000001000000065254008d177f00"
    -- parseFlowFile "flow4"



    --print $ parse' zRouteParser "0909080001080a00000000"
    --print $ parse' zPrefixIPv4Parser "02c0a87a1d20"
    --print $ parse' zParser "001602c0a87a1d20"
    --print $ parse' zParser "00080909080001100b0500000000"
    --print $ parse' zParser "00080909080001180c000200000000"
    --print $ parse' zParser "00080909080001180c000100000000"
    -- parseFlowFile "flow4"
    --parseFlowFile "flow2"
    -- parseFlowFile "flow3"
    -- parseFlowFile "flow4"
    -- parseFlowFile "flow5"

parseFlowFile path = do
    flow <- BS.readFile path
    let zmsgs' = DAB.parseOnly zFlowParser flow
        zmsgs = fmap (take k) zmsgs'
    let raw' = DAB.parseOnly zRawFlowParser flow
        raw = fmap (take k) raw'
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

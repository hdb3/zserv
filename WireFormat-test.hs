{-# LANGUAGE RecordWildCards,MultiWayIf,OverloadedStrings #-}
module Main where
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString -- from package attoparsec
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.Binary -- from package attoparsec-binary
import Control.Applicative
import Control.Monad(when,unless,liftM)
import Control.Exception
import Data.IP
import Data.Bits
import Data.Word
--import Data.Either

import Debug
import ZMsg
import ZSpec
import WireFormat

parse' p s = (s,parseOnly (p n) bs) where
    bs = fromHex s
    n  = BS.length bs

main = do
    --print $ parse' zRouteParser "0909080001080a00000000"
    --print $ parse' zPrefixIPv4Parser "02c0a87a1d20"
    --print $ parse' zParser "001602c0a87a1d20"
    print $ parse' zParser "00080909080001100b0500000000"
    print $ parse' zParser "00080909080001180c000200000000"
    print $ parse' zParser "00080909080001180c000100000000"
    parseFlowFile "flow3"
    --parseFlowFile "flow2"
    -- parseFlowFile "flow3"
    -- parseFlowFile "flow4"
    -- parseFlowFile "flow5"

parseFlowFile path = do
    flow <- BS.readFile path
    let zmsgs = parseOnly zFlowParser flow
    let raw = parseOnly zRawFlowParser flow
    -- let zmsgs = parseOnly (zFlowParser <* endOfInput) flow
    putStrLn $ "\nraw  " ++ path
    either putStrLn
           ( mapM_ ( \(cmd,pl) -> putStrLn $ "cmd " ++ show cmd ++ " len " ++ show (BS.length pl) ++ " : " ++ (toHex pl) ))
           raw
{-
    let parsed = either Left  (Right . map eparse) raw
        eparse = parse' zParser . snd

    putStrLn $ "\neparsed"
    putStrLn $ either show
                      ( unlines . map show )
                      parsed
-}
    putStrLn $ "\nparsed"
    putStrLn $ either show
                      ( unlines . map show )
                      zmsgs
    putStrLn ""

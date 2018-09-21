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

import Debug
import ZMsg
import ZSpec
import WireFormat
{-
zMessageParser :: Parser (Maybe ZMsg)
zMessageParser' :: Parser ZMsg
zParser :: Int -> Parser ZMsg
zNextHopParser :: Parser ZNextHop
zRouteParser :: Int -> Parser ZRoute
zInterfaceParser :: Int -> Parser ZInterface
zvPrefixIPv4Parser :: Parser ZPrefix
zPrefixIPv4Parser :: Int -> Parser ZPrefix
-}

parse' p s = parse (p n) bs where
    bs = fromHex s
    n  = BS.length bs
main = print $ parse' zRouteParser "0909080001080a00000000"

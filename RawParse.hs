module Main where
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString -- from package attoparsec
import System.IO(stdin)

import Debug
import WireFormat

main = do
    file <- BS.hGetContents stdin
    let zmsgs = parseOnly zRawFlowParser file
    either putStrLn
           ( mapM_ ( \(cmd,pl) -> putStrLn $ "cmd " ++ show cmd ++ " len " ++ show (BS.length pl) ++ " : " ++ (toHex pl) ))
           zmsgs

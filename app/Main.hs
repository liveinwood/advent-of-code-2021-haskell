
{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Data.ByteString.Char8 as BS
import           Day16

main :: IO ()
main = do
    input <- BS.getLine
    let bits = hexToBits input
    let pkts = case runParser packetParser bits of
                        Just (pkts, _) -> pkts
                        Nothing        -> error "error"
    print pkts
    print $ eval pkts

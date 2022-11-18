{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M

solve :: Int -> M.Map BS.ByteString BS.ByteString -> BS.ByteString -> Int
solve n rule s = let ints = map BS.length . BS.group . BS.sort $ pairInsertionN n rule s
                 in (maximum ints - minimum ints)

chop :: BS.ByteString -> [BS.ByteString] -> [BS.ByteString]
chop ss acc = if BS.length ss <= 2 then reverse (ss:acc)
              else chop (BS.drop 1 ss) $ BS.take 2 ss:acc

insertion :: M.Map BS.ByteString BS.ByteString -> [BS.ByteString] -> [BS.ByteString]
insertion rule = map insert
    where
        insert :: BS.ByteString -> BS.ByteString
        insert s = case M.lookup s rule of
                        Just c  -> BS.concat [BS.init s, c, BS.tail s]
                        Nothing -> s

pairConcat :: [BS.ByteString] -> BS.ByteString
pairConcat = foldl (\acc s -> concat' acc s) ""
    where
        concat' :: BS.ByteString -> BS.ByteString -> BS.ByteString
        concat' "" s' = s'
        concat' s s'  = BS.concat [BS.init s, s']

pairInsertion :: M.Map BS.ByteString BS.ByteString -> BS.ByteString -> BS.ByteString
pairInsertion rule s = pairConcat . insertion rule $ chop s []

pairInsertionN :: Int -> M.Map BS.ByteString BS.ByteString -> BS.ByteString -> BS.ByteString
pairInsertionN 0 rule s = s
pairInsertionN n rule s = pairInsertionN (n - 1) rule (pairInsertion rule s)

rule :: M.Map BS.ByteString BS.ByteString
rule = M.fromList [("HN","S"),("FK","N"),("CH","P"),("VP","P"),("VV","C"),("PB","H"),("CP","F"),("KO","P"),("KN","V"),("NO","K"),("NF","N"),("CO","P"),("HO","H"),("VH","V"),("OV","C"),("VS","F"),("PK","H"),("OS","S"),("BF","S"),("SN","P"),("NK","N"),("SV","O"),("KB","O"),("ON","O"),("FN","H"),("FO","N"),("KV","S"),("CS","C"),("VO","O"),("SP","O"),("VK","H"),("KP","S"),("SK","N"),("NC","B"),("PN","N"),("HV","O"),("HS","C"),("CN","N"),("OO","V"),("FF","B"),("VC","V"),("HK","K"),("CC","H"),("BO","H"),("SC","O"),("HH","C"),("BV","P"),("OB","O"),("FC","H"),("PO","C"),("FV","C"),("BK","F"),("HB","B"),("NH","P"),("KF","N"),("BP","H"),("KK","O"),("OH","K"),("CB","H"),("CK","C"),("OK","H"),("NN","F"),("VF","N"),("SO","K"),("OP","F"),("NP","B"),("FS","S"),("SH","O"),("FP","O"),("SF","V"),("HF","N"),("KC","K"),("SB","V"),("FH","N"),("SS","C"),("BB","C"),("NV","K"),("OC","S"),("CV","N"),("HC","P"),("BC","N"),("OF","K"),("BH","N"),("NS","K"),("BN","F"),("PC","C"),("CF","N"),("HP","F"),("BS","O"),("PF","S"),("PV","B"),("KH","K"),("VN","V"),("NB","N"),("PH","V"),("KS","B"),("PP","V"),("PS","C"),("VB","N"),("FB","N")]

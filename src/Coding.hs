module Coding where

xor :: Bool -> Bool -> Bool
xor p q = (p && not q) || (not p && q)

xorArray :: [(Bool, Bool)] -> [Bool]
xorArray = map (uncurry xor)

grayCoding :: [Bool] -> [Bool]
grayCoding []           = []
grayCoding [b         ] = [b]
grayCoding (b : c : bs) = b `xor` c : grayCoding (c : bs)

grayDecoding :: [Bool] -> [Bool]
grayDecoding = foldr go []
 where
  go c []         = [c]
  go c bs@(b : _) = b `xor` c : bs

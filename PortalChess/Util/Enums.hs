module PortalChess.Util.Enums where

numOf :: (Bounded b, Enum b) => b -> Int
numOf b = succ (maxElem - minElem) 
  where 
    maxElem = fromEnum $ maxBound `asTypeOf` b
    minElem = fromEnum $ minBound `asTypeOf` b

fromEnumProd :: (Enum e, Enum b, Bounded b) => e -> b -> Int
fromEnumProd e b = fromEnum e * numOf b + fromEnum b

toEnumProd :: (Enum e, Enum b, Bounded b) => (e -> b -> p) -> Int -> p 
toEnumProd cons i = cons eVal bVal
  where
    bVal = toEnum m
    eVal = toEnum d
    (d,m) = i `quotRem` numOf bVal

minBoundProd :: (Enum a) => a
minBoundProd = toEnum 0

maxBoundProd :: (Enum a, Enum b, Bounded a, Bounded b, Enum c) => a -> b -> c
maxBoundProd a b = toEnum . pred $ numOf a * numOf b

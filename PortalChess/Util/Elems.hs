module PortalChess.Util.Elems where

import Data.Word
import qualified Data.Map as Map
import Control.Applicative

class Elems a where
  elems :: [a]

instance Elems () where
  elems = empty

instance (Elems a, Elems b) => Elems (a,b) where
  elems = (,) <$> elems <*> elems

instance (Elems a) => Elems (Maybe a) where
  elems = pure Nothing <|> Just <$> elems

instance Elems Word8 where
  elems = elemsFrom 0
    where elemsFrom n
            | n == maxBound = [n]
            | otherwise = n : elemsFrom (n+1)

elemsToEnum :: (Elems a, Ord a) => Int -> a
elemsToEnum = (Map.!) (Map.fromList (zip [0..] elems))

elemsFromEnum :: (Elems a, Ord a) => a -> Int
elemsFromEnum = (Map.!) (Map.fromList (zip elems [0..]))


instance Elems Player where
  elems = [Black, White]
  
instance Elems Direction where
  elems = [North, Northeast, East, Southeast, South, Southwest, West, Northwest]

instance Elems LoopKind where
  elems = [Unidrectional Bidirectional]

instance Elems PieceInfo where
  elems = [Pawn] ++ Arrow <$> elems ++ Portal <$> elems ++ [Prism, DCannon, CCannon, King] 

instance Elems Loop where
  elems = Loop <$> elems <*> elems

instance Elems Piece where
  elems = Piece <$> elems <*> elems

instance Elems Square where
  elems = Square <$> elems <*> elems

instance Enums Square where
  toEnum = elemsToEnum
  fromEnum = elemsFromEnum

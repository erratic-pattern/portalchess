{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PortalChess.Types where
import PortalChess.Util.Enums
import Control.Monad.Logic
import Control.Monad.RWS
import Control.Monad.Identity
import Data.Map (Map)
import Data.Set (Set)
import Data.DList (DList)
import Data.Word
import qualified Data.Vector.Storable as V
import Foreign.Storable
import Foreign.Ptr

data Player = 
  Black | White
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Rank = 
  R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data File = 
  A | B | C | D | E | F | G | H
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

type Coord = (Rank, File)

data Direction = 
  South | North | Southwest | Northeast | West | East | Northwest | Southeast
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
diagonals = [Northeast, Southwest, Southeast, Northwest]
cardinals = [North, East, South, West]
           
data Move = Move {mCoord :: Coord 
                 ,mDir   :: Direction
                 }
            deriving (Show, Read, Eq, Ord)
cardinalMovesOf c = map (Move c) $ cardinals
diagonalMovesOf c = map (Move c) $ diagonals

data Piece = Piece {piecePlayer :: Player 
                   ,pieceInfo   :: PieceInfo
                   }
           deriving (Show, Read, Eq, Ord)

data PieceInfo =
    Pawn 
  | Arrow Direction
  | Lens Direction
  | Portal Direction (Maybe Coord)
  | DCannon
  | CCannon
  | King
  deriving (Show, Read, Eq, Ord)

type Square = Maybe Piece

newtype StorableSquare = StorableSquare {getSquare :: Square} 
                       deriving (Show, Read, Eq, Ord, Enum, Bounded)

type Board = V.Vector StorableSquare


data LoopKind = 
  Directed | Bidirected
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Loop = Loop {lOwner :: Player 
                 ,lSet   :: Set Move
                 ,lKind  :: LoopKind
                 }
          deriving (Show, Read, Eq, Ord)

data Game = Game {gBoard   :: Board
                 ,gLoops   :: [Loop]
                 ,gTurn    :: Player
                 }
          deriving (Show, Read, Eq, Ord)

data WalkInfo = WalkInfo {wBoard  :: Board
                         ,wSquare :: Square
                         ,wMove   :: Move
                         }

type WalkT m = LogicT (RWST WalkInfo (DList Char) (Set Move) m)

type Walk = WalkT Identity

instance (Enum a) => Enum (Maybe a) where  
  fromEnum Nothing  = 0
  fromEnum (Just e) = 1 + fromEnum e
  toEnum 0          = Nothing
  toEnum n          = Just (toEnum (n-1))

instance (Bounded a) => Bounded (Maybe a) where
  minBound = Nothing
  maxBound = Just (maxBound)

instance (Enum a, Enum b, Bounded b) => Enum (a,b) where
  fromEnum (a,b) = fromEnumProd a b
  toEnum i       = toEnumProd (,) i

instance Enum PieceInfo where
  fromEnum e = case e of
    Pawn        -> 0
    Arrow d     -> 1 + fromEnum d
    Lens d      -> 9 + fromEnum d
    Portal d c  -> 17 + fromEnumProd d c
    DCannon     -> 537
    CCannon     -> 538
    King        -> 539
    
  toEnum n = case n of
    0   -> Pawn
    537 -> DCannon
    538 -> CCannon
    539 -> King
    _  -> case d of
      0 -> Arrow direction
      1 -> Lens direction
      _ -> toEnumProd Portal (n - 17) 
      where
        direction = toEnum m
        (d, m) = (n-1) `quotRem` numOf direction

instance Bounded PieceInfo where
  minBound = Pawn
  maxBound = King

{-  No longer valid. Loop takes three fields
instance Enum Loop where
  fromEnum (Loop d k) = fromEnumProd d k
  toEnum i            = toEnumProd Loop i
-}

instance Enum Piece where
  fromEnum (Piece p i) = fromEnumProd p i
  toEnum i             = toEnumProd Piece i

instance Bounded Piece where
  minBound = minBoundProd
  maxBound = maxBoundProd (undefined :: Player) (undefined :: PieceInfo) 

--This is the integral type we want to use for storing
--square cells in the Vector
type SquareRep = Word16

instance Storable StorableSquare where
  sizeOf    _ = sizeOf (undefined :: SquareRep)
  alignment _ = alignment (undefined :: SquareRep) 
  peek = fmap convert . peek . castPtr
    where
      convert = toEnum . fromIntegral :: SquareRep -> StorableSquare
  poke p = poke (castPtr p) . convert
    where
      convert = fromIntegral . fromEnum :: StorableSquare -> SquareRep

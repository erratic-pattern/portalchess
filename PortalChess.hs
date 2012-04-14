{-# LANGUAGE FlexibleContexts, NamedFieldPuns#-}
module PortalChess where
import PortalChess.Types
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Logic
import Control.Applicative
import Control.Arrow
import qualified Data.Vector.Storable as V
import qualified Data.Set as S
import qualified Data.DList as DL
import Data.List
import Data.Maybe
import Data.Function


infix  9 `at`

at :: Board -> Coord -> Square
at b c = square
  where
    StorableSquare square = b V.! fromEnum c

imap :: (Coord -> Square -> Square) -> Board -> Board
imap boardF = V.imap vecF
  where
    vecF ind (StorableSquare square) = StorableSquare $ boardF (toEnum ind) square

emptyBoard :: Board
emptyBoard = V.generate 64 (const (StorableSquare Nothing))

newBoard :: Board
newBoard = imap fillBoard emptyBoard
  where
    fillBoard (rank, file) square =
      case rank of
        R1 -> fillPieceRank White
        R2 -> fillPawnRank White
        R8 -> fillPieceRank Black
        R7 -> fillPawnRank Black
        _  -> Nothing
      where
        fillPawnRank p = Just (Piece p Pawn)

        fillPieceRank player = Just (Piece player pieceType)
          where
            pieceType = case file of
              E -> King
              A -> CCannon
              H -> DCannon
              C -> Portal d $ Just (rank, F)
              F -> Portal d $ Just (rank, C)
              D -> Lens d
              _ -> Arrow d
            d = startDirFor player

moveSetAt :: Board -> Coord -> [Coord]
moveSetAt board c = map mCoord . fst . runWalk board c . moveSet c . at board $ c

localMove :: (MonadReader WalkInfo m) => m a -> Move -> m a
localMove m move = local (\info -> info {wMove = move} ) m


moveSet :: (MonadLogic m, MonadState (S.Set Move) m, MonadReader WalkInfo m) =>
           Coord -> Square -> m Move
moveSet _ Nothing = mzero
moveSet c (Just (Piece player piece)) = case piece of
  Pawn ->
    forwardStep `mplus` diagCapture
      where
        forwardStep = linearStep . Move c . startDirFor $ player

        diagCapture = (mplus `on` step captureRule . Move c) diag1 diag2
        (diag1, diag2) = rotateCW &&& rotateCCW $ startDirFor player
        captureRule = ifBlocked `mplus` followPortal >>= localMove ifBlocked

  Arrow _ ->
    noMove `mplus` walkAll diagMoves `mplus`  stepAll cardMoves
  Lens  _ ->
    noMove `mplus` walkAll (cardMoves `mplus` diagMoves)
  Portal d _ ->
    noMove `mplus` stepWith (chebyshevStep 3) undefined (Move c undefined)
  cannon_or_king ->
    stepAll (cardMoves `mplus` diagMoves)
  where
    noMove    = return $ Move c North
    cardMoves = cardinalMovesOf c
    diagMoves = diagonalMovesOf c


runWalk :: Board -> Coord -> Walk Move -> ([Move], DL.DList Char)
runWalk b c m = evalRWS (observeAllT m) info  S.empty
  where
    info = WalkInfo {wBoard = b,  wSquare = b `at` c,  wMove = Move c undefined}

walkAll :: (MonadState (S.Set Move) m, MonadLogic m, MonadReader WalkInfo m)
           => [Move] -> m Move
walkAll = msum . map linearWalk

linearWalk :: (MonadState (S.Set Move) m, MonadLogic m,
               MonadReader WalkInfo m) =>
              Move -> m Move
linearWalk = walk linearWalker

walk :: (MonadPlus m, MonadReader WalkInfo m) => m Move -> Move -> m Move
walk = walkWith oneStep

walkWith :: (MonadPlus m, MonadReader WalkInfo m) =>
            (Move -> m Move) -> m Move -> Move -> m Move
walkWith stepF ruleM move = s `mplus` s >>= walkWith stepF ruleM
  where s = stepWith stepF ruleM move

stepAll :: (MonadLogic m, MonadState (S.Set Move) m, MonadReader WalkInfo m)
           => [Move] -> m Move
stepAll = msum . map linearStep

linearStep :: (MonadState (S.Set Move) m, MonadLogic m,
               MonadReader WalkInfo m) =>
              Move -> m Move
linearStep = step linearWalker

step :: (MonadPlus m, MonadReader WalkInfo m) => m Move -> Move -> m Move
step = stepWith oneStep

stepWith :: (MonadReader WalkInfo m) =>
            (Move -> m Move) -> m Move -> Move -> m Move
stepWith stepF ruleM move = stepF move >>= localMove ruleM

oneStep :: (MonadPlus m) => Move -> m Move
oneStep (Move (r, f) d) = liftM (`Move` d) (liftM2 (,) r' f')
  where
    r'
      | d `elem` [Northeast, North, Northwest] = succRank r
      | d `elem` [Southeast, South, Southwest] = predRank r
      | otherwise                              = return r
    f'
      | d `elem` [Southeast, East, Northeast]  = predFile f
      | d `elem` [Southwest, West, Northwest]  = succFile f
      | otherwise                              = return f
{-  ugly but possibly faster version
  (r'', f'') =
    case d of
      Northeast -> (ru, fu)
      East      -> (r', fu)
      Southeast -> (rd, fu)
      South     -> (rd, f')
      Southwest -> (rd, fd)
      West      -> (r', fd)
      Northwest -> (ru, fd)
        where
          (r', f') = (return r, return f)
          (ru, rd) = (succRank r, predRank r)
          (fu, fd) = (succFile f, predFile f)
-}


chebyshevStep :: (MonadPlus m) => Int -> Move -> m Move
chebyshevStep n = msum . chebyshev' . mCoord

chebyshevHop :: (MonadPlus m) => Int -> Move -> m Move
chebyshevHop n = (!!n) . chebyshev' . mCoord

chebyshev' :: (MonadPlus m) => Coord -> [m Move]
chebyshev' c = zipWith mplus allDiags fanCards
  where
    allDiags = spreadFrom diagonals
    allCards = spreadFrom cardinals
    spreadFrom dirs =
      let startPoints = msum . map (oneStep . Move c) $ dirs
      in iterate (>>=oneStep) startPoints

    fanCards = map (\(i,c) -> c >>= fanCard i) $ zip [1..] allCards
    fanCard i m@(Move c d) =
      let joinFans = mplus `on`
                     take i . iterate (>>=oneStep) . oneStep . Move c
      in msum $ return m : uncurry joinFans (rightAngles d)


succRank :: (MonadPlus m) => Rank -> m Rank
succRank = guardMap (/= R8) succ

predRank :: (MonadPlus m) => Rank -> m Rank
predRank = guardMap (/= R1) pred

succFile :: (MonadPlus m) => File -> m File
succFile = guardMap (/= H) succ

predFile :: (MonadPlus m) => File -> m File
predFile = guardMap (/= A) pred

guardMap :: (MonadPlus m) => (a -> Bool) -> (a -> b) -> a -> m b
guardMap p f a = guard (p a) >> return (f a)


linearWalker :: (MonadState (S.Set Move) m,
                 MonadPlus m, MonadReader WalkInfo m) => m Move
linearWalker = stopAtPiece `mplus` followNewPortal

stopAtPiece :: (MonadPlus m, MonadReader WalkInfo m) => m Move
stopAtPiece = do
  maybe (asks wMove) (const mzero) =<< asks wSquare

followNewPortal :: (MonadState (S.Set Move) m, MonadPlus m,
                    MonadReader WalkInfo m) => m Move
followNewPortal = followPortal >>= localMove unlessVisited

followPortal :: (MonadPlus m, MonadReader WalkInfo m) => m Move
followPortal = do
  WalkInfo{wBoard,wSquare, wMove = Move _ dir} <- ask
  case wSquare of
    Just (Piece _ (Portal portalD maybeC)) ->
      do
        guard (backward portalD == dir)
        fromMaybe mzero $ do
          outPortalC <- maybeC
          Piece _ outPortal <- wBoard `at` outPortalC
          case outPortal of
            Portal d _ -> Just $ oneStep (Move outPortalC d)
            _          -> Nothing
    _  -> mzero

unlessVisited :: (MonadState (S.Set Move) m, MonadPlus m, MonadReader WalkInfo m) => m Move
unlessVisited = do
  WalkInfo{wMove} <- ask
  guard =<< liftM (not . (S.member wMove)) get
  modify (S.insert wMove)
  return wMove

ifBlocked :: (MonadLogic m, MonadReader WalkInfo m) => m Move
ifBlocked  = lnot (followPortal) >> ifPiece

ifPiece :: (MonadPlus m, MonadReader WalkInfo m) => m Move
ifPiece = asks wSquare >>= guard . isJust >> asks wMove

startDirFor :: Player -> Direction
startDirFor White = North
startDirFor Black = South

backward :: Direction -> Direction
backward d = let i = fromEnum d
             in toEnum $ if even i
                         then i+1
                         else i-1

rightAngles :: Direction -> (Direction, Direction)
rightAngles d = case d of
  North -> (East, West)
  South -> (East, West)
  West  -> (North, South)
  East  -> (North, South)

rotateCW :: Direction -> Direction
rotateCW Northwest = North   -- special cases
rotateCW Southeast = South
rotateCW d = toEnum $ (fromEnum d + 2)

rotateCCW :: Direction -> Direction
rotateCCW North = Northwest  -- special cases
rotateCCW South = Southeast
rotateCCW d = toEnum $ (fromEnum d - 2)

test1 = print (toEnum . fromEnum $ Just (Piece White King) :: Square)

test2 = print $ fromEnum (maxBound :: Square)

test3 = print (toEnum . fromEnum $ Just (Piece White (Arrow North)) :: Square)

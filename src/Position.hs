module Position
    (
      Position (..)
    , readFEN
    , evolve
    , illegal
    , initialPosition
    ) where

import Control.Applicative
import Data.Bits
import Data.Char
import Data.List
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.Word
 
-- |Chessboard position encoded by five 64 bit word fields, with
-- the first four encoding the bitboards for all pieces as such:
-- 1___ white
-- 0___ black
-- _000 no piece
-- _01_ slides orthogonally
-- _0_1 slides diagonally
-- _100 knight
-- _101 pawn
-- _110 king
-- _111 en passant
data Chessboard = Chessboard
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    deriving (Eq)

-- |Bitboard encoding for white pieces
whiteBB :: Chessboard -> Word64
whiteBB (Chessboard a _ _ _) = a

-- |Bitboard encoding for black pieces
blackBB :: Chessboard -> Word64
blackBB (Chessboard a _ _ _) = complement a

-- |Bitboard encoding for all pieces
pieceBB :: Chessboard -> Word64
pieceBB (Chessboard _ b c d) = (b .|. c .|. d) .&. complement (b .&. c .&. d)

-- |Bitboard encoding for rooks
rookBB :: Chessboard -> Word64
rookBB (Chessboard _ b c d) = c .&. complement (b .|. d)

-- |Bitboard encoding for queens
queenBB :: Chessboard -> Word64
queenBB (Chessboard _ b c d) = complement b .&. c .&. d

-- |Bitboard encoding for bishops
bishopBB :: Chessboard -> Word64
bishopBB (Chessboard _ b c d) = complement (b .|. c) .&. d

-- |Bitboard encoding for knights
knightBB :: Chessboard -> Word64
knightBB (Chessboard _ b c d) = b .&. complement (c .|. d)

-- |Bitboard encoding for pawns
pawnBB :: Chessboard -> Word64
pawnBB (Chessboard _ b c d) = b .&. complement c .&. d

-- |Bitboard encoding for kings
kingBB :: Chessboard -> Word64
kingBB (Chessboard _ b c d) = b .&. c .&. complement d

-- |Bitboard encoding for en passant square
enpassantBB :: Chessboard -> Word64
enpassantBB (Chessboard _ b c d) = b .&. c .&. d

-- |This is probably a little too "pretty" for a basic
-- Show instance to be completely honest.
instance Show Chessboard where
    show cb =
        let pawns = pawnBB cb
            bishops = bishopBB cb
            knights = knightBB cb
            rooks = rookBB cb
            queens = queenBB cb
            kings = kingBB cb
            whites = whiteBB cb
            blacks = blackBB cb
            ep = enpassantBB cb
            bb = [ f 'P' (pawns .&. whites)
                 , f 'p' (pawns .&. blacks)
                 , f 'B' (bishops .&. whites)
                 , f 'b' (bishops .&. blacks)
                 , f 'N' (knights .&. whites)
                 , f 'n' (knights .&. blacks)
                 , f 'R' (rooks .&. whites)
                 , f 'r' (rooks .&. blacks)
                 , f 'Q' (queens .&. whites)
                 , f 'q' (queens .&. blacks)
                 , f 'K' (kings .&. whites)
                 , f 'k' (kings .&. blacks)
                 , f 'e' ep
                 ]
            str = map (fromMaybe '.') $ foldr1 (zipWith (<|>)) bb
        in intercalate "\n" (reverse $ chunksOf 8 str)
        where f c b = map (\l -> if testBit b l
                           then Just c
                           else Nothing
                      ) [0..63]

-- |Chess position including both the chessboard encoding
-- as well as metadata about whose move it is, castling
-- rights, and move counters.
data Position = Position
                    Chessboard --Bitboard encoding
                    Bool       --Is white to move?
                    Word8      --Dense encoding = _ _ _ _ q k Q K
    deriving (Eq)

-- |Bitboard encoding for the active player.
toMoveBB :: Position -> Word64
toMoveBB (Position cb w _) = if w then whiteBB cb else blackBB cb

-- |Bitboard encoding for the inactive player.
notToMoveBB :: Position -> Word64
notToMoveBB (Position cb w _) = if w then blackBB cb else whiteBB cb

-- |Read a FEN string into a Position.
readFEN :: String -> Position
readFEN fen =
    let bd:m:castles:ep:_ = words fen
        bd' = concatMap repl bd
        bd'' = concat $ reverse $ chunksOf 8 $ filter (/= '/') bd'
        a = bbFromList $ map isUpper bd''
        b = bbFromList $ map (`elem` "NnPpKk") bd''
        c = bbFromList $ map (`elem` "RrQqKk") bd''
        d = bbFromList $ map (`elem` "QqBbPp") bd''
        castleRights = foldr ((.|.) . (\case 'K' -> 0x1
                                             'Q' -> 0x2
                                             'k' -> 0x4
                                             'q' -> 0x8
                                             _   -> 0x0)) 0x0 castles
        e = maybe 0x0 bit $ elemIndex ep [[fi, r] | fi <- ['a'..'h'], r <- ['1'..'8']]
        cb = Chessboard (a .|. e) (b .|. e) (c .|. e) (d .|. e)
    in Position cb (m == "w") castleRights
    where repl x | isDigit x = replicate (digitToInt x) '.'
          repl x = [x]
          bbFromList = foldr f (zeroBits :: Word64)
          f False i = unsafeShiftL i 1 
          f True i = bit 0 .|. unsafeShiftL i 1

-- |Initial position in standard chess.
initialPosition :: Position
initialPosition = readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

-- |Again, this is probably too pretty for a basic
-- Show instance.
instance Show Position where
    show (Position cb w _) =
        let footer = if w then "\n\nWhite to move" else "\n\nBlack to move"
        in "\n" ++ show cb ++ footer

-- |Datatype for all 16 intercardinal directions pieces can move.
data Direction = N | NNE | NE | NEE
               | E | SEE | SE | SSE
               | S | SSW | SW | SWW
               | W | NWW | NW | NNW
               deriving (Show, Eq)


notAMask :: Word64
notAMask = 0xFEFEFEFEFEFEFEFE

notABMask :: Word64
notABMask = 0xFCFCFCFCFCFCFCFC

yes8Mask :: Word64
yes8Mask = 0xFF00000000000000

yes3Mask :: Word64
yes3Mask = 0xFF0000

yes6Mask :: Word64
yes6Mask = 0xFF0000000000

yes1Mask :: Word64
yes1Mask = 0xFF

shiftOne :: Direction -> Word64 -> Word64
shiftOne N bb = unsafeShiftL bb 8
shiftOne NNE bb = unsafeShiftL bb 17 .&. notAMask
shiftOne NE bb = unsafeShiftL bb 9 .&. notAMask
shiftOne NEE bb = unsafeShiftL bb 10 .&. notABMask
shiftOne E bb = unsafeShiftL bb 1 .&. notAMask
shiftOne SEE bb = unsafeShiftR bb 6 .&. notABMask
shiftOne SE bb = unsafeShiftR bb 7 .&. notAMask
shiftOne SSE bb = unsafeShiftR bb 15 .&. notAMask
shiftOne S bb = unsafeShiftR bb 8
shiftOne SSW bb = unsafeShiftR (bb .&. notAMask) 17
shiftOne SW bb = unsafeShiftR (bb .&. notAMask) 9
shiftOne SWW bb = unsafeShiftR (bb .&. notABMask) 10
shiftOne W bb = unsafeShiftR (bb .&. notAMask) 1
shiftOne NWW bb = unsafeShiftL (bb .&. notABMask) 6
shiftOne NW bb = unsafeShiftL (bb .&. notAMask) 7
shiftOne NNW bb = unsafeShiftL (bb .&. notAMask) 15

slidingAttacks :: Word64 -> Word64 -> Word64 -> [Direction] -> Word64
slidingAttacks pc unocc unoccOpp dirs =
    foldr1 (.|.) $ map (\dir -> unoccOpp .&. shiftOne dir (foldr1 (.|.) $ take 7 $ iterate (\bb -> unocc .&. shiftOne dir bb) pc)) dirs

evolve :: Position -> [Position]
evolve pos@(Position cb@(Chessboard a b c d) w castleRights) =
    let unocc = complement $ pieceBB cb
        unoccOpp = complement (toMoveBB pos .&. pieceBB cb)
        ep = enpassantBB cb
--Calculate rook moves
        rooks = toMoveBB pos .&. rookBB cb
        rookFrom = serialize rooks
        rookTo = calcTos unocc unoccOpp [N,E,S,W] rookFrom
        rookMoves = calcMoves rookFrom rookTo
        rookNewPos = map (\(from, to) ->
            Chessboard (if w then a .|. to else a .&. complement to)
                       (b .&. complement to)
                       (c .&. complement from .|. to)
                       (d .&. complement to)) rookMoves --TODO: Castling logic
--Calculate bishop moves
        bishops = toMoveBB pos .&. bishopBB cb
        bishopFrom = serialize bishops
        bishopTo = calcTos unocc unoccOpp [NE,SE,SW,NW] bishopFrom
        bishopMoves = calcMoves bishopFrom bishopTo
        bishopNewPos = map (\(from, to) ->
            Chessboard (if w then a .|. to else a .&. complement to)
                       (b .&. complement to)
                       (c .&. complement to)
                       (d .&. complement from .|. to)) bishopMoves
--Calculate queen moves
        queens = toMoveBB pos .&. queenBB cb
        queenFrom = serialize queens
        queenTo = calcTos unocc unoccOpp [N,NE,E,SE,S,SW,W,NW] queenFrom
        queenMoves = calcMoves queenFrom queenTo
        queenNewPos = map (\(from, to) ->
            Chessboard (if w then a .|. to else a .&. complement to)
                       (b .&. complement to)
                       (c .&. complement from .|. to)
                       (d .&. complement from .|. to)) queenMoves
--Calculate knight moves
        knights = toMoveBB pos .&. knightBB cb
        knightFrom = serialize knights
        knightTo = map (\bb ->
            serialize $ foldr1 (.|.) $ map (\dir -> unoccOpp .&. shiftOne dir bb)
                [NNE, NEE, SEE, SSE, SSW, SWW, NWW, NNW]) knightFrom
        knightMoves = calcMoves knightFrom knightTo
        knightNewPos = map (\(from, to) ->
            Chessboard (if w then a .|. to else a .&. complement to)
                       (b .&. complement from .|. to)
                       (c .&. complement to)
                       (d .&. complement to)) knightMoves
--Calculate king moves
        kings = toMoveBB pos .&. kingBB cb
        kingFrom = serialize kings
        kingTo = map (\bb ->
            serialize $ foldr1 (.|.) $ map (\dir -> unoccOpp .&. shiftOne dir bb)
                [N, NE, E, SE, S, SW, W, NW]) kingFrom
        kingMoves = calcMoves kingFrom kingTo
        kingNewPos = map (\(from, to) ->
            Chessboard (if w then a .|. to else a .&. complement to)
                       (b .&. complement from .|. to)
                       (c .&. complement from .|. to)
                       (d .&. complement to)) kingMoves --TODO: Castling logic
--Calculate pawn moves (single, double, attack, en passant, promote)
        pawns = toMoveBB pos .&. pawnBB cb
        (pawnMoveDir, pawnRevMoveDir, pawnAttackDirs, pawnDoubleMoveMask, pawnPromoteMask) = if w
            then (N, S, [NE,NW], yes3Mask, yes8Mask)
            else (S, N, [SE,SW], yes6Mask, yes1Mask)
        pawnFrom = serialize pawns
        pawnTo = map ((unocc .&.) . shiftOne pawnMoveDir) pawnFrom
        pawnToSingle = map serialize pawnTo
        pawnToDouble = map (\bb -> serialize (unocc .&. shiftOne pawnMoveDir (bb .&. pawnDoubleMoveMask))) pawnTo
        pawnToAttack = map (\bb -> serialize ((notToMoveBB pos .&. pieceBB cb) .&. foldr ((.|.) .(`shiftOne` bb)) 0x0 pawnAttackDirs)) pawnFrom
        pawnToEp = map (\bb -> serialize (ep .&. foldr ((.|.) . (`shiftOne` bb)) 0x0 pawnAttackDirs)) pawnFrom
        pawnSingleMoves = calcMoves pawnFrom pawnToSingle ++ calcMoves pawnFrom pawnToAttack
        pawnEpMoves = calcMoves pawnFrom pawnToEp
        (pawnMoves', pawnPromoteMoves) = partition ((== zeroBits) . (.&. pawnPromoteMask) . snd) pawnSingleMoves
        pawnDoubleMoves = calcMoves pawnFrom pawnToDouble
        pawnSingleNewPos = map (\(from, to) ->
            Chessboard (if w then a .|. to else a .&. complement to)
                       (b .&. complement from .|. to)
                       (c .&. complement to)
                       (d .&. complement from .|. to)) pawnMoves'
        pawnDoubleNewPos = map (\(from, to) ->
            Chessboard (if w then a .|. to else a .&. complement to)
                       (b .&. complement from .|. to .|. shiftOne pawnRevMoveDir to)
                       (c .&. complement to .|. shiftOne pawnRevMoveDir to)
                       (d .&. complement from .|. to .|. shiftOne pawnRevMoveDir to)) pawnDoubleMoves
        pawnEpNewPos = map (\(from, to) ->
            Chessboard (if w then a .|. to else a .&. complement to)
                       (b .&. complement (from .|. shiftOne pawnRevMoveDir to) .|. to)
                       (c .&. complement to)
                       (d .&. complement (from .|. shiftOne pawnRevMoveDir to) .|. to)) pawnEpMoves
        pawnQueenPromotes = map (\(from, to) ->
            Chessboard (if w then a .|. to else a .&. complement to)
                       (b .&. complement (from .|. to))
                       (c .|. to)
                       (d .&. complement from .|. to)) pawnPromoteMoves
        pawnRookPromotes = map (\(from, to) ->
            Chessboard (if w then a .|. to else a .&. complement to)
                       (b .&. complement (from .|. to))
                       (c .|. to)
                       (d .&. complement (from .|. to))) pawnPromoteMoves
        pawnBishopPromotes = map (\(from, to) ->
            Chessboard (if w then a .|. to else a .&. complement to)
                       (b .&. complement (from .|. to))
                       (c .&. complement to)
                       (d .&. complement from .|. to)) pawnPromoteMoves
        pawnKnightPromotes = map (\(from, to) ->
            Chessboard (if w then a .|. to else a .&. complement to)
                       (b .&. complement from .|. to)
                       (c .&. complement to)
                       (d .&. complement (from .|. to))) pawnPromoteMoves
--TODO: Calculate castling moves 
    in filter (not . illegal) $ map (\bd@(Chessboard a' b' c' d') ->
                                            Position
                                                (if enpassantBB bd /= zeroBits
                                                    then Chessboard a'
                                                                    (b' .&. complement ep)
                                                                    (c' .&. complement ep)
                                                                    (d' .&. complement ep)
                                                    else bd)
                                                (not w)
                                                castleRights)
            (rookNewPos ++ bishopNewPos ++ queenNewPos ++ knightNewPos ++ kingNewPos
                        ++ pawnSingleNewPos ++ pawnDoubleNewPos ++ pawnEpNewPos
                        ++ pawnQueenPromotes ++ pawnRookPromotes ++ pawnBishopPromotes ++ pawnKnightPromotes)
    where calcTos unocc unoccOpp dirs = map (\pc ->
                serialize $ slidingAttacks pc unocc unoccOpp dirs)
          calcMoves f t = concatMap (\(from, tos) ->
                map (from,) tos) $ zip f t

illegal :: Position -> Bool
illegal pos@(Position cb w _) =
    let unocc = complement $ pieceBB cb
        unoccOpp = complement (toMoveBB pos .&. pieceBB cb)
        mking = notToMoveBB pos .&. kingBB cb
        pawnAttackDirs = if w then [SE, SW] else [NE, NW]
        pawnCheck = foldr1 (.|.) (map (`shiftOne` mking) pawnAttackDirs) .&. toMoveBB pos .&. pawnBB cb /= zeroBits
        kingCheck = foldr1 (.|.) (map (`shiftOne` mking) [N, NE, E, SE, S, SW, W, NW]) .&. toMoveBB pos .&. kingBB cb /= zeroBits
        knightCheck = foldr1 (.|.) (map (`shiftOne` mking) [NNE, NEE, SEE, SSE, SSW, SWW, NWW, NNW]) .&. toMoveBB pos .&. knightBB cb /= zeroBits
        orthCheck = slidingAttacks (toMoveBB pos .&. (rookBB cb .|. queenBB cb)) unocc unoccOpp [N,E,S,W] .&. mking /= zeroBits
        diagCheck = slidingAttacks (toMoveBB pos .&. (bishopBB cb .|. queenBB cb)) unocc unoccOpp [NE, SE, SW, NW] .&. mking /= zeroBits
    in pawnCheck || knightCheck || kingCheck || orthCheck || diagCheck

serialize :: Word64 -> [Word64]
serialize 0 = []
serialize b = (b .&. negate b):serialize (b .&. (b - 1))

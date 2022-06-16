/*The code is mostly from Weiss
Integrated classic bitboard move generation code from Butter*/

#include <stddef.h>
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include <windows.h>
#include <intrin.h>
#include "math.h"
#include <stdbool.h>
#include <inttypes.h>
#include <stdint.h>
#include <setjmp.h>

#define NAME "Journeyman"
#define VERSION "2.1"
#define AUTHOR "Jay Warendorff"

typedef uint64_t Bitboard;
typedef uint64_t Key;

typedef uint32_t Move;
typedef uint32_t Square;

typedef int32_t Color;
typedef int32_t Piece;
typedef int32_t PieceType;


#define MAXGAMEMOVES 256
#define MAXPOSITIONMOVES 256
#define MAXDEPTH 128

#define INF 32500

#define ISMATE (INF - MAXDEPTH)

#define NOSCORE 32501

// Macro for printing size_t
#ifdef _WIN32
#  ifdef _WIN64
#    define PRI_SIZET PRIu64
#  else
#    define PRI_SIZET PRIu32
#  endif
#else
#  define PRI_SIZET "zu"
#endif

#define MIN(A, B) ((A) < (B) ? (A) : (B))
#define MAX(A, B) ((A) > (B) ? (A) : (B))
#define CLAMP(x, low, high)  (((x) > (high)) ? (high) : (((x) < (low)) ? (low) : (x)))

#define lastMoveNullMove (!root && history(-1).move == NOMOVE)
#define history(offset) (pos->gameHistory[pos->histPly + offset])
#define killer1 (pos->killers[pos->ply][0])
#define killer2 (pos->killers[pos->ply][1])

#define BB(sq) (1ULL << sq)
#define pieceBB(type) (pos->pieceBB[(type)])
#define colorBB(color) (pos->colorBB[(color)])
#define colorPieceBB(color, type) (colorBB(color) & pieceBB(type))
#define sideToMove (pos->stm)
#define pieceOn(sq) (pos->board[sq])
#define kingSq(color) (Lsb(colorPieceBB(color, KING)))

enum Color {
    BLACK, WHITE, COLOR_NB
};

enum PieceType {
    ALL, PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING, TYPE_NB = 8
};

enum Piece {
    EMPTY,
    bP = 1, bN, bB, bR, bQ, bK,
    wP = 9, wN, wB, wR, wQ, wK,
    PIECE_NB = 16
};

enum PieceValue {
    P_MG = 102, P_EG = 174,
    N_MG = 420, N_EG = 516,
    B_MG = 425, B_EG = 527,
    R_MG = 585, R_EG = 926,
    Q_MG = 1418, Q_EG = 1702
};

enum File {
    FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H, FILE_NONE
};

enum Rank {
    RANK_1, RANK_2, RANK_3, RANK_4, RANK_5, RANK_6, RANK_7, RANK_8, RANK_NONE
};

enum Square  {
    A1, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8
};

typedef enum Direction {
    north = 8,
    east = 1,
    south = -north,
    west = -east
} Direction;

enum { NORTH, NORTH_EAST, EAST, SOUTH_EAST, SOUTH, SOUTH_WEST, WEST, NORTH_WEST };

enum CastlingRights {
    WHITE_OO = 1,
    WHITE_OOO = 2,
    BLACK_OO = 4,
    BLACK_OOO = 8,

    OO = WHITE_OO | BLACK_OO,
    OOO = WHITE_OOO | BLACK_OOO,
    WHITE_CASTLE = WHITE_OO | WHITE_OOO,
    BLACK_CASTLE = BLACK_OO | BLACK_OOO
};

typedef struct PV {
    int length;
    Move input[MAXDEPTH];
} PV;

enum { BOUND_NONE, BOUND_UPPER, BOUND_LOWER, BOUND_EXACT };

typedef struct {

    Key posKey;
    Move move;
    int16_t score;
    uint8_t depth;
    uint8_t bound;

} TTEntry;

typedef struct {

    void* mem;
    TTEntry* table;
    size_t count;
    size_t currentMB;
    size_t requestedMB;
    bool dirty;

} TranspositionTable;

typedef struct {
    Key posKey;
    Move move;
    uint8_t epSquare;
    uint8_t rule50;
    uint8_t castlingRights;
    uint8_t padding; // not used
    int eval;
} History;

typedef struct {
    Move move;

    uint8_t epSquare;
    uint8_t rule50;
    uint8_t castlingRights;
    Key posKey;
} Undo;

typedef struct Position {

    uint8_t board[64];
    Bitboard pieceBB[TYPE_NB];
    Bitboard colorBB[2];

    int nonPawnCount[2];

    int material;
    int phaseValue;
    int phase;

    Color stm;
    uint8_t epSquare;
    uint8_t rule50;
    uint8_t castlingRights;

    uint8_t ply;
    uint16_t histPly;
    uint16_t gameMoves;

    Key key;

    History gameHistory[MAXGAMEMOVES];

    int history[PIECE_NB][64];
    Move killers[MAXDEPTH][2];

} Position;

typedef struct SearchInfo {

    int quit;
    int startTime;
    int endTime;
    int timeSet;
    int movestogo;

    uint64_t nodes;

    int score;
    int depth;
    Move bestMove;
    int seldepth;

    PV pv;

    jmp_buf jumpBuffer;

    // Anything below here is not zeroed out between searches

    int index;
    int count;

    int IDDepth;

} SearchInfo;

// Population count

#define bitCount(bb) ((int) (__popcnt64(bb)))

// Returns the index of the least significant bit

int Lsb(const Bitboard bb) {
    unsigned long index = -1;
    _BitScanForward64(&index, bb);
    return index;
}

// Returns the index of the least significant bit and unsets it
int PopLsb(Bitboard* bb) {

    int lsb = Lsb(*bb);
    *bb &= (*bb - 1);

    return lsb;
}

// Mirrors a square horizontally
Square MirrorSquare(const Square sq) {
    return sq ^ 56;
}

Square RelativeSquare(const Color color, const Square sq) {
    return color == WHITE ? sq : MirrorSquare(sq);
}

// Returns the same piece of the opposite color
Piece MirrorPiece(Piece piece) {
    return piece == EMPTY ? EMPTY : piece ^ 8;
}

int FileOf(const Square square) {
    return square & 7;
}

int RankOf(const Square square) {
    return square >> 3;
}

// Converts a rank into the rank relative to the given color
int RelativeRank(const Color color, const int rank) {
    return color == WHITE ? rank : RANK_8 - rank;
}

Color ColorOf(const Piece piece) {
    return piece >> 3;
}

PieceType PieceTypeOf(const Piece piece) {
    return (piece & 7);
}

Piece MakePiece(const Color color, const PieceType pt) {
    return (color << 3) + pt;
}

/* Move contents - total 23bits used
0000 0000 0000 0000 0011 1111 -> From       <<  0
0000 0000 0000 1111 1100 0000 -> To         <<  6
0000 0000 1111 0000 0000 0000 -> Captured   << 12
0000 1111 0000 0000 0000 0000 -> Promotion  << 16
0001 0000 0000 0000 0000 0000 -> En passant << 20
0010 0000 0000 0000 0000 0000 -> Pawn Start << 21
0100 0000 0000 0000 0000 0000 -> Castle     << 22
*/

#define NOMOVE 0

// Fields
#define MOVE_FROM       0x00003F
#define MOVE_TO         0x000FC0
#define MOVE_CAPT       0x00F000
#define MOVE_PROMO      0x0F0000
#define MOVE_FLAGS      0x700000

// Special move flags
#define FLAG_NONE       0
#define FLAG_ENPAS      0x100000
#define FLAG_PAWNSTART  0x200000
#define FLAG_CASTLE     0x400000

// Move constructor
#define MOVE(f, t, ca, pro, fl) ((f) | ((t) << 6) | ((ca) << 12) | ((pro) << 16) | (fl))

// Extract info from a move
#define fromSq(move)     ((move) & MOVE_FROM)
#define toSq(move)      (((move) & MOVE_TO)    >>  6)
#define capturing(move) (((move) & MOVE_CAPT)  >> 12)
#define promotion(move) (((move) & MOVE_PROMO) >> 16)

// Move types
#define moveIsEnPas(move)   (move & FLAG_ENPAS)
#define moveIsPStart(move)  (move & FLAG_PAWNSTART)
#define moveIsCastle(move)  (move & FLAG_CASTLE)
#define moveIsSpecial(move) (move & (MOVE_FLAGS | MOVE_PROMO))
#define moveIsCapture(move) (move & MOVE_CAPT)
#define moveIsNoisy(move)   (move & (MOVE_CAPT | MOVE_PROMO | FLAG_ENPAS))
#define moveIsQuiet(move)   (!moveIsNoisy(move))

typedef struct {
    Move move;
    int score;
} MoveListEntry;

typedef struct {
    int count;
    int next;
    MoveListEntry moves[MAXPOSITIONMOVES];
} MoveList;

typedef enum MPStage {
    tTMOVE, gEN_NOISY, nOISY_GOOD, kILLER1, kILLER2, gEN_QUIET, qUIET, nOISY_BAD
} MPStage;

typedef struct MovePicker {
    Position* pos;
    MoveList* list;
    MPStage stage;
    uint8_t depth;
    Move ttMove, kill1, kill2;
    int bads;
    bool onlyNoisy;
} MovePicker;

#define START_FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
#define INPUT_SIZE 4096

void PrintThinking(Position* pos, SearchInfo* info);
void PrintConclusion(SearchInfo* info);

#define fileABB 0x0101010101010101ULL
#define fileBBB 0x0202020202020202ULL
#define fileCBB 0x0404040404040404ULL
#define fileDBB 0x0808080808080808ULL
#define fileEBB 0x1010101010101010ULL
#define fileFBB 0x2020202020202020ULL
#define fileGBB 0x4040404040404040ULL
#define fileHBB 0x8080808080808080ULL

#define rank1BB 0xFF
#define rank2BB 0xFF00
#define rank3BB 0xFF0000
#define rank4BB 0xFF000000
#define rank5BB 0xFF00000000
#define rank6BB 0xFF0000000000
#define rank7BB 0xFF000000000000
#define rank8BB 0xFF00000000000000

const Bitboard FileBB[8] = {
    fileABB, fileBBB, fileCBB, fileDBB, fileEBB, fileFBB, fileGBB, fileHBB
};

const Bitboard RankBB[8] = {
    rank1BB, rank2BB, rank3BB, rank4BB, rank5BB, rank6BB, rank7BB, rank8BB
};

Bitboard PassedMask[2][64];
Bitboard IsolatedMask[64];

// Shifts a bitboard (protonspring version)
// Doesn't work for shifting more than one step horizontally
Bitboard ShiftBB(const Direction dir, Bitboard bb) {

    // Horizontal shifts should not wrap around
    const int h = dir & 7;
    bb = (h == 1) ? bb & ~fileHBB
        : (h == 7) ? bb & ~fileABB
        : bb;

    // Can only shift by positive numbers
    return dir > 0 ? bb << dir
        : bb >> -dir;
}

Bitboard AdjacentFilesBB(const Square sq) {

    return ShiftBB(west, FileBB[FileOf(sq)])
        | ShiftBB(east, FileBB[FileOf(sq)]);
}

// Checks whether or not a bitboard has multiple set bits
bool Multiple(Bitboard bb) {

    return bb & (bb - 1);
}

// Checks whether or not a bitboard has a single set bit
bool Single(Bitboard bb) {

    return bb && !Multiple(bb);
}

#define getSquare(file, rank) (((rank) * 8) + (file))
#define onBoard(file, rank) (file >= FILE_A && file <= FILE_H && rank >= RANK_1 && rank <= RANK_8)

#define setBit(bb,sq) ((bb) |= setMask[(sq)])

int fileArray[64];
int rankArray[64];

Bitboard knightAttacks[64];
Bitboard kingAttacks[64];
Bitboard rookAttacksEmpty[64];
Bitboard bishopAttacksEmpty[64];

Bitboard bitRays[8][64];

Bitboard setMask[65];

void initBitMasks() {
    for (int index = 0; index < 64; index++) {
        setMask[index] = 1ULL << index;
    }
    setMask[64] = 0ULL;
}

void initFileRankArrays() {
    for (int square = 0; square < 64; square++) {
        fileArray[square] = square % 8;
        rankArray[square] = square / 8;
    }
}

void initKnightAttacks() {
    for (int square = 0; square < 64; square++) {
        int originalFile = fileArray[square];
        int originalRank = rankArray[square];

        int file, rank;
        knightAttacks[square] = 0ULL;

        file = originalFile + 2; rank = originalRank + 1;
        if (onBoard(file, rank)) setBit(knightAttacks[square], getSquare(file, rank));
        rank = originalRank - 1;
        if (onBoard(file, rank)) setBit(knightAttacks[square], getSquare(file, rank));
        file = originalFile + 1; rank = originalRank + 2;
        if (onBoard(file, rank)) setBit(knightAttacks[square], getSquare(file, rank));
        rank = originalRank - 2;
        if (onBoard(file, rank)) setBit(knightAttacks[square], getSquare(file, rank));
        file = originalFile - 1; rank = originalRank + 2;
        if (onBoard(file, rank)) setBit(knightAttacks[square], getSquare(file, rank));
        rank = originalRank - 2;
        if (onBoard(file, rank)) setBit(knightAttacks[square], getSquare(file, rank));
        file = originalFile - 2; rank = originalRank + 1;
        if (onBoard(file, rank)) setBit(knightAttacks[square], getSquare(file, rank));
        rank = originalRank - 1;
        if (onBoard(file, rank)) setBit(knightAttacks[square], getSquare(file, rank));
    }
}

void initKingAttacks() {
    for (int square = 0; square < 64; square++) {
        int file = fileArray[square];
        int rank = rankArray[square];

        kingAttacks[square] = 0ULL;

        if (onBoard(file + 1, rank + 1)) setBit(kingAttacks[square], getSquare(file + 1, rank + 1));
        if (onBoard(file + 1, rank)) setBit(kingAttacks[square], getSquare(file + 1, rank));
        if (onBoard(file + 1, rank - 1)) setBit(kingAttacks[square], getSquare(file + 1, rank - 1));
        if (onBoard(file, rank + 1)) setBit(kingAttacks[square], getSquare(file, rank + 1));
        if (onBoard(file, rank - 1)) setBit(kingAttacks[square], getSquare(file, rank - 1));
        if (onBoard(file - 1, rank + 1)) setBit(kingAttacks[square], getSquare(file - 1, rank + 1));
        if (onBoard(file - 1, rank)) setBit(kingAttacks[square], getSquare(file - 1, rank));
        if (onBoard(file - 1, rank - 1)) setBit(kingAttacks[square], getSquare(file - 1, rank - 1));
    }
}

void initRookAttacks() {
    for (int square = 0; square < 64; square++) {
        rookAttacksEmpty[square] = 0ULL;

        int file = fileArray[square];
        int rank = rankArray[square];

        for (int f = file + 1; f <= FILE_H; f++) {
            setBit(rookAttacksEmpty[square], getSquare(f, rank));
        }
        for (int f = file - 1; f >= FILE_A; f--) {
            setBit(rookAttacksEmpty[square], getSquare(f, rank));
        }
        for (int r = rank + 1; r <= RANK_8; r++) {
            setBit(rookAttacksEmpty[square], getSquare(file, r));
        }
        for (int r = rank - 1; r >= RANK_1; r--) {
            setBit(rookAttacksEmpty[square], getSquare(file, r));
        }
    }
}

void initBishopAttacks() {
    for (int square = 0; square < 64; square++) {
        bishopAttacksEmpty[square] = 0ULL;

        for (int tr = square + 9; (tr % 8 > 0) && (tr < 64); tr += 9) {
            setBit(bishopAttacksEmpty[square], tr);
        }
        for (int tl = square + 7; (tl % 8 < 7) && (tl < 64); tl += 7) {
            setBit(bishopAttacksEmpty[square], tl);
        }
        for (int br = square - 7; (br % 8 > 0) && (br >= 0); br -= 7) {
            setBit(bishopAttacksEmpty[square], br);
        }
        for (int bl = square - 9; (bl % 8 < 7) && (bl >= 0); bl -= 9) {
            setBit(bishopAttacksEmpty[square], bl);
        }
    }
}

void initBitRays() {
    for (int i = 0; i < 64; i++) {
        bitRays[NORTH][i] = 0ULL;
        bitRays[NORTH_EAST][i] = 0ULL;
        bitRays[EAST][i] = 0ULL;
        bitRays[SOUTH_EAST][i] = 0ULL;
        bitRays[SOUTH][i] = 0ULL;
        bitRays[SOUTH_WEST][i] = 0ULL;
        bitRays[WEST][i] = 0ULL;
        bitRays[NORTH_WEST][i] = 0ULL;
        for (int sq = i + 8; sq < 64; sq += 8) {
            setBit(bitRays[NORTH][i], sq);
        }
        for (int sq = i + 9; sq < 64; sq += 9) {
            if (fileArray[sq] == FILE_A) break;
            setBit(bitRays[NORTH_EAST][i], sq);
        }
        for (int sq = i + 1; sq < 64; sq += 1) {
            if (fileArray[sq] == FILE_A) break;
            setBit(bitRays[EAST][i], sq);
        }
        for (int sq = i - 7; sq >= 0; sq -= 7) {
            if (fileArray[sq] == FILE_A) break;
            setBit(bitRays[SOUTH_EAST][i], sq);
        }
        for (int sq = i - 8; sq >= 0; sq -= 8) {
            setBit(bitRays[SOUTH][i], sq);
        }
        for (int sq = i - 9; sq >= 0; sq -= 9) {
            if (fileArray[sq] == FILE_H) break;
            setBit(bitRays[SOUTH_WEST][i], sq);
        }
        for (int sq = i - 1; sq >= 0; sq -= 1) {
            if (fileArray[sq] == FILE_H) break;
            setBit(bitRays[WEST][i], sq);
        }
        for (int sq = i + 7; sq < 64; sq += 7) {
            if (fileArray[sq] == FILE_H) break;
            setBit(bitRays[NORTH_WEST][i], sq);
        }
    }
}

int peekBit(Bitboard bb) {
    unsigned long index = -1;
    _BitScanForward64(&index, bb);
    return index;
}

int peekBitReverse(Bitboard bb) {
    unsigned long index = -1;
    _BitScanReverse64(&index, bb);
    return index;
}

const Bitboard NOT_FILE_A_MASK = 0xFEFEFEFEFEFEFEFE;
const Bitboard NOT_FILE_H_MASK = 0x7F7F7F7F7F7F7F7F;

Bitboard rookAttacks(Position* pos, int sq) {
    Bitboard occ = (pos->colorBB[0] | pos->colorBB[1]) | 0x8000000000000001;
    int n = peekBit(occ & (bitRays[NORTH][sq] | setMask[63]));
    int e = peekBit(occ & (bitRays[EAST][sq] | setMask[63]));
    int s = peekBitReverse(occ & (bitRays[SOUTH][sq] | setMask[0]));
    int w = peekBitReverse(occ & (bitRays[WEST][sq] | setMask[0]));
    return rookAttacksEmpty[sq] ^ bitRays[NORTH][n] ^ bitRays[EAST][e] ^ bitRays[SOUTH][s] ^ bitRays[WEST][w];
}

Bitboard bishopAttacks(Position* pos, int sq) {
    Bitboard occ = (pos->colorBB[0] | pos->colorBB[1]) | 0x8000000000000001;
    int nw = peekBit(occ & (bitRays[NORTH_WEST][sq] | setMask[63]));
    int ne = peekBit(occ & (bitRays[NORTH_EAST][sq] | setMask[63]));
    int sw = peekBitReverse(occ & (bitRays[SOUTH_WEST][sq] | setMask[0]));
    int se = peekBitReverse(occ & (bitRays[SOUTH_EAST][sq] | setMask[0]));
    return bishopAttacksEmpty[sq] ^ bitRays[NORTH_WEST][nw] ^ bitRays[NORTH_EAST][ne] ^ bitRays[SOUTH_WEST][sw] ^ bitRays[SOUTH_EAST][se];
}

Bitboard RookAttackBB(int sq, Bitboard occ) {
    Bitboard occ1 = occ | 0x8000000000000001;
    int n = peekBit(occ1 & (bitRays[NORTH][sq] | setMask[63]));
    int e = peekBit(occ1 & (bitRays[EAST][sq] | setMask[63]));
    int s = peekBitReverse(occ1 & (bitRays[SOUTH][sq] | setMask[0]));
    int w = peekBitReverse(occ1 & (bitRays[WEST][sq] | setMask[0]));
    return rookAttacksEmpty[sq] ^ bitRays[NORTH][n] ^ bitRays[EAST][e] ^ bitRays[SOUTH][s] ^ bitRays[WEST][w];
}

Bitboard BishopAttackBB(int sq, Bitboard occ) {
    Bitboard occ1 = occ | 0x8000000000000001;
    int nw = peekBit(occ1 & (bitRays[NORTH_WEST][sq] | setMask[63]));
    int ne = peekBit(occ1 & (bitRays[NORTH_EAST][sq] | setMask[63]));
    int sw = peekBitReverse(occ1 & (bitRays[SOUTH_WEST][sq] | setMask[0]));
    int se = peekBitReverse(occ1 & (bitRays[SOUTH_EAST][sq] | setMask[0]));
    return bishopAttacksEmpty[sq] ^ bitRays[NORTH_WEST][nw] ^ bitRays[NORTH_EAST][ne] ^ bitRays[SOUTH_WEST][sw] ^ bitRays[SOUTH_EAST][se];
}

int attackedByWhite(Position* pos, int sq) {
    if (knightAttacks[sq] & colorPieceBB(WHITE, KNIGHT)) return 1;
    if (bishopAttacks(pos, sq) & ((pieceBB(BISHOP) | pieceBB(QUEEN)) & colorBB(WHITE))) return 1;
    if (rookAttacks(pos, sq) & ((pieceBB(ROOK) | pieceBB(QUEEN)) & colorBB(WHITE))) return 1;
    if ((((colorPieceBB(WHITE, PAWN) << 7) & NOT_FILE_H_MASK) | ((colorPieceBB(WHITE, PAWN) << 9) & NOT_FILE_A_MASK)) & setMask[sq]) return 1;
    if (kingAttacks[sq] & colorPieceBB(WHITE, KING)) return 1;
    return 0;
}

int attackedByBlack(Position* pos, int sq) {
    if (knightAttacks[sq] & colorPieceBB(BLACK, KNIGHT)) return 1;
    if (bishopAttacks(pos, sq) & ((pieceBB(BISHOP) | pieceBB(QUEEN)) & colorBB(BLACK))) return 1;
    if (rookAttacks(pos, sq) & ((pieceBB(ROOK) | pieceBB(QUEEN)) & colorBB(BLACK))) return 1;
    if ((((colorPieceBB(BLACK, PAWN) >> 9)& NOT_FILE_H_MASK) | ((colorPieceBB(BLACK, PAWN) >> 7)& NOT_FILE_A_MASK))& setMask[sq]) return 1;
    if (kingAttacks[sq] & colorPieceBB(BLACK, KING)) return 1;
    return 0;
}

bool SqAttacked(int sq, Color color, Position* pos)
{
    if (color == WHITE)
    {
        return (attackedByWhite(pos, sq) == 1);
    }
    else
    {
        return (attackedByBlack(pos, sq) == 1);
    }
}

Bitboard BetweenBB[64][64];

// Checks legality of a specific castle move given the current position
bool CastlePseudoLegal(Position* pos, Color color, int side) {

    uint8_t castle = color == WHITE ? side & WHITE_CASTLE
        : side & BLACK_CASTLE;

    Square kingSq = color == WHITE ? E1 : E8;

    Square rookSq = side == OO ? kingSq + 3 * east
        : kingSq + 4 * west;

    Square midway = side == OO ? kingSq + east
        : kingSq + west;

    Bitboard blocking = BetweenBB[kingSq][rookSq];

    return (pos->castlingRights & castle)
        && !(pieceBB(ALL) & blocking)
        && !SqAttacked(kingSq, !color, pos)
        && !SqAttacked(midway, !color, pos);
}

Bitboard PseudoAttacks[TYPE_NB][64];
Bitboard PawnAttacks[2][64];

uint8_t SqDistance[64][64];

// Initialize distance lookup table
void InitDistance() {

    for (Square sq1 = A1; sq1 <= H8; ++sq1)
        for (Square sq2 = A1; sq2 <= H8; ++sq2) {
            int vertical = abs(RankOf(sq1) - RankOf(sq2));
            int horizontal = abs(FileOf(sq1) - FileOf(sq2));
            SqDistance[sq1][sq2] = MAX(vertical, horizontal);
        }
}

// Returns the distance between two squares
int Distance(const Square sq1, const Square sq2) {
    return SqDistance[sq1][sq2];
}

// Initializes evaluation bit masks
static void InitEvalMasks() {

    // For each square a pawn can be on
    for (Square sq = A2; sq <= H7; ++sq) {

        IsolatedMask[sq] = AdjacentFilesBB(sq);

        PassedMask[WHITE][sq] = ShiftBB(north * RelativeRank(WHITE, RankOf(sq)), ~rank1BB)
            & (FileBB[FileOf(sq)] | AdjacentFilesBB(sq));

        PassedMask[BLACK][sq] = ShiftBB(south * RelativeRank(BLACK, RankOf(sq)), ~rank8BB)
            & (FileBB[FileOf(sq)] | AdjacentFilesBB(sq));
    }
}

// Helper function that returns a bitboard with the landing square of
// the step, or an empty bitboard if the step would go outside the board
Bitboard LandingSquareBB(const Square sq, const int step) {

    const Square to = sq + step;
    return (Bitboard)(to <= H8 && Distance(sq, to) <= 2) << to;
}

// Initializes non-slider attack lookups
static void InitNonSliderAttacks() {

    int KSteps[8] = { -9, -8, -7, -1,  1,  7,  8,  9 };
    int NSteps[8] = { -17,-15,-10, -6,  6, 10, 15, 17 };
    int PSteps[2][2] = { { -9, -7 }, { 7, 9 } };

    for (Square sq = A1; sq <= H8; ++sq) {

        // Kings and knights
        for (int i = 0; i < 8; ++i) {
            PseudoAttacks[KING][sq] |= LandingSquareBB(sq, KSteps[i]);
            PseudoAttacks[KNIGHT][sq] |= LandingSquareBB(sq, NSteps[i]);
        }

        // Pawns
        for (int i = 0; i < 2; ++i) {
            PawnAttacks[WHITE][sq] |= LandingSquareBB(sq, PSteps[WHITE][i]);
            PawnAttacks[BLACK][sq] |= LandingSquareBB(sq, PSteps[BLACK][i]);
        }
    }
}

Bitboard SquareBB[64];

// Initializes all bitboard lookups
static void InitBitMasks() {

    for (Square sq = A1; sq <= H8; ++sq)
        SquareBB[sq] = (1ULL << sq);

    InitDistance();

    InitEvalMasks();

    InitNonSliderAttacks();
}

// Returns the attack bitboard for a piece of piecetype on square sq
Bitboard AttackBB(PieceType piecetype, Square sq, Bitboard occupied) {

    switch (piecetype) {
    case BISHOP: return BishopAttackBB(sq, occupied);
    case ROOK: return RookAttackBB(sq, occupied);
    case QUEEN: return RookAttackBB(sq, occupied) | BishopAttackBB(sq, occupied);
    default: return PseudoAttacks[piecetype][sq];
    }
}

static void InitBetweenBB() {
    for (Square sq1 = A1; sq1 <= H8; sq1++)
        for (Square sq2 = A1; sq2 <= H8; sq2++)
            for (PieceType pt = BISHOP; pt <= ROOK; pt++)
                if (AttackBB(pt, sq1, SquareBB[sq2]) & SquareBB[sq2])
                    BetweenBB[sq1][sq2] = AttackBB(pt, sq1, SquareBB[sq2]) & AttackBB(pt, sq2, SquareBB[sq1]);
}

// Returns an attack bitboard where sliders are allowed to xray other sliders moving the same directions
Bitboard XRayAttackBB(Position* pos, const Color color, const PieceType pt, const Square sq) {
    Bitboard occ = pieceBB(ALL);
    switch (pt) {
    case BISHOP: occ ^= pieceBB(QUEEN) ^ colorPieceBB(color, BISHOP); break;
    case ROOK: occ ^= pieceBB(QUEEN) ^ colorPieceBB(color, ROOK); break;
    case QUEEN: occ ^= pieceBB(QUEEN) ^ colorPieceBB(color, ROOK) ^ colorPieceBB(color, BISHOP); break;
    }
    return AttackBB(pt, sq, occ);
}

// Returns the attack bitboard for a pawn
Bitboard PawnAttackBB(Color color, Square sq) {

    return PawnAttacks[color][sq];
}

// Returns the combined attack bitboard of all pawns in the given bitboard
Bitboard PawnBBAttackBB(Bitboard pawns, Color color) {

    const Direction up = (color == WHITE ? north : south);

    return ShiftBB(up + west, pawns) | ShiftBB(up + east, pawns);
}

// Checks whether a king is attacked
bool KingAttacked(Position* pos, Color color) {

    return SqAttacked(Lsb(colorPieceBB(color, KING)), !color, pos);
}

const int NonPawn[PIECE_NB] = {
    false, false,  true,  true,  true,  true, false, false,
    false, false,  true,  true,  true,  true, false, false
};

// Zobrist key tables
uint64_t PieceKeys[PIECE_NB][64];
uint64_t CastleKeys[16];
uint64_t SideKey;


// Pseudo-random number generator
static uint64_t Rand64() {

    // http://vigna.di.unimi.it/ftp/papers/xorshift.pdf

    static uint64_t seed = 1070372ull;

    seed ^= seed >> 12;
    seed ^= seed << 25;
    seed ^= seed >> 27;

    return seed * 2685821657736338717ull;
}

// Inits zobrist key tables
InitHashKeys() {

    // Side to play
    SideKey = Rand64();

    // En passant
    for (Square sq = A3; sq <= H6; ++sq)
        PieceKeys[0][sq] = Rand64();

    // White pieces
    for (Piece piece = wP; piece <= wK; ++piece)
        for (Square sq = A1; sq <= H8; ++sq)
            PieceKeys[piece][sq] = Rand64();

    // Black pieces
    for (Piece piece = bP; piece <= bK; ++piece)
        for (Square sq = A1; sq <= H8; ++sq)
            PieceKeys[piece][sq] = Rand64();

    // Castling rights
    for (int i = 0; i < 16; ++i)
        CastleKeys[i] = Rand64();
}

// Generates a hash key for the position. During
// a search this is incrementally updated instead.
static Key GeneratePosKey(Position* pos) {

    Key posKey = 0;

    // Pieces
    for (Square sq = A1; sq <= H8; ++sq) {
        Piece piece = pieceOn(sq);
        if (piece != EMPTY)
            posKey ^= PieceKeys[piece][sq];
    }

    // Side to play
    if (sideToMove == WHITE)
        posKey ^= SideKey;

    // En passant
    posKey ^= PieceKeys[EMPTY][pos->epSquare];

    // Castling rights
    posKey ^= CastleKeys[pos->castlingRights];

    return posKey;
}

// Calculates the position key after a move. Fails
// for special moves.
Key KeyAfter(Position* pos, Move move) {

    Square from = fromSq(move);
    Square to = toSq(move);
    Piece piece = pieceOn(from);
    Piece capt = capturing(move);
    Key key = pos->key ^ SideKey;

    if (capt)
        key ^= PieceKeys[capt][to];

    return key ^ PieceKeys[piece][from] ^ PieceKeys[piece][to];
}

// Clears the board
static void ClearPosition(Position* pos) {

    memset(pos, 0, sizeof(Position));
}

static const int MidGame = 256;

// Calculates the phase from the phase values of the pieces left
static inline int UpdatePhase(int value) {

    return (value * MidGame + 12) / 24;
}

int PSQT[PIECE_NB][64];
const int PhaseValue[PIECE_NB];

// Update the rest of a position to match pos->board
static void UpdatePosition(Position* pos) {

    // Loop through each square on the board
    for (Square sq = A1; sq <= H8; ++sq) {

        Piece piece = pieceOn(sq);

        // If it isn't empty we update the relevant lists
        if (piece != EMPTY) {

            Color color = ColorOf(piece);
            PieceType pt = PieceTypeOf(piece);

            // Bitboards
            pieceBB(ALL) |= SquareBB[sq];
            pieceBB(pt) |= SquareBB[sq];
            colorBB(color) |= SquareBB[sq];

            // Non pawn piece count
            pos->nonPawnCount[color] += NonPawn[piece];

            // Material score
            pos->material += PSQT[piece][sq];

            // Phase
            pos->phaseValue += PhaseValue[piece];
        }
    }

    pos->phase = UpdatePhase(pos->phaseValue);
}

// Translates a move to a string
char* BoardToFen(Position* pos) {

    const char PceChar[] = ".pnbrqk..PNBRQK";
    static char fen[100];
    char* ptr = fen;

    // Board
    for (int rank = RANK_8; rank >= RANK_1; --rank) {

        int count = 0;

        for (int file = FILE_A; file <= FILE_H; ++file) {
            Square sq = (rank * 8) + file;
            Piece piece = pieceOn(sq);

            if (piece) {
                if (count)
                    *ptr++ = '0' + count;
                *ptr++ = PceChar[piece];
                count = 0;
            }
            else
                count++;
        }

        if (count)
            *ptr++ = '0' + count;

        *ptr++ = rank == RANK_1 ? ' ' : '/';
    }

    // Side to move
    *ptr++ = sideToMove == WHITE ? 'w' : 'b';
    *ptr++ = ' ';

    // Castling rights
    int cr = pos->castlingRights;
    if (!cr)
        *ptr++ = '-';
    else {
        if (cr & WHITE_OO)  *ptr++ = 'K';
        if (cr & WHITE_OOO) *ptr++ = 'Q';
        if (cr & BLACK_OO)  *ptr++ = 'k';
        if (cr & BLACK_OOO) *ptr++ = 'q';
    }

    // En passant square in a separate string
    char ep[3] = "-";
    if (pos->epSquare)
        ep[0] = 'a' + FileOf(pos->epSquare),
        ep[1] = '1' + RankOf(pos->epSquare);

    // Add en passant, 50mr and game ply to the base
    sprintf(ptr, " %s %d %d", ep, pos->rule50, pos->gameMoves);

    return fen;
}

void PrintBoard(Position* pos) {

    const char PceChar[] = ".pnbrqk..PNBRQK";

    // Print board
    printf("\n");
    for (int rank = RANK_8; rank >= RANK_1; --rank) {
        for (int file = FILE_A; file <= FILE_H; ++file) {
            Square sq = (rank * 8) + file;
            printf("%3c", PceChar[pieceOn(sq)]);
        }
        printf("\n");
    }
    printf("\n");

    // Print FEN and zobrist key
    puts(BoardToFen(pos));
    printf("Zobrist Key: %" PRIu64 "\n\n", pos->key);
    fflush(stdout);
}

void MirrorBoard(Position* pos) {

    // Save the necessary position info mirrored
    uint8_t board[64];
    for (Square sq = A1; sq <= H8; ++sq)
        board[sq] = MirrorPiece(pieceOn(MirrorSquare(sq)));

    Color stm = !sideToMove;
    Square ep = pos->epSquare == 0 ? 0 : MirrorSquare(pos->epSquare);
    uint8_t cr = (pos->castlingRights & WHITE_CASTLE) << 2
        | (pos->castlingRights & BLACK_CASTLE) >> 2;

    // Clear the position
    ClearPosition(pos);

    // Fill in the mirrored position info
    for (Square sq = A1; sq <= H8; ++sq)
        pieceOn(sq) = board[sq];

    sideToMove = stm;
    pos->epSquare = ep;
    pos->castlingRights = cr;

    // Update the rest of the position to match pos->board
    UpdatePosition(pos);

    // Generate the position key
    pos->key = GeneratePosKey(pos);
}

#define MakeScore(mg, eg) ((int)((unsigned int)(eg) << 16) + (mg))
#define S(mg, eg) MakeScore((mg), (eg))
#define MgScore(s) ((int16_t)((uint16_t)((unsigned)((s)))))
#define EgScore(s) ((int16_t)((uint16_t)((unsigned)((s) + 0x8000) >> 16)))

// Piecetype values, combines with PSQTs [piecetype]
const int PieceTypeValue[7] = {
    0, S(P_MG, P_EG), S(N_MG, N_EG), S(B_MG, B_EG), S(R_MG, R_EG), S(Q_MG, Q_EG), 0
};

// Phase piece values, lookup used for futility pruning [phase][piece]
const int PieceValue[COLOR_NB][PIECE_NB] = {
    { 0, P_MG, N_MG, B_MG, R_MG, Q_MG, 0, 0,
      0, P_MG, N_MG, B_MG, R_MG, Q_MG, 0, 0 },
    { 0, P_EG, N_EG, B_EG, R_EG, Q_EG, 0, 0,
      0, P_EG, N_EG, B_EG, R_EG, Q_EG, 0, 0 }
};

// Phase value for each piece [piece]
const int PhaseValue[PIECE_NB] = {
    0, 0, 1, 1, 2, 4, 0, 0,
    0, 0, 1, 1, 2, 4, 0, 0
};

// Bonus for being the side to move
const int Tempo = 15;

// Misc bonuses and maluses
const int PawnDoubled = S(-11, -39);
const int PawnIsolated = S(-9, -12);
const int PawnSupport = S(18, 11);
const int PawnThreat = S(60, 44);
const int PushThreat = S(18, -2);
const int PawnOpen = S(-13, -9);
const int BishopPair = S(26, 102);
const int KingAtkPawn = S(48, 77);
const int OpenForward = S(30, 28);
const int SemiForward = S(9, 22);
const int NBBehindPawn = S(9, 36);

// Passed pawn
const int PawnPassed[8] = {
    S(0,  0), S(-10, 20), S(-12, 29), S(-8, 66),
    S(18,102), S(26,182), S(132,245), S(0,  0),
};

// Pawn phalanx
const int PawnPhalanx[8] = {
    S(0,  0), S(6,  6), S(18,  9), S(24, 25),
    S(62, 95), S(106,204), S(164,255), S(0,  0),
};

// KingLineDanger
const int KingLineDanger[28] = {
    S(0,  0), S(0,  0), S(0,  0), S(-12, 40),
    S(-32, 44), S(-39, 37), S(-38, 34), S(-42, 41),
    S(-50, 43), S(-66, 48), S(-65, 45), S(-75, 52),
    S(-78, 49), S(-84, 50), S(-87, 48), S(-78, 44),
    S(-75, 40), S(-65, 31), S(-58, 25), S(-56, 17),
    S(-54, 12), S(-61,  4), S(-67, -3), S(-85,-18),
    S(-101,-27), S(-115,-40), S(-118,-34), S(-123,-36),
};

// Mobility [pt-2][mobility]
const int Mobility[4][28] = {
    // Knight (0-8)
    { S(-45,-100), S(-25,-44), S(-5, 11), S(9, 27), S(19, 37), S(22, 55), S(29, 55), S(38, 51),
      S(50, 29) },
      // Bishop (0-13)
      { S(-48,-129), S(-23,-76), S(-6,-13), S(5, 12), S(14, 25), S(23, 48), S(28, 63), S(28, 69),
        S(28, 77), S(33, 77), S(37, 75), S(54, 65), S(53, 66), S(81, 39) },
        // Rook (0-14)
        { S(-79,-92), S(-16,-73), S(-3,-11), S(-1,  6), S(2, 37), S(6, 48), S(4, 68), S(11, 68),
          S(17, 74), S(24, 77), S(31, 81), S(31, 85), S(29, 89), S(36, 82), S(70, 60) },
          // Queen (0-27)
          { S(-62,-48), S(-84,-49), S(-68,-91), S(-24,-100), S(-5,-92), S(7,-59), S(12,-14), S(16, 18),
            S(20, 42), S(24, 55), S(27, 67), S(30, 76), S(33, 79), S(33, 88), S(34, 95), S(35, 98),
            S(33,107), S(30,112), S(25,119), S(25,119), S(32,115), S(38,111), S(49,102), S(66, 82),
            S(77, 65), S(89, 61), S(95, 96), S(98,120) }
};

// KingSafety [pt-2]
const int AttackPower[4] = { 35, 20, 40, 80 };
const int CheckPower[4] = { 100, 35, 65, 65 };
const int CountModifier[8] = { 0, 0, 64, 96, 113, 120, 124, 128 };

// Fills a bitboard in either vertical direction
Bitboard Fill(Bitboard bb, const Direction dir) {
    bb |= ShiftBB(dir, bb);
    bb |= ShiftBB(dir * 2, bb);
    bb |= ShiftBB(dir * 4, bb);
    return bb;
}

// Evaluates pawns
int EvalPawns(Position* pos, const Color color) {

    const Direction down = color == WHITE ? south : north;

    int eval = 0, count;

    Bitboard pawns = colorPieceBB(color, PAWN);

    // Doubled pawns (only when one is blocking the other from moving)
    count = bitCount(pawns & ShiftBB(north, pawns));
    eval += PawnDoubled * count;

    // Pawns defending pawns
    count = bitCount(pawns & PawnBBAttackBB(pawns, !color));
    eval += PawnSupport * count;

    // Open pawns
    Bitboard open = ~Fill(colorPieceBB(!color, PAWN), down);
    count = bitCount(pawns & open & ~PawnBBAttackBB(pawns, color));
    eval += PawnOpen * count;

    // Phalanx
    Bitboard phalanx = pawns & ShiftBB(west, pawns);
    while (phalanx) {
        int rank = RelativeRank(color, RankOf(PopLsb(&phalanx)));
        eval += PawnPhalanx[rank];
    }

    // Evaluate each individual pawn
    while (pawns) {

        Square sq = PopLsb(&pawns);

        // Isolated pawns
        if (!(IsolatedMask[sq] & colorPieceBB(color, PAWN))) {
            eval += PawnIsolated;
        }

        // Passed pawns
        if (!((PassedMask[color][sq]) & colorPieceBB(!color, PAWN))) {
            eval += PawnPassed[RelativeRank(color, RankOf(sq))];
        }
    }

    return eval;
}

typedef struct EvalInfo {

    Bitboard mobilityArea[2];
    Bitboard kingZone[2];
    int16_t attackPower[2];
    int16_t attackCount[2];

} EvalInfo;

// Evaluates knights, bishops, rooks, or queens
int EvalPiece(Position* pos, EvalInfo* ei, const Color color, const PieceType pt) {

    const Direction up = color == WHITE ? north : south;
    const Direction down = color == WHITE ? south : north;

    int eval = 0;

    Bitboard pieces = colorPieceBB(color, pt);

    // Bishop pair
    if (pt == BISHOP && Multiple(pieces)) {
        eval += BishopPair;
    }

    // Minor behind pawn
    if (pt == KNIGHT || pt == BISHOP) {
        int count = bitCount(pieces & ShiftBB(down, pieceBB(PAWN)));
        eval += count * NBBehindPawn;
    }

    // Evaluate each individual piece
    while (pieces) {

        Square sq = PopLsb(&pieces);

        // Mobility
        //int mob = bitCount(AttackBB(pt, sq, pieceBB(ALL)) & ei->mobilityArea[color]);
        Bitboard mobilityBB = XRayAttackBB(pos, color, pt, sq) & ei->mobilityArea[color];
        int mob = bitCount(mobilityBB);
        eval += Mobility[pt - 2][mob];

        // Attacks for king safety calculations
        int attacks = bitCount(mobilityBB & ei->kingZone[!color]);
        int checks = bitCount(mobilityBB & AttackBB(pt, kingSq(!color), pieceBB(ALL)));

        if (attacks > 0 || checks > 0) {
            ei->attackCount[color]++;
            ei->attackPower[color] += attacks * AttackPower[pt - 2]
                + checks * CheckPower[pt - 2];
        }

        // Forward mobility for rooks
        if (pt == ROOK) {
            Bitboard forward = Fill(BB(sq), up);
            if (!(forward & pieceBB(PAWN))) {
                eval += OpenForward;
            }
            else if (!(forward & colorPieceBB(color, PAWN))) {
                eval += SemiForward;
            }
        }
    }

    return eval;
}

// Evaluates kings
int EvalKings(Position* pos, EvalInfo* ei, const Color color) {

    int eval = 0;

    Square kingSq = kingSq(color);

    // Open lines from the king
    Bitboard SafeLine = RankBB[RelativeRank(color, RANK_1)];
    int count = bitCount(~SafeLine & AttackBB(QUEEN, kingSq, colorBB(color) | pieceBB(PAWN)));
    eval += KingLineDanger[count];

    // King threatening a pawn
    if (AttackBB(KING, kingSq, 0) & colorPieceBB(!color, PAWN)) {
        eval += KingAtkPawn;
    }

    // Add to enemy's attack power based on open lines
    ei->attackPower[!color] += (count - 3) * 8;

    int danger = ei->attackPower[!color]
        * CountModifier[MIN(7, ei->attackCount[!color])];

    eval -= S(danger / 128, 0);

    return eval;
}

int EvalPieces(Position* pos, EvalInfo* ei) {

    return  EvalPawns(pos, WHITE)
        - EvalPawns(pos, BLACK)
        + EvalPiece(pos, ei, WHITE, KNIGHT)
        - EvalPiece(pos, ei, BLACK, KNIGHT)
        + EvalPiece(pos, ei, WHITE, BISHOP)
        - EvalPiece(pos, ei, BLACK, BISHOP)
        + EvalPiece(pos, ei, WHITE, ROOK)
        - EvalPiece(pos, ei, BLACK, ROOK)
        + EvalPiece(pos, ei, WHITE, QUEEN)
        - EvalPiece(pos, ei, BLACK, QUEEN)
        + EvalKings(pos, ei, WHITE)
        - EvalKings(pos, ei, BLACK);
}

// Evaluates threats
int EvalThreats(Position* pos, const Color color) {

    const Direction up = color == WHITE ? north : south;

    int count, eval = 0;

    Bitboard ourPawns = colorPieceBB(color, PAWN);
    Bitboard theirNonPawns = colorBB(!color) ^ colorPieceBB(!color, PAWN);

    count = bitCount(PawnBBAttackBB(ourPawns, color) & theirNonPawns);
    eval += PawnThreat * count;

    Bitboard pawnPushes = ShiftBB(up, ourPawns) & ~pieceBB(ALL);

    count = bitCount(PawnBBAttackBB(pawnPushes, color) & theirNonPawns);
    eval += PushThreat * count;

    return eval;
}

// Initializes the eval info struct
void InitEvalInfo(Position* pos, EvalInfo* ei, const Color color) {

    const Direction up = (color == WHITE ? north : south);
    const Direction down = (color == WHITE ? south : north);

    Bitboard b, pawns = colorPieceBB(color, PAWN);

    // Mobility area is defined as any square not attacked by an enemy pawn, nor
    // occupied by our own pawn either on its starting square or blocked from advancing.
    b = pawns & (RankBB[RelativeRank(color, RANK_2)] | ShiftBB(down, pieceBB(ALL)));
    ei->mobilityArea[color] = ~(b | PawnBBAttackBB(colorPieceBB(!color, PAWN), !color));

    // King Safety
    ei->kingZone[color] = AttackBB(KING, kingSq(color), 0);

    ei->attackPower[color] = -30;
    ei->attackCount[color] = 0;
}

#define BlackSquaresBB 0xAA55AA55AA55AA55

// Calculate scale factor to lower overall eval based on various features
int ScaleFactor(Position* pos, const int eval) {

    // Scale down eval for opposite-colored bishops endgames
    if (!pieceBB(QUEEN) && !pieceBB(ROOK) && !pieceBB(KNIGHT)
        && pos->nonPawnCount[WHITE] == 1
        && pos->nonPawnCount[BLACK] == 1
        && (Single(pieceBB(BISHOP) & BlackSquaresBB)))
        return 64;

    // Scale down eval the fewer pawns the stronger side has
    Color strong = eval > 0 ? WHITE : BLACK;
    int strongPawnCount = bitCount(colorPieceBB(strong, PAWN));
    int x = 8 - strongPawnCount;
    return 128 - x * x;
}

// Calculate a static evaluation of a position
int EvalPosition(Position* pos) {

    //if (MaterialDraw(pos)) return 0;

    EvalInfo ei;

    InitEvalInfo(pos, &ei, WHITE);
    InitEvalInfo(pos, &ei, BLACK);

    // Material (includes PSQT)
    int eval = pos->material;

    // Evaluate pieces
    eval += EvalPieces(pos, &ei);

    // Evaluate threats
    eval += EvalThreats(pos, WHITE)
        - EvalThreats(pos, BLACK);

    // Adjust eval by scale factor
    int scale = ScaleFactor(pos, eval);

    // Adjust score by phase
    eval = ((MgScore(eval) * pos->phase)
        + (EgScore(eval) * (MidGame - pos->phase)) * scale / 128)
        / MidGame;

    // Return the evaluation, negated if we are black
    return (sideToMove == WHITE ? eval : -eval) + Tempo;
}

#define HASH_PCE(piece, sq) (pos->key ^= PieceKeys[(piece)][(sq)])
#define HASH_CA             (pos->key ^= CastleKeys[pos->castlingRights])
#define HASH_SIDE           (pos->key ^= SideKey)
#define HASH_EP             (pos->key ^= PieceKeys[EMPTY][pos->epSquare])


static const uint8_t CastlePerm[64] = {
    13, 15, 15, 15, 12, 15, 15, 14,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
     7, 15, 15, 15,  3, 15, 15, 11
};


// Remove a piece from a square sq
static void ClearPiece(Position* pos, const Square sq, const bool hash) {

    const Piece piece = pieceOn(sq);
    const Color color = ColorOf(piece);
    const PieceType pt = PieceTypeOf(piece);

    // Hash out the piece
    if (hash)
        HASH_PCE(piece, sq);

    // Set square to empty
    pieceOn(sq) = EMPTY;

    // Update material
    pos->material -= PSQT[piece][sq];

    // Update phase
    pos->phaseValue -= PhaseValue[piece];
    pos->phase = UpdatePhase(pos->phaseValue);

    // Update non-pawn count
    pos->nonPawnCount[color] -= NonPawn[piece];

    // Update bitboards
    pieceBB(ALL) ^= SquareBB[sq];
    pieceBB(pt) ^= SquareBB[sq];
    colorBB(color) ^= SquareBB[sq];
}

// Add a piece piece to a square
static void AddPiece(Position* pos, const Square sq, const Piece piece, const bool hash) {

    const Color color = ColorOf(piece);
    const PieceType pt = PieceTypeOf(piece);

    // Hash in piece at square
    if (hash)
        HASH_PCE(piece, sq);

    // Update square
    pieceOn(sq) = piece;

    // Update material
    pos->material += PSQT[piece][sq];

    // Update phase
    pos->phaseValue += PhaseValue[piece];
    pos->phase = UpdatePhase(pos->phaseValue);

    // Update non-pawn count
    pos->nonPawnCount[color] += NonPawn[piece];

    // Update bitboards
    pieceBB(ALL) |= SquareBB[sq];
    pieceBB(pt) |= SquareBB[sq];
    colorBB(color) |= SquareBB[sq];
}

// Move a piece from one square to another
static void MovePiece(Position* pos, const Square from, const Square to, const bool hash) {

    const Piece piece = pieceOn(from);
    const Color color = ColorOf(piece);
    const PieceType pt = PieceTypeOf(piece);

    // Hash out piece on old square, in on new square
    if (hash)
        HASH_PCE(piece, from),
        HASH_PCE(piece, to);

    // Set old square to empty, new to piece
    pieceOn(from) = EMPTY;
    pieceOn(to) = piece;

    // Update material
    pos->material += PSQT[piece][to] - PSQT[piece][from];

    // Update bitboards
    pieceBB(ALL) ^= SquareBB[from] ^ SquareBB[to];
    pieceBB(pt) ^= SquareBB[from] ^ SquareBB[to];
    colorBB(color) ^= SquareBB[from] ^ SquareBB[to];
}

// Take back the previous move
void TakeMove(Position* pos) {

    // Decrement histPly, ply
    pos->histPly--;
    pos->ply--;

    // Change side to play
    sideToMove ^= 1;

    // Get the move from history
    const Move move = history(0).move;
    const Square from = fromSq(move);
    const Square to = toSq(move);

    // Add in pawn captured by en passant
    if (moveIsEnPas(move))
        AddPiece(pos, to ^ 8, MakePiece(!sideToMove, PAWN), false);

    // Move rook back if castling
    else if (moveIsCastle(move))
        switch (to) {
        case C1: MovePiece(pos, D1, A1, false); break;
        case C8: MovePiece(pos, D8, A8, false); break;
        case G1: MovePiece(pos, F1, H1, false); break;
        default: MovePiece(pos, F8, H8, false); break;
        }

    // Make reverse move (from <-> to)
    MovePiece(pos, to, from, false);

    // Add back captured piece if any
    Piece capt = capturing(move);
    if (capt != EMPTY) {
        AddPiece(pos, to, capt, false);
    }

    // Remove promoted piece and put back the pawn
    Piece promo = promotion(move);
    if (promo != EMPTY) {
        ClearPiece(pos, from, false);
        AddPiece(pos, from, MakePiece(sideToMove, PAWN), false);
    }

    // Get various info from history
    pos->key = history(0).posKey;
    pos->epSquare = history(0).epSquare;
    pos->rule50 = history(0).rule50;
    pos->castlingRights = history(0).castlingRights;
}

// Make a move - take it back and return false if move was illegal
bool MakeMove(Position* pos, Move move) {

    // Save position
    history(0).posKey = pos->key;
    history(0).move = move;
    history(0).epSquare = pos->epSquare;
    history(0).rule50 = pos->rule50;
    history(0).castlingRights = pos->castlingRights;

    // Increment histPly, ply and 50mr
    pos->histPly++;
    pos->ply++;
    pos->rule50++;

    // Hash out en passant if there was one, and unset it
    HASH_EP;
    pos->epSquare = 0;

    const Square from = fromSq(move);
    const Square to = toSq(move);

    // Rehash the castling rights
    HASH_CA;
    pos->castlingRights &= CastlePerm[from] & CastlePerm[to];
    HASH_CA;

    // Remove captured piece if any
    Piece capt = capturing(move);
    if (capt != EMPTY) {
        ClearPiece(pos, to, true);
        pos->rule50 = 0;
    }

    // Move the piece
    MovePiece(pos, from, to, true);

    // Pawn move specifics
    if (PieceTypeOf(pieceOn(to)) == PAWN) {

        pos->rule50 = 0;
        Piece promo = promotion(move);

        // Set en passant square if applicable
        if (moveIsPStart(move)) {
            if ((PawnAttackBB(sideToMove, to ^ 8)
                & colorPieceBB(!sideToMove, PAWN))) {

                pos->epSquare = to ^ 8;
                HASH_EP;
            }

            // Remove pawn captured by en passant
        }
        else if (moveIsEnPas(move))
            ClearPiece(pos, to ^ 8, true);

        // Replace promoting pawn with new piece
        else if (promo != EMPTY) {
            ClearPiece(pos, to, true);
            AddPiece(pos, to, promo, true);
        }

        // Move the rook during castling
    }
    else if (moveIsCastle(move))
        switch (to) {
        case C1: MovePiece(pos, A1, D1, true); break;
        case C8: MovePiece(pos, A8, D8, true); break;
        case G1: MovePiece(pos, H1, F1, true); break;
        default: MovePiece(pos, H8, F8, true); break;
        }

    // Change turn to play
    sideToMove ^= 1;
    HASH_SIDE;

    // If own king is attacked after the move, take it back immediately
    if (KingAttacked(pos, sideToMove ^ 1))
        return TakeMove(pos), false;

    return true;
}

// Pass the turn without moving
void MakeNullMove(Position* pos) {

    // Save misc info for takeback
    history(0).posKey = pos->key;
    history(0).move = NOMOVE;
    history(0).epSquare = pos->epSquare;
    history(0).rule50 = pos->rule50;
    history(0).castlingRights = pos->castlingRights;

    // Increase ply
    pos->ply++;
    pos->histPly++;

    pos->rule50 = 0;

    // Change side to play
    sideToMove ^= 1;
    HASH_SIDE;

    // Hash out en passant if there was one, and unset it
    HASH_EP;
    pos->epSquare = 0;
}

// Take back a null move
void TakeNullMove(Position* pos) {

    // Decrease ply
    pos->histPly--;
    pos->ply--;

    // Change side to play
    sideToMove ^= 1;

    // Get info from history
    pos->key = history(0).posKey;
    pos->epSquare = history(0).epSquare;
    pos->rule50 = history(0).rule50;
    pos->castlingRights = history(0).castlingRights;
}

// Checks whether a move is pseudo-legal (assuming it is pseudo-legal in some position)
bool MoveIsPseudoLegal(Position* pos, Move move) {

    if (!move) return false;

    const Color color = sideToMove;
    const Square from = fromSq(move);
    const Square to = toSq(move);

    // Must move our own piece to a square not occupied by our own pieces
    if (!(colorBB(color) & SquareBB[from])
        || (colorBB(color) & SquareBB[to])
        || capturing(move) != pieceOn(to))
        return false;

    // Castling
    if (moveIsCastle(move))
        switch (to) {
        case C1: return CastlePseudoLegal(pos, WHITE, OOO);
        case G1: return CastlePseudoLegal(pos, WHITE, OO);
        case C8: return CastlePseudoLegal(pos, BLACK, OOO);
        case G8: return CastlePseudoLegal(pos, BLACK, OO);
        default: return false;
        }

    // All non-pawn, non-castling moves
    if (PieceTypeOf(pieceOn(from)) != PAWN) {

        // No flags or promotion, and the piece currently attacks it
        return !moveIsSpecial(move)
            && SquareBB[to] & AttackBB(PieceTypeOf(pieceOn(from)), from, pieceBB(ALL));

        // Pawn moves
    }
    else {

        // En passant
        if (moveIsEnPas(move))
            return to == pos->epSquare
            && SquareBB[to] & PawnAttackBB(color, from);

        // Pawn start
        if (moveIsPStart(move))
            return pieceOn(to ^ 8) == EMPTY
            && (to + 16 - 32 * color) == from;

        // Pawn moves to the last rank must promote
        if (RelativeRank(color, RankOf(to)) == RANK_8
            && !promotion(move))
            return false;

        // Normal moves and promotions
        return (moveIsCapture(move)) ? SquareBB[to] & PawnAttackBB(color, from)
            : (to + 8 - 16 * color) == from;
    }
}

//hash table

TranspositionTable TT;

// Mate scores are stored as mate in 0 as they depend on the current ply
int ScoreToTT(const int score, const uint8_t ply) {

    return score >= ISMATE ? score + ply
        : score <= -ISMATE ? score - ply
        : score;
}

// Translates from mate in 0 to the proper mate score at current ply
int ScoreFromTT(const int score, const uint8_t ply) {

    return score >= ISMATE ? score - ply
        : score <= -ISMATE ? score + ply
        : score;
}

TTEntry* GetEntry(Key posKey) {

    // https://lemire.me/blog/2016/06/27/a-fast-alternative-to-the-modulo-reduction/
    return &TT.table[((uint32_t)posKey * (uint64_t)TT.count) >> 32];
}

// Clears the transposition table
void ClearTT() {

    if (!TT.dirty) return;

    memset(TT.table, 0, TT.count * sizeof(TTEntry));

    TT.dirty = false;
}

// Allocates memory for the transposition table
void InitTT() {

    // Ignore if already correct size
    if (TT.currentMB == TT.requestedMB)
        return;

    size_t MB = TT.requestedMB;

    TT.count = MB * 1024 * 1024 / sizeof(TTEntry);

    // Free memory if already allocated
    if (TT.currentMB > 0)
        free(TT.mem);

    // Allocate memory
    TT.mem = malloc(TT.count * sizeof(TTEntry) + 64 - 1);

    // Allocation failed
    if (!TT.mem) {
        printf("Allocating %" PRI_SIZET "MB for the transposition table failed.\n", MB);
        fflush(stdout);
    }

    TT.table = (TTEntry*)(((uintptr_t)TT.mem + 64 - 1) & ~(64 - 1));
    TT.currentMB = MB;

    // Ensure the memory is 0'ed out
    TT.dirty = true;
    ClearTT();

    printf("HashTable init complete with %" PRI_SIZET " entries, using %" PRI_SIZET "MB.\n", TT.count, MB);
    fflush(stdout);
}

// Probe the transposition table
TTEntry* ProbeTT(const Key posKey, bool* ttHit) {

    TTEntry* tte = GetEntry(posKey);

    *ttHit = tte->posKey == posKey;

    return tte;
}

// Store an entry in the transposition table
void StoreTTEntry(TTEntry* tte, const Key posKey, const Move move, const int score, const int depth, const int bound) {

    // Store new data unless it would overwrite data about the same
    // position searched to a higher depth.
    if (posKey != tte->posKey || depth >= tte->depth || bound == BOUND_EXACT)
        tte->posKey = posKey,
        tte->move = move,
        tte->score = score,
        tte->depth = depth,
        tte->bound = bound;
}

// Estimates the load factor of the transposition table (1 = 0.1%)
int HashFull() {

    int used = 0;
    const int samples = 1000;

    for (int i = 0; i < samples; ++i)
        if (TT.table[i].move != NOMOVE)
            used++;

    return used / (samples / 1000);
}

enum { QUIET, NOISY };

// Constructs and adds a move to the move list
void AddMove(Position* pos, MoveList* list, const Square from, const Square to, const Piece promo, const int flag) {

    list->moves[list->count++].move = MOVE(from, to, pieceOn(to), promo, flag);
}

// Adds promotions
void AddPromotions(Position* pos, MoveList* list, const Square from, const Square to, const Color color, const int type) {

    if (type == NOISY)
        AddMove(pos, list, from, to, MakePiece(color, QUEEN), FLAG_NONE);

    if (type == QUIET) {
        AddMove(pos, list, from, to, MakePiece(color, KNIGHT), FLAG_NONE);
        AddMove(pos, list, from, to, MakePiece(color, ROOK), FLAG_NONE);
        AddMove(pos, list, from, to, MakePiece(color, BISHOP), FLAG_NONE);
    }
}

// Castling is now a bit less of a mess
void GenCastling(Position* pos, MoveList* list, const Color color, const int type) {

    if (type != QUIET) return;

    const Square from = color == WHITE ? E1 : E8;

    // King side castle
    if (CastlePseudoLegal(pos, color, OO))
        AddMove(pos, list, from, from + 2, EMPTY, FLAG_CASTLE);

    // Queen side castle
    if (CastlePseudoLegal(pos, color, OOO))
        AddMove(pos, list, from, from - 2, EMPTY, FLAG_CASTLE);
}

// Pawns are a mess
void GenPawn(Position* pos, MoveList* list, const Color color, const int type) {

    const Direction up = color == WHITE ? north : south;
    const Direction left = color == WHITE ? west : east;
    const Direction right = color == WHITE ? east : west;

    const Bitboard empty = ~pieceBB(ALL);
    const Bitboard enemies = colorBB(!color);
    const Bitboard pawns = colorPieceBB(color, PAWN);

    const Bitboard on7th = pawns & RankBB[RelativeRank(color, RANK_7)];
    const Bitboard not7th = pawns ^ on7th;

    // Normal moves forward
    if (type == QUIET) {

        Bitboard pawnMoves = empty & ShiftBB(up, not7th);
        Bitboard pawnStarts = empty & ShiftBB(up, pawnMoves)
            & RankBB[RelativeRank(color, RANK_4)];

        // Normal pawn moves
        while (pawnMoves) {
            Square to = PopLsb(&pawnMoves);
            AddMove(pos, list, to - up, to, EMPTY, FLAG_NONE);
        }
        // Pawn starts
        while (pawnStarts) {
            Square to = PopLsb(&pawnStarts);
            AddMove(pos, list, to - up * 2, to, EMPTY, FLAG_PAWNSTART);
        }
    }

    // Promotions
    if (on7th) {

        Bitboard promotions = empty & ShiftBB(up, on7th);
        Bitboard lPromoCap = enemies & ShiftBB(up + left, on7th);
        Bitboard rPromoCap = enemies & ShiftBB(up + right, on7th);

        // Promoting captures
        while (lPromoCap) {
            Square to = PopLsb(&lPromoCap);
            AddPromotions(pos, list, to - (up + left), to, color, type);
        }
        while (rPromoCap) {
            Square to = PopLsb(&rPromoCap);
            AddPromotions(pos, list, to - (up + right), to, color, type);
        }
        // Promotions
        while (promotions) {
            Square to = PopLsb(&promotions);
            AddPromotions(pos, list, to - up, to, color, type);
        }
    }
    // Captures
    if (type == NOISY) {

        Bitboard lAttacks = enemies & ShiftBB(up + left, not7th);
        Bitboard rAttacks = enemies & ShiftBB(up + right, not7th);

        while (lAttacks) {
            Square to = PopLsb(&lAttacks);
            AddMove(pos, list, to - (up + left), to, EMPTY, FLAG_NONE);
        }
        while (rAttacks) {
            Square to = PopLsb(&rAttacks);
            AddMove(pos, list, to - (up + right), to, EMPTY, FLAG_NONE);
        }
        // En passant
        if (pos->epSquare) {
            Bitboard enPassers = not7th & PawnAttackBB(!color, pos->epSquare);
            while (enPassers)
                AddMove(pos, list, PopLsb(&enPassers), pos->epSquare, EMPTY, FLAG_ENPAS);
        }
    }
}

// Knight, bishop, rook, queen and king except castling
void GenPieceType(Position* pos, MoveList* list, const Color color, const int type, const PieceType pt) {

    const Bitboard occupied = pieceBB(ALL);
    const Bitboard enemies = colorBB(!color);
    const Bitboard targets = type == NOISY ? enemies : ~occupied;

    Bitboard pieces = colorPieceBB(color, pt);

    while (pieces) {

        Square from = PopLsb(&pieces);

        Bitboard moves = targets & AttackBB(pt, from, occupied);

        while (moves)
            AddMove(pos, list, from, PopLsb(&moves), EMPTY, FLAG_NONE);
    }
}

// Generate moves
static void GenMoves(Position* pos, MoveList* list, const Color color, const int type) {

    GenCastling(pos, list, color, type);
    GenPawn(pos, list, color, type);
    GenPieceType(pos, list, color, type, KNIGHT);
    GenPieceType(pos, list, color, type, ROOK);
    GenPieceType(pos, list, color, type, BISHOP);
    GenPieceType(pos, list, color, type, QUEEN);
    GenPieceType(pos, list, color, type, KING);
}

// Generate quiet moves
void GenQuietMoves(Position* pos, MoveList* list) {

    GenMoves(pos, list, sideToMove, QUIET);
}

// Generate noisy moves
void GenNoisyMoves(Position* pos, MoveList* list) {

    GenMoves(pos, list, sideToMove, NOISY);
}

static int MvvLvaScores[PIECE_NB][PIECE_NB];


// Initializes the MostValuableVictim-LeastValuableAttacker scores used for ordering captures
InitMvvLva() {

    const int VictimScore[PIECE_NB] = { 0, 106, 206, 306, 406, 506, 606, 0, 0, 106, 206, 306, 406, 506, 606, 0 };
    const int AttackerScore[PIECE_NB] = { 0,   1,   2,   3,   4,   5,   6, 0, 0,   1,   2,   3,   4,   5,   6, 0 };

    for (Piece Attacker = EMPTY; Attacker < PIECE_NB; ++Attacker)
        for (Piece Victim = EMPTY; Victim < PIECE_NB; ++Victim)
            MvvLvaScores[Victim][Attacker] = VictimScore[Victim] - AttackerScore[Attacker];
}

// Return the next best move
static Move PickNextMove(MovePicker* mp) {

    MoveList* list = mp->list;

    if (list->next == list->count)
        return NOMOVE;

    Move bestMove = list->moves[list->next++].move;

    // Avoid returning the TT or killer moves again
    if (bestMove == mp->ttMove || bestMove == mp->kill1 || bestMove == mp->kill2)
        return PickNextMove(mp);

    return bestMove;
}

static void SortMoves(MoveList* list, int threshold) {

    MoveListEntry* begin = &list->moves[list->next];
    MoveListEntry* end = &list->moves[list->count];

    for (MoveListEntry* sortedEnd = begin, *p = begin + 1; p < end; ++p) {
        if (p->score > threshold) {
            MoveListEntry tmp = *p, * q;
            *p = *++sortedEnd;
            for (q = sortedEnd; q != begin && (q - 1)->score < tmp.score; --q)
                *q = *(q - 1);
            *q = tmp;
        }
    }
}

// Gives a score to each move left in the list
static void ScoreMoves(MoveList* list, Position* pos, const int stage, uint8_t depth) {

    for (int i = list->next; i < list->count; ++i) {

        Move move = list->moves[i].move;

        if (stage == gEN_NOISY)
            list->moves[i].score = moveIsEnPas(move) ? 105
            : MvvLvaScores[pieceOn(toSq(move))][pieceOn(fromSq(move))];

        if (stage == gEN_QUIET)
            list->moves[i].score = pos->history[pieceOn(fromSq(move))][toSq(move)];
    }

    SortMoves(list, -1000 * depth);
}

// Returns a bitboard with all attackers of a square
Bitboard Attackers(Position* pos, const Square sq, const Bitboard occ) {

    const Bitboard bishops = pieceBB(BISHOP) | pieceBB(QUEEN);
    const Bitboard rooks = pieceBB(ROOK) | pieceBB(QUEEN);

    return (PawnAttackBB(WHITE, sq) & colorPieceBB(BLACK, PAWN))
        | (PawnAttackBB(BLACK, sq) & colorPieceBB(WHITE, PAWN))
        | (AttackBB(KNIGHT, sq, occ) & pieceBB(KNIGHT))
        | (AttackBB(KING, sq, occ) & pieceBB(KING))
        | (AttackBB(BISHOP, sq, occ) & bishops)
        | (AttackBB(ROOK, sq, occ) & rooks);
}

// Tapered Eval

// The game is split into two phases, midgame where all the non-pawns
// are still on the board, and endgame where only kings and pawns are
// left. There's a gliding transition between them, depending on how
// many of each piece type are left, represented by a phase value
// ranging from 256 (midgame) to 0 (endgame). This allows features to
// vary in importance depending on the situation on the board. Each
// feature is given two values, one for each phase. These are stored
// as a single integer and handled using the defines below.

enum Phase { MG, EG };

// Static Exchange Evaluation
bool SEE(Position* pos, const Move move, const int threshold) {

    if (moveIsSpecial(move))
        return true;

    Square to = toSq(move);
    Square from = fromSq(move);

    // Making the move and not losing it must beat the threshold
    int value = PieceValue[MG][pieceOn(to)] - threshold;
    if (value < 0) return false;

    // Trivial if we still beat the threshold after losing the piece
    value -= PieceValue[MG][pieceOn(from)];
    if (value >= 0) return true;

    Bitboard occupied = (pieceBB(ALL) ^ BB(from)) | BB(to);
    Bitboard attackers = Attackers(pos, to, occupied);

    Bitboard bishops = pieceBB(BISHOP) | pieceBB(QUEEN);
    Bitboard rooks = pieceBB(ROOK) | pieceBB(QUEEN);

    Color side = !ColorOf(pieceOn(from));

    // Make captures until one side runs out, or fail to beat threshold
    while (true) {

        // Remove used pieces from attackers
        attackers &= occupied;

        Bitboard myAttackers = attackers & colorBB(side);
        if (!myAttackers) break;

        // Pick next least valuable piece to capture with
        PieceType pt;
        for (pt = PAWN; pt < KING; ++pt)
            if (myAttackers & pieceBB(pt))
                break;

        side = !side;

        // Value beats threshold, or can't beat threshold (negamaxed)
        if ((value = -value - 1 - PieceValue[MG][pt]) >= 0) {

            if (pt == KING && (attackers & colorBB(side)))
                side = !side;

            break;
        }

        // Remove the used piece from occupied
        occupied ^= BB(Lsb(myAttackers & pieceBB(pt)));

        // Add possible discovered attacks from behind the used piece
        if (pt == PAWN || pt == BISHOP || pt == QUEEN)
            attackers |= AttackBB(BISHOP, to, occupied) & bishops;
        if (pt == ROOK || pt == QUEEN)
            attackers |= AttackBB(ROOK, to, occupied) & rooks;
    }

    return side != ColorOf(pieceOn(from));
}

// Returns the next move to try in a position
Move NextMove(MovePicker* mp) {

    Move move;

    // Switch on stage, falls through to the next stage
    // if a move isn't returned in the current stage.
    switch (mp->stage) {

    case tTMOVE:
        mp->stage++;
        return mp->ttMove;

        // fall through
    case gEN_NOISY:
        GenNoisyMoves(mp->pos, mp->list);
        ScoreMoves(mp->list, mp->pos, gEN_NOISY, mp->depth);
        mp->stage++;

        // fall through
    case nOISY_GOOD:
         while ((move = PickNextMove(mp)))
            if (mp->list->moves[mp->list->next - 1].score > 12000
                || (mp->list->moves[mp->list->next - 1].score > -8000 && SEE(mp->pos, move, 0)))
                return move;
            else
                mp->list->moves[mp->bads++].move = move;

        mp->stage++;

        // fall through
    case kILLER1:
        mp->stage++;
        if (mp->kill1 != mp->ttMove
            && MoveIsPseudoLegal(mp->pos, mp->kill1))
            return mp->kill1;

        // fall through
    case kILLER2:
        mp->stage++;
        if (mp->kill2 != mp->ttMove
            && MoveIsPseudoLegal(mp->pos, mp->kill2))
            return mp->kill2;

        // fall through
    case gEN_QUIET:
        if (mp->onlyNoisy)
            return NOMOVE;

        GenQuietMoves(mp->pos, mp->list);
        ScoreMoves(mp->list, mp->pos, gEN_QUIET, mp->depth);
        mp->stage++;

        // fall through
    case qUIET:
        if (!mp->onlyNoisy)
            if ((move = PickNextMove(mp)))
                return move;

        mp->stage++;
        mp->list->next = 0;
        mp->list->moves[mp->bads].move = NOMOVE;

        // fall through
    case nOISY_BAD:
        return mp->list->moves[mp->list->next++].move;

    default:
        return NOMOVE;
    }
}

// Init normal movepicker
void InitNormalMP(MovePicker* mp, MoveList* list, Position* pos, uint8_t depth, Move ttMove, Move kill1, Move kill2) {
    list->count = list->next = 0;
    mp->list = list;
    mp->pos = pos;
    mp->ttMove = MoveIsPseudoLegal(pos, ttMove) ? ttMove : NOMOVE;
    mp->stage = mp->ttMove ? tTMOVE : gEN_NOISY;
    mp->depth = depth;
    mp->kill1 = kill1;
    mp->kill2 = kill2;
    mp->bads = 0;
    mp->onlyNoisy = false;
}

// Init noisy movepicker
void InitNoisyMP(MovePicker* mp, MoveList* list, Position* pos) {
    InitNormalMP(mp, list, pos, 0, NOMOVE, NOMOVE, NOMOVE);
    mp->onlyNoisy = true;
}

const int PieceTypeValue[7];


int PSQT[PIECE_NB][64];

// Black's point of view - easier to read as it's not upside down
const int PieceSqValue[6][64] = {

    { S(0,  0), S(0,  0), S(0,  0), S(0,  0), S(0,  0), S(0,  0), S(0,  0), S(0,  0),
      S(52, 76), S(41, 71), S(44, 49), S(49,  1), S(44, -4), S(40, 16), S(-45, 71), S(-49, 79),
      S(16, 82), S(23, 81), S(51, 33), S(56,-16), S(72,-21), S(121,  4), S(82, 50), S(35, 58),
      S(-5, 40), S(-8, 26), S(-1,  7), S(11,-20), S(25,-19), S(34,-11), S(2, 14), S(4, 16),
      S(-14, 17), S(-22, 15), S(-8, -9), S(-8,-13), S(1,-13), S(4,-12), S(-8,  1), S(-5, -4),
      S(-22,  6), S(-28,  1), S(-24, -2), S(-19, -8), S(-9, -4), S(-11,  1), S(-2,-11), S(-7,-12),
      S(-9, 10), S(-10,  7), S(-11,  7), S(-6,  5), S(-4,  8), S(15,  7), S(24, -6), S(3,-21),
      S(0,  0), S(0,  0), S(0,  0), S(0,  0), S(0,  0), S(0,  0), S(0,  0), S(0,  0) },

    { S(-143,-93), S(-51,-30), S(-79,  7), S(-33,-11), S(-13, -5), S(-68,  1), S(-51,-23), S(-112,-89),
      S(-15,-22), S(-9,  0), S(34, -8), S(50,  5), S(38, -3), S(58,-26), S(-24, -2), S(-18,-30),
      S(-17,-12), S(26, -1), S(26, 39), S(46, 35), S(80, 13), S(52, 19), S(31,-11), S(-18,-20),
      S(7, -2), S(21, 16), S(40, 41), S(35, 50), S(27, 44), S(52, 32), S(22, 12), S(23,-11),
      S(2,  4), S(18, 12), S(20, 42), S(24, 42), S(23, 44), S(31, 32), S(41,  9), S(30,  2),
      S(-25,-28), S(-8,  0), S(-6, 14), S(5, 30), S(9, 26), S(2,  7), S(5, -5), S(-3,-22),
      S(-33,-14), S(-37, -4), S(-24, -7), S(-9,  8), S(-13,  3), S(-22, -9), S(-29,-12), S(-10, -1),
      S(-57,-42), S(-20,-28), S(-21,-16), S(-11,  2), S(-4,  6), S(-9,-20), S(-18,-13), S(-42,-40) },

    { S(-36, 31), S(-38, 20), S(-66, 22), S(-73, 27), S(-67, 23), S(-90, 13), S(-24, 11), S(-39, 11),
      S(-25, 13), S(15, 14), S(4, 15), S(-18, 19), S(0, 10), S(-15, 16), S(-19, 20), S(-60, 22),
      S(3, 16), S(28, 15), S(45,  9), S(38,  4), S(35,  4), S(54, 14), S(16, 14), S(11,  3),
      S(-3, 10), S(39, 11), S(31, 10), S(48, 29), S(47, 16), S(30, 15), S(46,  0), S(-4, 10),
      S(11, -7), S(12,  7), S(20, 21), S(35, 22), S(31, 22), S(24, 11), S(16,  8), S(36,-14),
      S(9,-11), S(27,  9), S(17, 12), S(14, 22), S(17, 20), S(22, 10), S(32, -1), S(30,-11),
      S(21,-11), S(17,-26), S(13,-12), S(-1,  6), S(0,  5), S(4, -9), S(20,-23), S(21,-37),
      S(23, -7), S(23,  3), S(12,  9), S(2,  2), S(12,  1), S(7,  8), S(15,  0), S(27,-25) },

    { S(28, 31), S(21, 39), S(-1, 55), S(1, 47), S(6, 42), S(8, 43), S(24, 35), S(30, 34),
      S(-5, 39), S(-15, 54), S(5, 55), S(13, 51), S(1, 46), S(17, 28), S(-8, 35), S(8, 27),
      S(-3, 36), S(39, 23), S(22, 34), S(44, 13), S(61,  4), S(47,  8), S(71,  0), S(23, 12),
      S(-3, 32), S(16, 30), S(19, 35), S(34, 19), S(27, 10), S(26,  9), S(29, 10), S(18, 10),
      S(-18, 23), S(-16, 34), S(-11, 30), S(-5, 23), S(-8, 19), S(-23, 23), S(3, 12), S(-9,  7),
      S(-26,  9), S(-13, 13), S(-21,  9), S(-17,  5), S(-14, -1), S(-13,-10), S(12,-19), S(-12,-17),
      S(-39, 17), S(-18,  6), S(-8,  7), S(-7,  2), S(-1, -7), S(-14, -8), S(0,-16), S(-43,  7),
      S(-16, 16), S(-10, 10), S(-9, 12), S(-3, -1), S(-2, -8), S(0,  4), S(3, -6), S(-10, -4) },

    { S(-17,  6), S(-5, 24), S(7, 42), S(17, 54), S(17, 60), S(34, 57), S(12, 48), S(10, 36),
      S(-10,  8), S(-56, 54), S(-31, 60), S(-60,101), S(-59,128), S(-19, 81), S(-46, 79), S(-13, 75),
      S(-5,  4), S(-2,  6), S(-8, 41), S(-13, 67), S(2, 83), S(15, 92), S(26, 57), S(13, 67),
      S(3, -7), S(12, 20), S(-6, 29), S(-13, 74), S(-9, 93), S(-2, 91), S(26, 84), S(15, 53),
      S(14,-32), S(8,  8), S(5, 12), S(-5, 51), S(-3, 48), S(9, 35), S(25, 18), S(29, 19),
      S(6,-54), S(20,-31), S(8, -3), S(3,-12), S(6,-11), S(8,-15), S(30,-42), S(18,-43),
      S(7,-62), S(13,-60), S(19,-70), S(14,-32), S(16,-41), S(14,-106), S(20,-111), S(4,-62),
      S(3,-72), S(-2,-77), S(2,-78), S(7,-70), S(6,-72), S(0,-90), S(5,-83), S(2,-81) },

    { S(-82,-103), S(-49,-51), S(-68,-20), S(-63,  6), S(-73,-10), S(-61,-16), S(-57,-12), S(-66,-107),
      S(-64,-44), S(-16, 33), S(-33, 47), S(-32, 40), S(-18, 32), S(-22, 46), S(-17, 49), S(-48,-40),
      S(-26,-20), S(16, 43), S(22, 64), S(27, 70), S(34, 68), S(47, 68), S(24, 53), S(-35,-13),
      S(-30,-22), S(21, 23), S(26, 64), S(33, 85), S(22, 82), S(46, 60), S(13, 26), S(-60,-18),
      S(-23,-53), S(37,  2), S(54, 43), S(23, 74), S(35, 69), S(41, 40), S(51, -1), S(-75,-38),
      S(-47,-41), S(25,-10), S(18, 22), S(4, 44), S(18, 39), S(2, 26), S(19,-10), S(-60,-33),
      S(7,-51), S(6,-21), S(-3, -1), S(-51, 19), S(-40, 15), S(-25,  4), S(7,-28), S(-2,-64),
      S(-35,-115), S(13,-82), S(-15,-52), S(-77,-42), S(-39,-58), S(-77,-34), S(1,-73), S(-28,-125) },
};

// Initialize the piece square tables with piece values included
InitPSQT() {

    // Black scores are negative (white double negated -> positive)
    for (PieceType pt = PAWN; pt <= KING; ++pt)
        for (Square sq = A1; sq <= H8; ++sq) {
            // Base piece value + the piece square value
            PSQT[MakePiece(BLACK, pt)][sq] = -(PieceTypeValue[pt] + PieceSqValue[pt - 1][sq]);
            // Same score inverted used for white on the square mirrored horizontally
            PSQT[MakePiece(WHITE, pt)][MirrorSquare(sq)] = -PSQT[pt][sq];
        }
}

volatile bool ABORT_SIGNAL = false;


// Initializes the late move reduction array
int Reductions[2][32][32];

static void InitReductions() {
    for (int depth = 1; depth < 32; ++depth)
        for (int moves = 1; moves < 32; ++moves)
            Reductions[0][depth][moves] = 0.00 + log(depth) * log(moves) / 3.25, // capture
            Reductions[1][depth][moves] = 1.75 + log(depth) * log(moves) / 1.75; // quiet
}

// Check if current position is a repetition
static bool IsRepetition(Position* pos) {

    // Compare current posKey to posKeys in history, skipping
    // opponents turns as that wouldn't be a repetition
    for (int i = 4; i <= pos->rule50 && i <= pos->histPly; i += 2)
        if (pos->key == history(-i).posKey)
            return true;

    return false;
}

// Check time situation
static bool OutOfTime(SearchInfo* info) {

    if ((info->nodes & 2048) == 0
        && info->timeSet
        && GetTickCount() >= info->endTime)

        return true;

    return false;
}

static int Quiescence(Position* pos, SearchInfo* info, int alpha, const int beta) {

    MovePicker mp;
    MoveList list;

    // Check time situation
    if (OutOfTime(info) || ABORT_SIGNAL)
        longjmp(info->jumpBuffer, true);

    // Do a static evaluation for pruning considerations
    int eval = EvalPosition(pos);

    // If we are at max depth, return static eval
    if (pos->ply >= MAXDEPTH)
        return eval;

    // If eval beats beta we assume some move will also beat it
    if (eval >= beta)
        return eval;

    // Use eval as a lowerbound if it's above alpha (but below beta)
    if (eval > alpha)
        alpha = eval;

    InitNoisyMP(&mp, &list, pos);

    int futility = eval + 40;
    int bestScore = eval;
    int score;

    // Move loop
    Move move;
    while ((move = NextMove(&mp))) {

        // Skip moves SEE deem bad
        if (mp.stage > nOISY_GOOD) break;

        if (futility + PieceValue[EG][pieceOn(toSq(move))] <= alpha
            && !(PieceTypeOf(pieceOn(fromSq(move))) == PAWN
                && RelativeRank(sideToMove, RankOf(toSq(move))) > 5))
            continue;

        if (futility <= alpha
            && !SEE(pos, move, 1)) {
            bestScore = MAX(bestScore, futility);
            continue;
        }
        
        // Recursively search the positions after making the moves, skipping illegal ones
        if (!MakeMove(pos, move)) continue;
        score = -Quiescence(pos, info, -beta, -alpha);
        TakeMove(pos);

        // Found a new best move in this position
        if (score > bestScore) {

            bestScore = score;

            // If score beats alpha we update alpha
            if (score > alpha) {
                alpha = score;

                // If score beats beta we have a cutoff
                if (score >= beta)
                    break;
            }
        }
    }

    return bestScore;
}

int tempo = 15;

// Alpha Beta
static int AlphaBeta(Position* pos, SearchInfo* info, int alpha, int beta, int depth, PV* pv) {

    const bool pvNode = alpha != beta - 1;
    const bool root = pos->ply == 0;

    PV pvFromHere;
    pv->length = 0;

    MovePicker mp;
    MoveList list;

    int R;

    // Check time situation
    if (OutOfTime(info) || ABORT_SIGNAL)
        longjmp(info->jumpBuffer, true);

    // Early exits
    if (!root) {

        // Position is drawn
        if (IsRepetition(pos) || pos->rule50 >= 100)
            return 0;

        // Max depth reached
        if (pos->ply >= MAXDEPTH)
            return EvalPosition(pos);

        // Mate distance pruning
        alpha = MAX(alpha, -INF + pos->ply);
        beta = MIN(beta, INF - pos->ply - 1);
        if (alpha >= beta)
            return alpha;
    }

    // Extend search if in check
    const bool inCheck = KingAttacked(pos, sideToMove);
    if (inCheck) depth++;

    // Update node count and selective depth
    info->nodes++;
    if (pos->ply > info->seldepth)
        info->seldepth = pos->ply;

    // Quiescence at the end of search
    if (depth <= 0)
        return Quiescence(pos, info, alpha, beta);

    // Probe transposition table
    bool ttHit;
    Key posKey = pos->key;
    TTEntry* tte = ProbeTT(posKey, &ttHit);

    Move ttMove = ttHit ? tte->move : NOMOVE;
    int ttScore = ttHit ? ScoreFromTT(tte->score, pos->ply) : NOSCORE;

    // Trust the ttScore in non-pvNodes as long as the entry depth is equal or higher
    if (!pvNode && ttHit && tte->depth >= depth) {

        // Check if ttScore causes a cutoff
        if (ttScore >= beta ? tte->bound & BOUND_LOWER
            : tte->bound & BOUND_UPPER)

            return ttScore;
    }

    int bestScore = -INF;

    int score;

    // Do a static evaluation for pruning considerations
    int eval = history(0).eval = inCheck ? NOSCORE
        : lastMoveNullMove ? -history(-1).eval + 2 * Tempo
        : EvalPosition(pos);

    // Use ttScore as eval if useful
    if (ttScore != NOSCORE
        && (tte->bound & (ttScore > eval ? BOUND_LOWER : BOUND_UPPER)))
        eval = ttScore;
    
    // Improving if not in check, and current eval is higher than 2 plies ago
    bool improving = !inCheck && eval > history(-2).eval;
    
    // Skip pruning while in check and at the root
    if (inCheck || pvNode || root)
        goto move_loop;

    // Razoring
    if (depth < 2
        && eval + 350 < alpha)
        return Quiescence(pos, info, alpha, beta);

    // Reverse Futility Pruning
    if (depth < 7
        && eval - 175 * depth + 100 * improving >= beta
        && abs(beta) < ISMATE)
        return eval;
    
    // Null Move Pruning
    if (depth >= 3
        && eval >= beta
        && history(-1).move != NOMOVE
        && history(0).eval >= beta
        && pos->nonPawnCount[sideToMove] > (depth > 8)
        && (!ttHit || !(tte->bound & BOUND_UPPER) || ttScore >= beta)) {

        int R = 3 + depth / 5 + MIN(3, (eval - beta) / 256);

        MakeNullMove(pos);
        score = -AlphaBeta(pos, info, -beta, -beta + 1, depth - R, &pvFromHere);
        TakeNullMove(pos);

        // Cutoff
        if (score >= beta) {
            // Don't return unproven terminal win scores
            return score >= ISMATE ? beta : score;
        }
    }

    // ProbCut
    if (depth >= 5
        && abs(beta) < ISMATE
        && (!ttHit || !(tte->bound & BOUND_UPPER) || ttScore >= beta)) {

        int pbBeta = beta + 200;

        MovePicker pbMP;
        MoveList pbList;
        InitNoisyMP(&pbMP, &pbList, pos);

        Move pbMove;
        while ((pbMove = NextMove(&pbMP))) {

            if (pbMP.stage > nOISY_GOOD) break;

            if (!MakeMove(pos, pbMove)) continue;

            int pbScore = -Quiescence(pos, info, -pbBeta, -pbBeta + 1);

            if (pbScore >= pbBeta)
                pbScore = -AlphaBeta(pos, info, -pbBeta, -pbBeta + 1, depth - 4, &pvFromHere);

            TakeMove(pos);

            if (pbScore >= pbBeta)
                return pbScore;
        }
    }

    // Internal iterative deepening based on Rebel's idea
    if (depth >= 4 && !ttMove)
        depth--;
move_loop:

    InitNormalMP(&mp, &list, pos, depth, ttMove, killer1, killer2);

    Move quiets[32] = { 0 };
    
    const int oldAlpha = alpha;
    int moveCount = 0, quietCount = 0;
    Move bestMove = NOMOVE;
    score = -INF;

    // Move loop
    Move move;
    while ((move = NextMove(&mp))) {

        bool quiet = moveIsQuiet(move);

        // Late move pruning
        if (!pvNode && !inCheck && quietCount > (3 + 2 * depth * depth) / (2 - improving))
            break;

        GetEntry(KeyAfter(pos, move));

        // Make the move, skipping to the next if illegal
        if (!MakeMove(pos, move)) continue;

        // Increment counts
        moveCount++;
        if (quiet && quietCount < 32)
            quiets[quietCount++] = move;

        const int newDepth = depth - 1;

        bool doLMR = depth > 2 && moveCount > (2 + pvNode);

        // Reduced depth zero-window search
        if (doLMR) {
            // Base reduction
            int R = Reductions[quiet][MIN(31, depth)][MIN(31, moveCount)];
            // Reduce less in pv nodes
            R -= pvNode;
            // Reduce less when improving
            R -= improving;
            // Reduce more for quiets
            R += quiet;

            // Depth after reductions, avoiding going straight to quiescence
            int RDepth = CLAMP(newDepth - R, 1, newDepth - 1);

            score = -AlphaBeta(pos, info, -alpha - 1, -alpha, RDepth, &pvFromHere);
        }
        // Full depth zero-window search
        if (doLMR ? score > alpha : !pvNode || moveCount > 1)
            score = -AlphaBeta(pos, info, -alpha - 1, -alpha, newDepth, &pvFromHere);

        // Full depth alpha-beta window search
        if (pvNode && ((score > alpha&& score < beta) || moveCount == 1))
            score = -AlphaBeta(pos, info, -beta, -alpha, newDepth, &pvFromHere);

        // Undo the move
        TakeMove(pos);

        // Found a new best move in this position
        if (score > bestScore) {

            bestScore = score;
            bestMove = move;

            // Update the Principle Variation
            if ((score > alpha&& pvNode) || (root && moveCount == 1)) {
                pv->length = 1 + pvFromHere.length;
                pv->input[0] = move;
                memcpy(pv->input + 1, pvFromHere.input, sizeof(int) * pvFromHere.length);
            }

            // If score beats alpha we update alpha
            if (score > alpha) {

                alpha = score;

                // Update search history
                if (quiet && depth > 1)
                    pos->history[pieceOn(fromSq(bestMove))][toSq(bestMove)] += depth * depth;

                // If score beats beta we have a cutoff
                if (score >= beta) {

                    // Update killers if quiet move
                    if (quiet && killer1 != move) {
                        killer2 = killer1;
                        killer1 = move;
                    }

                    break;
                }
            }
        }
    }

    // Lower history scores of moves that failed to produce a cut
    if (bestScore >= beta && moveIsQuiet(bestMove))
        for (int i = 0; i < quietCount; ++i) {
            Move m = quiets[i];
            if (m == bestMove) continue;
            pos->history[pieceOn(fromSq(m))][toSq(m)] -= depth * depth;
        }

    // Checkmate or stalemate
    if (!moveCount)
        return inCheck ? -INF + pos->ply : 0;

    // Store in TT
    const int flag = bestScore >= beta ? BOUND_LOWER
        : alpha != oldAlpha ? BOUND_EXACT
        : BOUND_UPPER;

    StoreTTEntry(tte, posKey, bestMove, ScoreToTT(bestScore, pos->ply), depth, flag);

    return bestScore;
}

// Aspiration window
static int AspirationWindow(Position* pos, SearchInfo* info) {

    int score = info->score;
    int depth = info->IDDepth;

    const int initialWindow = 12;
    int delta = 16;

    int alpha = -INF;
    int beta = INF;

    // Shrink the window at higher depths
    if (depth > 6)
    {
        //does not reach
        alpha = MAX(score - initialWindow, -INF);
        beta = MIN(score + initialWindow, INF);
    }

    // Search with aspiration window until the result is inside the window
    while (true) {

        if (alpha < -3500) alpha = -INF;
        if (beta > 3500) beta = INF;

        score = AlphaBeta(pos, info, alpha, beta, info->IDDepth, &info->pv);

        // Failed low, relax lower bound and search again
        if (score <= alpha) {
            alpha = MAX(alpha - delta, -INF);
            beta = (alpha + beta) / 2;
            depth = info->IDDepth;

            // Failed high, relax upper bound and search again
        }
        else if (score >= beta) {
            beta = MIN(beta + delta, INF);
            depth -= (abs(score) < ISMATE);

            // Score within the bounds is accepted as correct
        }
        else return score;

        delta += delta * 2 / 3;
    }
}

// Get ready to start a search
static void PrepareSearch(Position* pos, SearchInfo* info) {

    memset(pos->history, 0, sizeof(pos->history));
    memset(pos->killers, 0, sizeof(pos->killers));

    pos->ply = 0;
    info->nodes = 0;
    info->seldepth = 0;

    // Mark TT as used
    TT.dirty = true;
}

void SearchPosition(Position* pos, SearchInfo* info)
{
    PrepareSearch(pos, info);
    
    // Iterative deepening
    for (info->IDDepth = 1; info->IDDepth <= info->depth; ++info->IDDepth) {


        if (setjmp(info->jumpBuffer)) break;
        
        // Search position, using aspiration windows for higher depths
        if (info->IDDepth > 6)
        {
            info->score = AspirationWindow(pos, info);
        }
        else
            info->score = AlphaBeta(pos, info, -INF, INF, info->IDDepth, &info->pv);

        PrintThinking(pos, info);
        
        info->bestMove = info->pv.input[0];
    }

    // Print conclusion
    PrintConclusion(info);

}

// Translates a move to a string
char* MoveToStr(Move move) {

    static char moveStr[6];

    int ff = FileOf(fromSq(move));
    int rf = RankOf(fromSq(move));
    int ft = FileOf(toSq(move));
    int rt = RankOf(toSq(move));

    PieceType promo = PieceTypeOf(promotion(move));

    char pchar = promo == QUEEN ? 'q'
        : promo == KNIGHT ? 'n'
        : promo == ROOK ? 'r'
        : promo == BISHOP ? 'b'
        : '\0';

    sprintf(moveStr, "%c%c%c%c%c", ('a' + ff), ('1' + rf), ('a' + ft), ('1' + rt), pchar);

    return moveStr;
}

Square AlgebraicToSq(const char file, const char rank) {
    return (file - 'a') + 8 * (rank - '1');
}

// Translates a string to a move (assumes correct input)
Move ParseMove(const char* str, Position* pos) {

    // Translate coordinates into square numbers
    Square from = AlgebraicToSq(str[0], str[1]);
    Square to = AlgebraicToSq(str[2], str[3]);

    Piece promo = str[4] == 'q' ? MakePiece(sideToMove, QUEEN)
        : str[4] == 'n' ? MakePiece(sideToMove, KNIGHT)
        : str[4] == 'r' ? MakePiece(sideToMove, ROOK)
        : str[4] == 'b' ? MakePiece(sideToMove, BISHOP)
        : 0;

    PieceType pt = PieceTypeOf(pieceOn(from));

    int flag = pt == KING && Distance(from, to) > 1 ? FLAG_CASTLE
        : pt == PAWN && Distance(from, to) > 1 ? FLAG_PAWNSTART
        : pt == PAWN && str[0] != str[2] && !pieceOn(to) ? FLAG_ENPAS
        : 0;

    return MOVE(from, to, pieceOn(to), promo, flag);
}

// Print thinking
void PrintThinking(Position* pos, SearchInfo* info) {

    int score = info->score;
    
    // Determine whether we have a centipawn or mate score
    char* type = abs(score) >= ISMATE ? "mate" : "cp";

    // Translate internal score into printed score
    score = score > ISMATE ? ((INF - score) / 2) + 1
        : score < -ISMATE ? -((INF + score) / 2)
        : score * 100 / P_MG;

    int depth = info->IDDepth;
    int seldepth = info->seldepth;
    int elapsed = GetTickCount() - info->startTime;
    int hashFull = HashFull();
    int nps = (int)(1000 * (info->nodes / (elapsed + 1)));
    uint64_t nodes = info->nodes;

    // Basic info
    printf("info depth %d seldepth %d score %s %d time %d nodes %" PRId64 " nps %d ",
        depth, seldepth, type, score, elapsed, nodes, nps);

    // Principal variation
    printf("pv");
    for (int i = 0; i < info->pv.length; i++)
        printf(" %s", MoveToStr(info->pv.input[i]));

    printf("\n");
    fflush(stdout);
}

// Print conclusion of search - best move and ponder move
void PrintConclusion(SearchInfo* info) {

    printf("bestmove %s", MoveToStr(info->bestMove));
    printf("\n\n");
    fflush(stdout);
}

#define START_FEN  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
#define INPUT_SIZE 4096

#define INPUTBUFFER 400 * 6

void parseGo(Position* pos, SearchInfo* info, char* input) {

    int depth = -1, movesToGo = 30, movetime = -1;
    int time = -1, increment = 0;
    char* ptr = NULL;
    info->timeSet = 0;

    if ((ptr = strstr(input, "binc")) && pos->stm == BLACK) {
        increment = atoi(ptr + 5);
    }

    if ((ptr = strstr(input, "winc")) && pos->stm == WHITE) {
        increment = atoi(ptr + 5);
    }

    if ((ptr = strstr(input, "wtime")) && pos->stm == WHITE) {
        time = atoi(ptr + 6);
    }

    if ((ptr = strstr(input, "btime")) && pos->stm == BLACK) {
        time = atoi(ptr + 6);
    }

    if ((ptr = strstr(input, "movestogo"))) {
        movesToGo = atoi(ptr + 10);
    }

    if ((ptr = strstr(input, "movetime"))) {
        movetime = atoi(ptr + 9);
    }

    if ((ptr = strstr(input, "depth"))) {
        depth = atoi(ptr + 6);
    }

    if (movetime != -1) {
        time = movetime;
        movesToGo = 1;
    }

    info->startTime = GetTickCount();
    info->depth = depth;

    if (time != -1)
    {
        info->timeSet = 1;
        time /= movesToGo;
        time -= 50;
        info->endTime = info->startTime + time + increment;
    }

    if (depth == -1) {
        info->depth = MAXDEPTH;
    }

    printf("time:%d start:%d stop:%d depth:%d timeSet:%d\n",
        time, info->startTime, info->endTime, info->depth, info->timeSet);
    SearchPosition(pos, info);
}

// Parse FEN and set up the position as described
void ParseFen(const char* fen, Position* pos) {

    ClearPosition(pos);

    // Piece locations
    Square sq = A8;
    while (*fen != ' ') {

        Piece piece;
        int count = 1;

        switch (*fen) {
            // Pieces
        case 'p': piece = bP; break;
        case 'n': piece = bN; break;
        case 'b': piece = bB; break;
        case 'r': piece = bR; break;
        case 'q': piece = bQ; break;
        case 'k': piece = bK; break;
        case 'P': piece = wP; break;
        case 'N': piece = wN; break;
        case 'B': piece = wB; break;
        case 'R': piece = wR; break;
        case 'Q': piece = wQ; break;
        case 'K': piece = wK; break;
            // Next rank
        case '/':
            sq -= 16;
            fen++;
            continue;
            // Numbers of empty squares
        default:
            piece = EMPTY;
            count = *fen - '0';
            break;
        }

        pieceOn(sq) = piece;
        sq += count;

        fen++;
    }
    fen++;

    // Update the rest of position to match pos->board
    UpdatePosition(pos);

    // Side to move
    sideToMove = (*fen == 'w') ? WHITE : BLACK;
    fen += 2;

    // Castling rights
    while (*fen != ' ') {

        switch (*fen) {
        case 'K': pos->castlingRights |= WHITE_OO;  break;
        case 'Q': pos->castlingRights |= WHITE_OOO; break;
        case 'k': pos->castlingRights |= BLACK_OO;  break;
        case 'q': pos->castlingRights |= BLACK_OOO; break;
        default: break;
        }
        fen++;
    }
    fen++;

    // En passant square
    Square ep = AlgebraicToSq(fen[0], fen[1]);
    bool epValid = *fen != '-' && (PawnAttackBB(!sideToMove, ep)
        & colorPieceBB(sideToMove, PAWN));
    pos->epSquare = epValid ? ep : 0;

    // 50 move rule and game moves
    pos->rule50 = atoi(fen += 2);
    pos->gameMoves = atoi(fen += 2);

    // Generate the position key
    pos->key = GeneratePosKey(pos);
}

void parsePosition(Position* pos, char* input) {

    input += 9;
    char* ptr = input;

    if (strncmp(input, "startpos", 8) == 0)
    {
        ParseFen(START_FEN, pos);
    }
    else
    {
        ptr = strstr(input, "fen");
        if (ptr == NULL)
        {
            ParseFen(START_FEN, pos);
        }
        else
        {
            ptr += 4;
            ParseFen(ptr, pos);
        }
    }

    ptr = strstr(input, "moves");
    int move;

    if (ptr != NULL) {
        ptr += 6;
        while (*ptr) {
            move = ParseMove(ptr, pos);
            if (move < 0) break;
            MakeMove(pos, move);
            pos->ply = 0;
            while (*ptr && *ptr != ' ') {
                ptr++;
            }
            ptr++;
        }
    }
    PrintBoard(pos);
}

void uci(Position* pos, SearchInfo* info) {

    setbuf(stdin, NULL);
    setbuf(stdout, NULL);

    char input[INPUTBUFFER];
    printf("id name " NAME " " VERSION "\n");
    printf("id author " AUTHOR "\n");
    printf("uciok\n");

    while (1) {
        memset(&input[0], 0, sizeof(input));
        fflush(stdout);
        if (!fgets(input, INPUTBUFFER, stdin))
            continue;

        if (input[0] == '\n')
            continue;

        if (!strncmp(input, "isready", 7)) {
            InitTT();
            printf("readyok\n");
            continue;
        }
        else if (!strncmp(input, "position", 8)) {
            parsePosition(pos, input);
        }
        else if (!strncmp(input, "ucinewgame", 10)) {
            parsePosition(pos, "position startpos\n");
        }
        else if (!strncmp(input, "go", 2)) {
            parseGo(pos, info, input);
        }
        else if (!strncmp(input, "quit", 4)) {
            info->quit = 1;
        }
        else if (!strncmp(input, "uci", 3)) {
            printf("id name " NAME " " VERSION "\n");
            printf("id author " AUTHOR "\n");
            printf("uciok\n");
        }
        if (info->quit) {
            break;
        }
    }
}

void initAll() {

    initBitMasks();
    InitBitMasks();
    InitPSQT();
    InitHashKeys();
    InitTT();
    InitReductions();
    InitMvvLva();

    initFileRankArrays();

    initBitRays();

    initKnightAttacks();
    initKingAttacks();
    initRookAttacks();
    initBishopAttacks();

    InitBetweenBB();
}

void printBitBoard(Bitboard bb) {
    for (int square = 56; square >= 0; square++) {
        if (setMask[square] & bb) printf(" X ");
        else printf(" - ");
        if ((square + 1) % 8 == 0) {
            square -= 16;
            printf("\n");
        }
    }
    printf("\n");
}

long leafNodes;

void perft(Position* pos, int depth) {

    if (depth == 0) {
        leafNodes++;
        return;
    }

    MoveList list[1];

    MovePicker mp;
    InitNormalMP(&mp, list, pos, 0, NOMOVE, NOMOVE, NOMOVE);

    Move move;
    while ((move = NextMove(&mp))) {
        if (!MakeMove(pos, move)) continue;
        perft(pos, depth - 1);
        TakeMove(pos);
    }

    return;
}

// Counts number of moves that can be made in a position to some depth
void Perft(int depth, Position* pos) {

    PrintBoard(pos);
    printf("\nStarting Test To Depth:%d\n", depth);
    leafNodes = 0;
    int start = GetTickCount();
    MoveList list[1];

    MovePicker mp;
    InitNormalMP(&mp, list, pos, 0, NOMOVE, NOMOVE, NOMOVE);

    Move move;
    int MoveNum = 0;
    while ((move = NextMove(&mp))) {
        ++MoveNum;
        if (!MakeMove(pos, move)) {
            continue;
        }
        long cumnodes = leafNodes;
        perft(pos, depth - 1);
        TakeMove(pos);
        long oldnodes = leafNodes - cumnodes;
        printf("move %d : %s : %ld\n", MoveNum + 1, MoveToStr(move), oldnodes);

    }

    printf("\nTest Complete : %ld nodes visited in %dms\n", leafNodes, GetTickCount() - start);

    return;
}

// Sets up the engine and follows UCI protocol commands
int main() {

    // debug mode variable
    int debug = 0;

    // if debugging - debug 1
    if (debug)
    {
        initAll();

        Position pos[1];
        MoveList list[1];
        ParseFen(START_FEN, pos);
        Perft(6, pos);

        while (1);
    }
    else
    {// Init engine
        initAll();

        Position pos[1];
        SearchInfo info[1];
        info->quit = 0;

        TT.currentMB = 0;
        TT.requestedMB = 128;

        setbuf(stdin, NULL);
        setbuf(stdout, NULL);

        // UCI loop
        char input[3000];
        while (1) {
            memset(&input[0], 0, sizeof(input));

            fflush(stdout);
            if (!fgets(input, 256, stdin))
                continue;
            if (input[0] == '\n')
                continue;
            if (!strncmp(input, "uci", 3)) {
                uci(pos, info);
                if (info->quit == 1) break;
                continue;
            }
            else if (!strncmp(input, "quit", 4)) {
                break;
            }
        }
        free(TT.mem);
    }
    return 0;
}

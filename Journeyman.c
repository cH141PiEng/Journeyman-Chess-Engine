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

//Integrated classic type bitboard move generation from Butter.
//The evaluation is mostly the same as in Ethereal 9.30.
//Search and many other parts of the code are from Weiss .10.

typedef unsigned long long U64;
typedef char string[200];

#define NAME "Journeyman 1.8"

#define NUM_SQUARES 64
#define MAX_GAME_MOVES 2048
#define MAX_DEPTH 128
#define MAX_POSITION_MOVES 256

#define STARTING_FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

#define infinite 32500
#define ISMATE (infinite - MAX_DEPTH)
#define NOSCORE = 32501

enum {
    WHITE, BLACK,
    WHITE_PAWN, WHITE_KNIGHT, WHITE_BISHOP, WHITE_ROOK, WHITE_QUEEN, WHITE_KING,
    BLACK_PAWN, BLACK_KNIGHT, BLACK_BISHOP, BLACK_ROOK, BLACK_QUEEN, BLACK_KING,
};

enum { PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING };

enum { FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H };
enum { RANK_1, RANK_2, RANK_3, RANK_4, RANK_5, RANK_6, RANK_7, RANK_8 };
enum { WHITE_KING_CASTLE = 1, WHITE_QUEEN_CASTLE = 2, BLACK_KING_CASTLE = 4, BLACK_QUEEN_CASTLE = 8 };

enum {
    A1, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8,
};

typedef uint64_t Key;
typedef uint32_t Move;
typedef int32_t Depth;

//Added

typedef struct PV {
    int length;
    int input[MAX_DEPTH];
} PV;

typedef struct {
    Move move;
    int score;
} MoveListEntry;

//added int next;

typedef struct {
    MoveListEntry moves[MAX_POSITION_MOVES];
    unsigned int count;
    unsigned int next;
}MoveList;

typedef struct {
    Key posKey;
    Move move;
    uint8_t enPassantSquare;
    uint8_t fiftyMoveCounter;
    uint8_t castlePermissions;
    int eval;
} History;

typedef struct {
    Key posKey;
    Move move;
    int16_t score;
    uint8_t depth;
    uint8_t bound;
} TTEntry;

typedef struct {
    int move;

    int castlePermissions;
    int fiftyMoveCounter;
    int enPassantSquare;
    U64 posKey;
} Undo;

typedef struct {
    U64 pieceBB[14];
    U64 emptyBB, occupiedBB;

    int side;
    uint8_t enPassantSquare;
    uint8_t castlePermissions;
    uint8_t fiftyMoveCounter;

    uint8_t ply;
    uint16_t gamePly;

    Key key;

    History gameHistory[MAX_GAME_MOVES];

    int history[14][64];
    Move killers[MAX_DEPTH][2];

    int hasCastled[2];
} Board;

typedef struct {
    int startTime;
    int endTime;
    unsigned int depth;

    int seldepth;
    int timeSet;
    int movesToGo;

    uint64_t nodes;

    int quit;
    int stopped;

#ifdef SEARCH_STATS
    float failHigh;
    float failHighFirst;
    int nullCut;
#endif

    PV pv;
    int bestMove;

    jmp_buf jumpBuffer;

    int score;
    int IDDepth;
} SearchInfo;

//added next 2

typedef enum MPStage {
    TTMOVE, GEN_NOISY, NOISY, GEN_QUIET, QUIET
} MPStage;

typedef struct MovePicker {
    Board* position;
    MoveList* list;
    MPStage stage;
    int ttMove;
    bool onlyNoisy;
} MovePicker;

enum { NORTH, NORTH_EAST, EAST, SOUTH_EAST, SOUTH, SOUTH_WEST, WEST, NORTH_WEST };

enum {
    FLAG_NONE, FLAG_ALPHA, FLAG_BETA, FLAG_EXACT
};

const U64 FLAG_CA = 0x1000000;
const U64 FLAG_EP = 0x2000000;
const U64 FLAG_PS = 0x4000000;

#define generateMove(f, t, ca, mov, pro, flags) ((f) | ((t) << 6) | ((ca) << 12) | ((mov) << 16) | ((pro) << 20) | flags)

#define from(m) ((m) & 0x3F)
#define to(m) (((m) >> 6) & 0x3F)
#define captured(m) (((m) >> 12) & 0xF)
#define moving(m) (((m) >> 16) & 0xF)
#define promoted(m) (((m) >> 20) & 0xF)

#define isCastle(m) ((m) & 0x1000000)
#define isEnPassant(m) ((m) & 0x2000000)
#define isPawnStart(m) ((m) & 0x4000000)
#define isCapture(m) ((m) & 0xF000)
#define isPromote(m) ((m) & 0xF00000)

#define NOMOVE 0

#define lastMoveNullMove (!root && history(-1).move == NOMOVE)
#define history(offset) (position->gameHistory[position->gamePly + offset])
#define killer1 (position->killers[position->ply][0])
#define killer2 (position->killers[position->ply][1])

#define KingAttacks(sq, tg) (KingMap[(sq)] & (tg))

#define File(sq) ((sq) & 7)
#define Rank(sq) ((sq) >> 3)

/* MACROS */

#define getSquare(file, rank) (((rank) * 8) + (file))
#define onBoard(file, rank) (file >= FILE_A && file <= FILE_H && rank >= RANK_1 && rank <= RANK_8)

#define popBit(bb) ((bb) &= ((bb) - 1))
#define setBit(bb,sq) ((bb) |= setMask[(sq)])
#define clearBit(bb,sq) ((bb) &= clearMask[(sq)])
#define bitCount(bb) ((int) (__popcnt64(bb)))

/* GLOBALS */

U64 setMask[65];
U64 clearMask[65];

int fileArray[64];
int rankArray[64];

int FilesBrd[64];
int RanksBrd[64];

U64 knightAttacks[64];
U64 kingAttacks[64];
U64 rookAttacksEmpty[64];
U64 bishopAttacksEmpty[64];

U64 bitRays[8][64];

U64 pieceKeys[14][64];
U64 sideKey;
U64 castleKeys[16];
U64 enPassantKeys[64];

//initialize

#define RAND_64 	((U64)rand() | \
					(U64)rand() << 15 | \
					(U64)rand() << 30 | \
					(U64)rand() << 45 | \
					((U64)rand() & 0xf) << 60 )

void initBitMasks() {
    for (int index = 0; index < 64; index++) {
        setMask[index] = 1ULL << index;
        clearMask[index] = ~setMask[index];
    }
    setMask[64] = 0ULL;
    clearMask[64] = ~setMask[64];
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

static inline int fileOf(int square) {
    return square & 7;
}

static inline int rankOf(int square) {
    return square >> 3;
}

U64 pawn_attacks[2][64];

// Inits the pawn attack bitboards
static void InitPawnAttacks() {

    // All squares needed despite pawns never being on 1. or 8. rank
    for (int sq = A1; sq <= H8; ++sq) {

        // White
        if (rankOf(sq) < RANK_8) {
            if (fileOf(sq) < FILE_H)
                setBit(pawn_attacks[WHITE][sq], sq + 9);
            if (fileOf(sq) > FILE_A)
                setBit(pawn_attacks[WHITE][sq], sq + 7);
        }
        // Black
        if (rankOf(sq) > RANK_1) {
            if (fileOf(sq) < FILE_H)
                setBit(pawn_attacks[BLACK][sq], sq - 7);
            if (fileOf(sq) > FILE_A)
                setBit(pawn_attacks[BLACK][sq], sq - 9);
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

void initHashKeys() {

    for (int i = 0; i < 14; i++) {
        for (int j = 0; j < 64; j++) {
            pieceKeys[i][j] = RAND_64;
        }
    }
    sideKey = RAND_64;
    for (int i = 0; i < 16; i++) {
        castleKeys[i] = RAND_64;
    }
    for (int i = 0; i < 64; i++) {
        enPassantKeys[i] = RAND_64;
    }
}

int FR2SQ(int f, int r)
{
    return f + (r * 8);
}

void InitFilesRanksBrd() {

    int index = 0;
    int file = FILE_A;
    int rank = RANK_1;
    int sq = A1;

    for (rank = RANK_1; rank <= RANK_8; ++rank) {
        for (file = FILE_A; file <= FILE_H; ++file) {
            sq = FR2SQ(file, rank);
            FilesBrd[sq] = file;
            RanksBrd[sq] = rank;
        }
    }
}

U64 IsolatedPawnMasks[64];
U64 PassedPawnMasks[2][64];
U64 PawnAttackMasks[2][64];
U64 PawnConnectedMasks[2][64];
U64 OutpostSquareMasks[2][64];
U64 OutpostRanks[64];

#define RANK8 0xFF00000000000000
#define RANK7 0x00FF000000000000
#define RANK6 0x0000FF0000000000
#define RANK5 0x000000FF00000000
#define RANK4 0x00000000FF000000
#define RANK3 0x0000000000FF0000
#define RANK2 0x000000000000FF00
#define RANK1 0x00000000000000FF

#define FILEA 0x0101010101010101
#define FILEB 0x0202020202020202
#define FILEC 0x0404040404040404
#define FILED 0x0808080808080808
#define FILEE 0x1010101010101010
#define FILEF 0x2020202020202020
#define FILEG 0x4040404040404040
#define FILEH 0x8080808080808080

U64 FILES[8] = { FILEA, FILEB, FILEC, FILED, FILEE, FILEF, FILEG, FILEH };
U64 RANKS[8] = { RANK1, RANK2, RANK3, RANK4, RANK5, RANK6, RANK7, RANK8 };

U64 RanksAtOrAboveMasks[2][8];
int DistanceBetween[64][64];

/**
 * Fill the various masks used to aid in the evaluation
 * function. These masks provide an easy way to determine
 * if a pawn is passed or isolated; if a pawn can attack
 * a given square; if a knight or bishop is on an outpost
 * square; as well as a quick way to determine if a pawn
 * may advance to a given square.
 */
void initalizeMasks() {

    int i, j, file, rank;
    uint64_t files;

    // Init a table for the distance between two given squares
    for (int sq1 = 0; sq1 < 64; sq1++)
        for (int sq2 = 0; sq2 < 64; sq2++)
            DistanceBetween[sq1][sq2] = MAX(abs(fileOf(sq1) - fileOf(sq2)), abs(rankOf(sq1) - rankOf(sq2)));

    // Initalize ranks above masks
    for (i = 0; i < 8; i++) {
        for (j = i; j < 8; j++)
            RanksAtOrAboveMasks[WHITE][i] |= RANKS[j];
        for (j = i; j >= 0; j--)
            RanksAtOrAboveMasks[BLACK][i] |= RANKS[j];
    }
    
    // Initalize isolated pawn masks
    for (i = 0; i < 64; i++) {

        file = File(i);

        if (file > 0 && file < 7)
            IsolatedPawnMasks[i] = FILES[file + 1] | FILES[file - 1];
        else if (file > 0)
            IsolatedPawnMasks[i] = FILES[file - 1];
        else
            IsolatedPawnMasks[i] = FILES[file + 1];
    }

    // Initalize passed pawn masks and outpost masks
    for (i = 0; i < 64; i++) {

        file = File(i); rank = Rank(i);
        files = IsolatedPawnMasks[i] | FILES[file];

        PassedPawnMasks[WHITE][i] = files;
        for (j = rank; j >= 0; j--)
            PassedPawnMasks[WHITE][i] &= ~(RANKS[j]);

        PassedPawnMasks[BLACK][i] = files;
        for (j = rank; j <= 7; j++)
            PassedPawnMasks[BLACK][i] &= ~(RANKS[j]);

        OutpostSquareMasks[WHITE][i] = PassedPawnMasks[WHITE][i] & ~FILES[file];
        OutpostSquareMasks[BLACK][i] = PassedPawnMasks[BLACK][i] & ~FILES[file];
    }

    // Initalize relative outpost ranks
    OutpostRanks[WHITE] = RANK4 | RANK5 | RANK6;
    OutpostRanks[BLACK] = RANK3 | RANK4 | RANK5;

    // Initalize attack square pawn masks
    for (i = 0; i < 64; i++) {

        file = File(i); rank = Rank(i);
        PawnAttackMasks[WHITE][i] = 0ULL;
        PawnAttackMasks[BLACK][i] = 0ULL;

        if (rank == 0) {
            PawnAttackMasks[BLACK][i] |= (1ULL << i) << 7;
            PawnAttackMasks[BLACK][i] |= (1ULL << i) << 9;
        }

        else if (rank == 7) {
            PawnAttackMasks[WHITE][i] |= (1ULL << i) >> 7;
            PawnAttackMasks[WHITE][i] |= (1ULL << i) >> 9;
        }

        else {
            PawnAttackMasks[WHITE][i] |= (1ULL << i) >> 7;
            PawnAttackMasks[WHITE][i] |= (1ULL << i) >> 9;
            PawnAttackMasks[BLACK][i] |= (1ULL << i) << 7;
            PawnAttackMasks[BLACK][i] |= (1ULL << i) << 9;
        }

        if (file == 0) {
            PawnAttackMasks[WHITE][i] &= ~FILEH;
            PawnAttackMasks[BLACK][i] &= ~FILEH;
        }

        else if (file == 7) {
            PawnAttackMasks[WHITE][i] &= ~FILEA;
            PawnAttackMasks[BLACK][i] &= ~FILEA;
        }
    }

    // Initalize pawn connected masks
    for (i = 8; i < 56; i++) {

        file = File(i);

        if (file == 0) {
            PawnConnectedMasks[WHITE][i] = (1ULL << (i + 1)) | (1ULL << (i - 7));
            PawnConnectedMasks[BLACK][i] = (1ULL << (i + 1)) | (1ULL << (i + 9));
        }

        else if (file == 7) {
            PawnConnectedMasks[WHITE][i] = (1ULL << (i - 1)) | (1ULL << (i - 9));
            PawnConnectedMasks[BLACK][i] = (1ULL << (i - 1)) | (1ULL << (i + 7));
        }

        else {
            PawnConnectedMasks[WHITE][i] = (1ULL << (i - 1)) | (1ULL << (i - 9))
                | (1ULL << (i + 1)) | (1ULL << (i - 7));

            PawnConnectedMasks[BLACK][i] = (1ULL << (i - 1)) | (1ULL << (i + 7))
                | (1ULL << (i + 1)) | (1ULL << (i + 9));
        }
    }
}

U64 KingMap[64];

/**
 * Fill the KingMap[64] array with the correct
 * BitBoards for generating king moves
 */
void generateKingMap() {

    int i;
    U64 z = 1;

    for (i = 0; i < 64; i++) {
        if (i + 9 < 64 && i % 8 != 7)
            KingMap[i] |= z << (i + 9);
        if (i - 9 >= 0 && i % 8 != 0)
            KingMap[i] |= z << (i - 9);
        if (i + 7 < 64 && i % 8 != 0)
            KingMap[i] |= z << (i + 7);
        if (i - 7 >= 0 && i % 8 != 7)
            KingMap[i] |= z << (i - 7);
        if (i + 1 < 64 && i % 8 != 7)
            KingMap[i] |= z << (i + 1);
        if (i - 1 >= 0 && i % 8 != 0)
            KingMap[i] |= z << (i - 1);
        if (i + 8 < 64)
            KingMap[i] |= z << (i + 8);
        if (i - 8 >= 0)
            KingMap[i] |= z << (i - 8);
    }
}

static int MvvLvaScores[16][16];

static void InitMvvLva() {

    const int VictimScore[16] = { 0, 106, 206, 306, 406, 506, 606, 0, 0, 106, 206, 306, 406, 506, 606, 0 };
    const int AttackerScore[16] = { 0,   1,   2,   3,   4,   5,   6, 0, 0,   1,   2,   3,   4,   5,   6, 0 };

    for (int Attacker = 1; Attacker < 16; ++Attacker)
        for (int Victim = 1; Victim < 16; ++Victim)
            MvvLvaScores[Victim][Attacker] = VictimScore[Victim] - AttackerScore[Attacker];
}

int LMRTable[32][32];

void InitSearch() {
    // creating the LMR table entries (idea from Ethereal)
    for (int moveDepth = 1; moveDepth < 32; moveDepth++)
        for (int played = 1; played < 32; played++)
            LMRTable[moveDepth][played] = .75 + (log(moveDepth) * log(played) / 2.25);
}

int Reductions[32][32];

// Initializes the late move reduction array
static void InitReductions() {

    for (int depth = 0; depth < 32; ++depth)
        for (int moves = 0; moves < 32; ++moves)
            Reductions[depth][moves] = 0.75 + log(depth) * log(moves) / 2.25;
}

const int SQInv[64] = {
    56,  57,  58,  59,  60,  61,  62,  63,
    48,  49,  50,  51,  52,  53,  54,  55,
    40,  41,  42,  43,  44,  45,  46,  47,
    32,  33,  34,  35,  36,  37,  38,  39,
    24,  25,  26,  27,  28,  29,  30,  31,
    16,  17,  18,  19,  20,  21,  22,  23,
     8,   9,  10,  11,  12,  13,  14,  15,
     0,   1,   2,   3,   4,   5,   6,   7,
};

int square32(int sq) {
    static const int table[8] = { 0, 1, 2, 3, 3, 2, 1, 0 };
    return ((sq >> 3) << 2) + table[sq & 0x7];
}

int relativeSquare32(int sq, int colour) {
    return colour == WHITE ? square32(sq) : square32(SQInv[sq]);
}

#define SQUARE_NB 64
#define PHASE_NB 2

int PSQTMidgame[32][SQUARE_NB];
int PSQTEndgame[32][SQUARE_NB];

const int PawnPSQT32[32][PHASE_NB] = {
    {   0,   0}, {   0,   0}, {   0,   0}, {   0,   0},
    { -23,   2}, {  12,   3}, {  -5,   6}, {  -6,  -2},
    { -25,   0}, {  -3,  -1}, {  -5,  -6}, {  -3, -10},
    { -21,   7}, {  -6,   6}, {   3,  -9}, {   3, -22},
    { -12,  14}, {   3,   8}, {  -1,  -3}, {   4, -23},
    {   3,  25}, {  14,  23}, {  18,   3}, {  16, -23},
    { -40,   6}, { -32,   9}, {   2, -17}, {   4, -33},
    {   0,   0}, {   0,   0}, {   0,   0}, {   0,   0},
};

const int KnightPSQT32[32][PHASE_NB] = {
    { -33, -46}, {   4, -40}, { -12, -12}, {   9,  -8},
    {  10, -50}, {   2, -12}, {  17, -24}, {  20,  -3},
    {   5, -18}, {  29, -18}, {  16,   5}, {  29,  15},
    {   7,  11}, {  27,   9}, {  31,  34}, {  38,  37},
    {  28,   9}, {  36,  15}, {  35,  41}, {  48,  42},
    { -23,   9}, {  38,   7}, {  42,  39}, {  50,  36},
    { -44, -18}, { -41,   7}, {  60, -26}, {  14,  -1},
    {-163, -29}, {-103, -30}, {-154,  -6}, { -55, -23},
};

const int BishopPSQT32[32][PHASE_NB] = {
    {  14, -21}, {  21, -21}, {   2, -10}, {  19, -14},
    {  31, -28}, {  33, -23}, {  23, -14}, {  10,  -2},
    {  25, -11}, {  32, -12}, {  25,   1}, {  19,   7},
    {  10,  -3}, {  13,   0}, {  11,  15}, {  33,  20},
    {  -9,  14}, {  23,   4}, {  10,  17}, {  35,  21},
    {  -6,  10}, {   8,   9}, {  32,   8}, {  22,   5},
    { -55,   6}, {  13,  -3}, {  -2, -10}, { -36,   3},
    { -35,  -1}, { -55,  -3}, {-144,   7}, {-124,  16},
};

const int RookPSQT32[32][PHASE_NB] = {
    {  -4, -32}, {  -7, -16}, {   5, -13}, {  11, -20},
    { -33, -25}, {  -7, -26}, {   0, -20}, {  10, -26},
    { -22, -21}, {   2, -13}, {   1, -21}, {   1, -20},
    { -23,  -1}, { -12,   5}, {  -7,   3}, {  -1,   2},
    { -15,  14}, {  -8,   9}, {  20,   8}, {  21,   8},
    { -14,  17}, {  18,  12}, {  20,  15}, {  22,  14},
    {   0,  18}, {  -4,  18}, {  40,   3}, {  22,   9},
    {  -1,  25}, {  17,  15}, { -19,  24}, {  11,  30},
};

const int QueenPSQT32[32][PHASE_NB] = {
    {  -3, -45}, { -13, -26}, {  -6, -17}, {  14, -39},
    {   3, -49}, {  12, -36}, {  19, -50}, {  14, -16},
    {   6, -22}, {  23, -19}, {   6,   3}, {   4,   3},
    {   4,  -5}, {   6,   3}, {   0,  11}, {  -5,  45},
    {  -9,  10}, { -12,  31}, {  -5,  20}, { -20,  50},
    {  -3,   3}, {   4,  21}, {   7,  18}, {  -6,  45},
    {   8,  15}, { -53,  55}, {  32,  12}, { -13,  70},
    { -18, -32}, {   5, -18}, {   0, -14}, { -10,  10},
};

const int KingPSQT32[32][PHASE_NB] = {
    {  78,-103}, {  88, -79}, {  37, -35}, {  20, -37},
    {  69, -53}, {  61, -45}, {  14,  -6}, { -16,   1},
    {  -1, -41}, {  46, -29}, {  20,  -1}, { -10,  14},
    { -52, -35}, {  29, -21}, {   7,  15}, { -46,  35},
    { -27, -19}, {  53,   1}, {   7,  30}, { -34,  37},
    {  38, -17}, {  82,   2}, {  63,  19}, {  -3,  17},
    {  25, -15}, {  60,  -2}, {  39,   4}, {  23,   7},
    {   1, -80}, {  97, -59}, { -12, -34}, { -21, -31},
};

#define MG 0
#define EG 1

void initializePSQT() {

    int sq, w32, b32;

    for (sq = 0; sq < SQUARE_NB; sq++) {

        w32 = relativeSquare32(sq, WHITE);
        b32 = relativeSquare32(sq, BLACK);

        PSQTMidgame[WHITE_PAWN][sq] = PawnPSQT32[w32][MG];
        PSQTEndgame[WHITE_PAWN][sq] = PawnPSQT32[w32][EG];
        PSQTMidgame[WHITE_KNIGHT][sq] = KnightPSQT32[w32][MG];
        PSQTEndgame[WHITE_KNIGHT][sq] = KnightPSQT32[w32][EG];
        PSQTMidgame[WHITE_BISHOP][sq] = BishopPSQT32[w32][MG];
        PSQTEndgame[WHITE_BISHOP][sq] = BishopPSQT32[w32][EG];
        PSQTMidgame[WHITE_ROOK][sq] = RookPSQT32[w32][MG];
        PSQTEndgame[WHITE_ROOK][sq] = RookPSQT32[w32][EG];
        PSQTMidgame[WHITE_QUEEN][sq] = QueenPSQT32[w32][MG];
        PSQTEndgame[WHITE_QUEEN][sq] = QueenPSQT32[w32][EG];
        PSQTMidgame[WHITE_KING][sq] = KingPSQT32[w32][MG];
        PSQTEndgame[WHITE_KING][sq] = KingPSQT32[w32][EG];

        PSQTMidgame[BLACK_PAWN][sq] = PawnPSQT32[b32][MG];
        PSQTEndgame[BLACK_PAWN][sq] = PawnPSQT32[b32][EG];
        PSQTMidgame[BLACK_KNIGHT][sq] = KnightPSQT32[b32][MG];
        PSQTEndgame[BLACK_KNIGHT][sq] = KnightPSQT32[b32][EG];
        PSQTMidgame[BLACK_BISHOP][sq] = BishopPSQT32[b32][MG];
        PSQTEndgame[BLACK_BISHOP][sq] = BishopPSQT32[b32][EG];
        PSQTMidgame[BLACK_ROOK][sq] = RookPSQT32[b32][MG];
        PSQTEndgame[BLACK_ROOK][sq] = RookPSQT32[b32][EG];
        PSQTMidgame[BLACK_QUEEN][sq] = QueenPSQT32[b32][MG];
        PSQTEndgame[BLACK_QUEEN][sq] = QueenPSQT32[b32][EG];
        PSQTMidgame[BLACK_KING][sq] = KingPSQT32[b32][MG];
        PSQTEndgame[BLACK_KING][sq] = KingPSQT32[b32][EG];
    }
}


void initAll() {
    initBitMasks();
    initFileRankArrays();

    InitFilesRanksBrd();

    initalizeMasks();
    generateKingMap();

    initKnightAttacks();
    initKingAttacks();
    initRookAttacks();
    initBishopAttacks();
    InitPawnAttacks();

    initBitRays();

    initHashKeys();
    InitMvvLva();
    InitSearch();

    InitReductions();
    initializePSQT();
}

//bitboard tools

void printBitBoard(U64 bb) {
    for (int square = 56; square >= 0; square++) {
        if (setMask[square] & bb) printf(" X ");
        else printf(" - ");
        if ((square + 1) % 8 == 0) {
            square -= 16;
            printf("\n");
        }
    }
}

int peekBit(U64 bb) {
    unsigned long index = -1;
    _BitScanForward64(&index, bb);
    return index;
}

int peekBitReverse(U64 bb) {
    unsigned long index = -1;
    _BitScanReverse64(&index, bb);
    return index;
}

//attacks

const U64 NOT_FILE_A_MASK = 0xFEFEFEFEFEFEFEFE;
const U64 NOT_FILE_H_MASK = 0x7F7F7F7F7F7F7F7F;

U64 rookAttacks(Board* position, int sq) {
    U64 occ = position->occupiedBB | 0x8000000000000001;
    int n = peekBit(occ & (bitRays[NORTH][sq] | setMask[63]));
    int e = peekBit(occ & (bitRays[EAST][sq] | setMask[63]));
    int s = peekBitReverse(occ & (bitRays[SOUTH][sq] | setMask[0]));
    int w = peekBitReverse(occ & (bitRays[WEST][sq] | setMask[0]));
    return rookAttacksEmpty[sq] ^ bitRays[NORTH][n] ^ bitRays[EAST][e] ^ bitRays[SOUTH][s] ^ bitRays[WEST][w];
}

U64 bishopAttacks(Board* position, int sq) {
    U64 occ = position->occupiedBB | 0x8000000000000001;
    int nw = peekBit(occ & (bitRays[NORTH_WEST][sq] | setMask[63]));
    int ne = peekBit(occ & (bitRays[NORTH_EAST][sq] | setMask[63]));
    int sw = peekBitReverse(occ & (bitRays[SOUTH_WEST][sq] | setMask[0]));
    int se = peekBitReverse(occ & (bitRays[SOUTH_EAST][sq] | setMask[0]));
    return bishopAttacksEmpty[sq] ^ bitRays[NORTH_WEST][nw] ^ bitRays[NORTH_EAST][ne] ^ bitRays[SOUTH_WEST][sw] ^ bitRays[SOUTH_EAST][se];
}

int attackedByWhite(Board* position, int sq) {
    if (knightAttacks[sq] & position->pieceBB[WHITE_KNIGHT]) return 1;
    if (bishopAttacks(position, sq) & (position->pieceBB[WHITE_BISHOP] | position->pieceBB[WHITE_QUEEN])) return 1;
    if (rookAttacks(position, sq) & (position->pieceBB[WHITE_ROOK] | position->pieceBB[WHITE_QUEEN])) return 1;
    if ((((position->pieceBB[WHITE_PAWN] << 7) & NOT_FILE_H_MASK) | ((position->pieceBB[WHITE_PAWN] << 9) & NOT_FILE_A_MASK)) & setMask[sq]) return 1;
    if (kingAttacks[sq] & position->pieceBB[WHITE_KING]) return 1;
    return 0;
}

int attackedByBlack(Board* position, int sq) {
    if (knightAttacks[sq] & position->pieceBB[BLACK_KNIGHT]) return 1;
    if (bishopAttacks(position, sq) & (position->pieceBB[BLACK_BISHOP] | position->pieceBB[BLACK_QUEEN])) return 1;
    if (rookAttacks(position, sq) & (position->pieceBB[BLACK_ROOK] | position->pieceBB[BLACK_QUEEN])) return 1;
    if ((((position->pieceBB[BLACK_PAWN] >> 9)& NOT_FILE_H_MASK) | ((position->pieceBB[BLACK_PAWN] >> 7)& NOT_FILE_A_MASK))& setMask[sq]) return 1;
    if (kingAttacks[sq] & position->pieceBB[BLACK_KING]) return 1;
    return 0;
}

int underCheck(Board* position, int side) {
    if (side == WHITE) return attackedByBlack(position, peekBit(position->pieceBB[WHITE_KING]));
    else return attackedByWhite(position, peekBit(position->pieceBB[BLACK_KING]));
}

char* PrSq(const int sq);
char* PrMove(const int move);

void debugMove(int move) {
    string names[14] = { "none","error","white pawn","white knight","white bishop","white rook","white queen","white king","black pawn","black knight","black bishop","black rook","black queen","black king" };

    int f = from(move);
    int t = to(move);
    int moving = moving(move);
    int captured = captured(move);
    int promoted = promoted(move);

    printf("move %s (%d) details:\n", PrMove(move), move);
    printf("from %d, to %d\n", f, t);
    char* pf = PrSq(f);
    printf("pf %s\n", pf);
    char* pt = PrSq(t);
    printf("pt %s\n", pt);
    printf("moving: %s\n", names[moving]);
    printf("captured: %s\n", names[captured]);
    printf("promoted: %s\n", names[promoted]);
    printf("flags: \n");
    if (isCapture(move)) printf("capture \n");
    if (isPromote(move)) printf("promote \n");
    if (isCastle(move)) printf("castle \n");
    if (isPawnStart(move)) printf("pawn_start \n");
    if (isEnPassant(move)) printf("en_passant \n");
}

char* PrSq(const int sq) {

    static char SqStr[3];

    int file = fileArray[sq];
    int rank = rankArray[sq];

    sprintf(SqStr, "%c%c", ('a' + file), ('1' + rank));

    return SqStr;

}

char* PrMove(const int move) {

    static char MvStr[6];

    int ff = fileArray[from(move)];
    int rf = rankArray[from(move)];
    int ft = fileArray[to(move)];
    int rt = rankArray[to(move)];

    int promoted = isPromote(move);

    if (promoted) {
        char pchar = 'q';
        int promotedPiece = promoted(move);
        if ((promotedPiece == WHITE_KNIGHT) || (promotedPiece == BLACK_KNIGHT)) {
            pchar = 'n';
        }
        else if ((promotedPiece == WHITE_ROOK) || (promotedPiece == BLACK_ROOK)) {
            pchar = 'r';
        }
        else if ((promotedPiece == WHITE_BISHOP) || (promotedPiece == BLACK_BISHOP)) {
            pchar = 'b';
        }
        sprintf(MvStr, "%c%c%c%c%c", ('a' + ff), ('1' + rf), ('a' + ft), ('1' + rt), pchar);
    }
    else {
        sprintf(MvStr, "%c%c%c%c", ('a' + ff), ('1' + rf), ('a' + ft), ('1' + rt));
    }

    return MvStr;
}

void printMoveList(const MoveList list) {
    int index = 0;
    int score = 0;
    int move = 0;
    printf("MoveList:\n");

    for (index = 0; index < list.count; ++index) {

        move = list.moves[index].move;
        score = list.moves[index].score;

        printf("Move:%d > %s (score:%d)\n", index + 1, PrMove(move), score);
    }
    printf("MoveList Total %d Moves:\n\n", list.count);
}

void unmakeMove(Board* position);
void GenAllMoves(Board* position, MoveList* list);

int moveExists(Board* position, const int move) {
    MoveList list[1];
    GenAllMoves(position, list);

    for (int i = 0; i < list->count; i++) {
        if (list->moves[i].move == move) {
            int validMove = makeMove(position, list->moves[i].move);
            unmakeMove(position);
            return validMove;
        }
    }
    return 0;
}

//make move

const int castlePermissionsBoard[64] = {
    13, 15, 15, 15, 12, 15, 15, 14,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    7, 15, 15, 15,  3, 15, 15, 11
};

#define HASH_PIECE(piece, square) (position->key ^= pieceKeys[(piece)][(square)])
#define HASH_CASTLE (position->key ^= castleKeys[position->castlePermissions])
#define HASH_SIDE (position->key ^= sideKey)
#define HASH_EN_PASSANT (position->key ^= enPassantKeys[position->enPassantSquare])

void movePiece(Board* position, int piece, int from, int to) {
    position->pieceBB[piece] ^= ((1ULL << from) | (1ULL << to));
    HASH_PIECE(piece, from);
    HASH_PIECE(piece, to);
    position->pieceBB[position->side] ^= ((1ULL << from) | (1ULL << to));
}

void flipPiece(Board* position, int piece, int square, int side) {
    position->pieceBB[piece] ^= (1ULL << square);
    HASH_PIECE(piece, square);
    position->pieceBB[side] ^= (1ULL << square);
}

void unmakeMove(Board* position) {
    position->gamePly--;
    position->ply--;

    position->side ^= 1;

    // Update castling rights, 50mr, en passant
    position->enPassantSquare = history(0).enPassantSquare;
    position->fiftyMoveCounter = history(0).fiftyMoveCounter;
    position->castlePermissions = history(0).castlePermissions;
    
    Move move = history(0).move;
    int from = from(move);
    int to = to(move);
    int movingPiece = moving(move);

    if (isPromote(move)) {
        int promotedPiece = promoted(move);
        position->pieceBB[promotedPiece] ^= (1ULL << to);
        position->pieceBB[movingPiece] ^= (1ULL << to);
    }

    position->pieceBB[movingPiece] ^= ((1ULL << from) | (1ULL << to));
    position->pieceBB[position->side] ^= ((1ULL << from) | (1ULL << to));

    if (isCapture(move)) {
        int target = to;
        if (isEnPassant(move)) {
            if (position->side == WHITE) target = to - 8;
            else target = to + 8;
        }
        int capturedPiece = captured(move);
        position->pieceBB[capturedPiece] ^= (1ULL << target);
        position->pieceBB[position->side ^ 1] ^= (1ULL << target);
    }
    else if (isCastle(move)) {
        switch (to) {
        case G1: position->pieceBB[WHITE_ROOK] ^= ((1ULL << H1) | (1ULL << F1));
            position->pieceBB[WHITE] ^= ((1ULL << H1) | (1ULL << F1));
            position->hasCastled[0] = 0;
            break;
        case C1: position->pieceBB[WHITE_ROOK] ^= ((1ULL << A1) | (1ULL << D1));
            position->pieceBB[WHITE] ^= ((1ULL << A1) | (1ULL << D1));
            position->hasCastled[0] = 0;
            break;
        case G8: position->pieceBB[BLACK_ROOK] ^= ((1ULL << H8) | (1ULL << F8));
            position->pieceBB[BLACK] ^= ((1ULL << H8) | (1ULL << F8));
            position->hasCastled[1] = 0;
            break;
        case C8: position->pieceBB[BLACK_ROOK] ^= ((1ULL << A8) | (1ULL << D8));
            position->pieceBB[BLACK] ^= ((1ULL << A8) | (1ULL << D8));
            position->hasCastled[1] = 0;
            break;
        }
    }

    position->occupiedBB = position->pieceBB[WHITE] | position->pieceBB[BLACK];
    position->emptyBB = ~position->occupiedBB;

    position->key = history(0).posKey;
}

int makeMove(Board* position, int move) {

    int from = from(move);
    int to = to(move);
    int movingPiece = moving(move);

    history(0).posKey = position->key;
    history(0).move = move;
    history(0).enPassantSquare = position->enPassantSquare;
    history(0).fiftyMoveCounter = position->fiftyMoveCounter;
    history(0).castlePermissions = position->castlePermissions;

    position->gamePly++;
    position->ply++;
    position->fiftyMoveCounter++;

    if (position->enPassantSquare < 64) {
        HASH_EN_PASSANT;
        position->enPassantSquare = 64;
    }

    HASH_CASTLE;
    position->castlePermissions &= castlePermissionsBoard[from] & castlePermissionsBoard[to];
    HASH_CASTLE;

    movePiece(position, movingPiece, from, to);

    if (isCapture(move)) {

        position->fiftyMoveCounter = 0;
        int capturedPiece = captured(move);

        int target = to;
        if (isEnPassant(move)) {
            if (position->side == WHITE) target = to - 8;
            else target = to + 8;
        }
        else if (isPromote(move)) {

            int promotedPiece = promoted(move);
            flipPiece(position, movingPiece, to, position->side);
            flipPiece(position, promotedPiece, to, position->side);
        }
        flipPiece(position, capturedPiece, target, position->side ^ 1);
    }
    else if (isCastle(move)) {

        switch (to) {
        case G1: movePiece(position, WHITE_ROOK, H1, F1); position->hasCastled[0] = 1; break;
        case C1: movePiece(position, WHITE_ROOK, A1, D1); position->hasCastled[0] = 1; break;
        case G8: movePiece(position, BLACK_ROOK, H8, F8); position->hasCastled[1] = 1; break;
        case C8: movePiece(position, BLACK_ROOK, A8, D8); position->hasCastled[1] = 1; break;
        }
    }
    else if (isPawnStart(move)) {

        position->fiftyMoveCounter = 0;
        position->enPassantSquare = (from + to) / 2;
        HASH_EN_PASSANT;
    }
    else if (isPromote(move)) {

        position->fiftyMoveCounter = 0;
        int promotedPiece = promoted(move);
        flipPiece(position, movingPiece, to, position->side);
        flipPiece(position, promotedPiece, to, position->side);
    }
    else if (movingPiece == WHITE_PAWN || movingPiece == BLACK_PAWN) {
        position->fiftyMoveCounter = 0;
    }

    position->occupiedBB = position->pieceBB[WHITE] | position->pieceBB[BLACK];
    position->emptyBB = ~position->occupiedBB;

    position->side ^= 1;
    HASH_SIDE;

    if (underCheck(position, position->side ^ 1)) {
        unmakeMove(position);
        return 0;
    }
    return 1;
}

void MakeNullMove(Board* position) {

    position->ply++;
    history(0).posKey = position->key;

    if (position->enPassantSquare != 64) HASH_EN_PASSANT;

    history(0).move = NOMOVE;
    history(0).fiftyMoveCounter = position->fiftyMoveCounter;
    history(0).enPassantSquare = position->enPassantSquare;
    history(0).castlePermissions = position->castlePermissions;
    position->enPassantSquare = 64;

    position->side ^= 1;
    position->gamePly++;
    HASH_SIDE;

    return;
}

void TakeNullMove(Board* position) {

    position->gamePly--;
    position->ply--;

    if (position->enPassantSquare != 64) HASH_EN_PASSANT;

    position->castlePermissions = history(0).castlePermissions;
    position->fiftyMoveCounter = history(0).fiftyMoveCounter;
    position->enPassantSquare = history(0).enPassantSquare;

    if (position->enPassantSquare != 64) HASH_EN_PASSANT;
    position->side ^= 1;
    HASH_SIDE;
}

//hash table

#define MINHASH 4
#define MAXHASH 16384
#define DEFAULTHASH 32

#define PRI_SIZET PRIu64

enum { BOUND_NONE, BOUND_UPPER, BOUND_LOWER, BOUND_EXACT };

typedef struct {

    void* mem;
    TTEntry* table;
    size_t count;
    size_t currentMB;
    size_t requestedMB;
    bool dirty;

} TranspositionTable;


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
TTEntry* ProbeTT(Key posKey, bool* ttHit) {

    TTEntry* tte = GetEntry(posKey);

    *ttHit = tte->posKey == posKey;

    return tte;
}

// Store an entry in the transposition table
void StoreTTEntry(TTEntry* tte, Key posKey, Move move, int score, Depth depth, int bound) {

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

//evaluate

int Lsb(const U64 bb) {
    unsigned long index = -1;
    _BitScanForward64(&index, bb);
    return index;
}

// Returns the index of the least significant bit and unsets it
int PopBit(U64* bb) {

    int lsb = Lsb(*bb);
    *bb &= (*bb - 1);

    return lsb;
}

bool testBit(U64 b, int i) {
    return b & (1ULL << i);
}

#define MG 0
#define EG 1
#define PhaseNb 2
#define RANK_NB 8

// [PHASE][DEFENDED]
const int KnightOutpostValues[PhaseNb][2] = { {20, 40}, {10, 20} };
const int BishopOutpostValues[PhaseNb][2] = { {15, 30}, { 3,  5} };

// [PHASE][CAN_ADVANCE][SAFE_ADVANCE][RANK]
const int PassedPawn[2][2][RANK_NB][PhaseNb] = {
  {{{   0,   0}, { -33, -30}, { -24,   8}, { -13,  -2}, {  24,   0}, {  66,  -5}, { 160,  32}, {   0,   0}},
   {{   0,   0}, {  -2,   1}, { -14,  23}, { -15,  35}, {   7,  44}, {  72,  60}, { 194, 129}, {   0,   0}}},
  {{{   0,   0}, {  -7,  12}, { -12,   6}, { -10,  27}, {  27,  32}, {  86,  63}, { 230, 149}, {   0,   0}},
   {{   0,   0}, {  -5,   8}, { -12,  17}, { -21,  52}, { -14, 109}, {  28, 202}, { 119, 369}, {   0,   0}}},
};

const int KnightAttackedByPawn[PhaseNb] = { -49, -32 };

const int KnightOutpost[2][PHASE_NB] = { {19, -34}, {38, 9} };

const int KnightBehindPawn[2] = { 4, 18 };

const int KnightMobility[9][PHASE_NB] = {
    { -86, -98}, { -37, -84}, { -15, -40},
    {  -5, -11}, {   3, -13}, {   8,   0},
    {  18,  -1}, {  32,  -1}, {  48, -30},
};

const int BishopAttackedByPawn[PhaseNb] = { -52, -34 };

const int BishopOutpost[2][PHASE_NB] = { {  20, -16}, {  53, -10} };

const int BishopMobility[14][PHASE_NB] = {
    { -58,-120}, { -47, -63}, { -19, -45}, {  -5, -21},
    {   4,  -7}, {  16,   0}, {  22,   8}, {  29,   4},
    {  30,  10}, {  36,   4}, {  42,   4}, {  53, -11},
    {  41,  -1}, {  34, -28},
};

const int BishopBehindPawn[2] = { 3, 13 };

const int RookMobility[15][PHASE_NB] = {
    {-148, -88}, { -69,-119}, { -16, -66}, {  -9, -26},
    {  -8,  -3}, {  -8,  15}, {  -7,  26}, {  -3,  32},
    {   0,  37}, {   6,  36}, {   9,  42}, {  19,  48},
    {  19,  50}, {  24,  47}, {  16,  44},
};

const int QueenChecked[PhaseNb] = { -35, -32 };

const int QueenCheckedByPawn[PhaseNb] = { -49, -45 };

const int QueenMobility[28][PHASE_NB] = {
    { -60,-258}, {-169,-232}, { -39,-187}, { -35,-174},
    { -19,-118}, { -25, -60}, { -19, -89}, { -18, -86},
    { -15, -58}, { -10, -53}, {  -9, -27}, {  -6, -29},
    {  -4, -15}, {  -2, -11}, {   1,  -8}, {   0,   4},
    {   2,  15}, {   0,  14}, {  11,  25}, {   0,  26},
    {   5,  29}, {  15,  29}, {  23,  14}, {  36,  17},
    {  51,  25}, {  47,   1}, {  -5,   1}, {  24,  13},
};

const int BishopHasWings[PhaseNb] = { 13, 36 };

const int BishopPair[PhaseNb] = { 40, 69 };

const int Tempo[PhaseNb] = { 5, 7 };

int SafetyTable[100] = { // Taken from CPW / Stockfish
    0,  0,   1,   2,   3,   5,   7,   9,  12,  15,
  18,  22,  26,  30,  35,  39,  44,  50,  56,  62,
  68,  75,  82,  85,  89,  97, 105, 113, 122, 131,
 140, 150, 169, 180, 191, 202, 213, 225, 237, 248,
 260, 272, 283, 295, 307, 319, 330, 342, 354, 366,
 377, 389, 401, 412, 424, 436, 448, 459, 471, 483,
 494, 500, 500, 500, 500, 500, 500, 500, 500, 500,
 500, 500, 500, 500, 500, 500, 500, 500, 500, 500,
 500, 500, 500, 500, 500, 500, 500, 500, 500, 500,
 500, 500, 500, 500, 500, 500, 500, 500, 500, 500
};

int PawnConnected[2][64] = {
    { 0, 0, 0, 0, 0, 0, 0, 0,
      2, 2, 2, 3, 3, 2, 2, 2,
      4, 4, 5, 6, 6, 5, 4, 4,
      7, 8,10,12,12,10, 8, 7,
     11,14,17,21,21,17,14,11,
     16,21,25,33,33,25,12,16,
     32,42,50,55,55,50,42,32,
      0, 0, 0, 0, 0, 0, 0, 0, },

    { 0, 0, 0, 0, 0, 0, 0, 0,
     32,42,50,55,55,50,42,32,
     16,21,25,33,33,25,12,16,
     11,14,17,21,21,17,14,11,
      7, 8,10,12,12,10, 8, 7,
      4, 4, 5, 6, 6, 5, 4, 4,
      2, 2, 2, 3, 3, 2, 2, 2,
      0, 0, 0, 0, 0, 0, 0, 0, }
};

#define ColourNb 2

#define FILEA 0x0101010101010101
#define FILEB 0x0202020202020202
#define FILEC 0x0404040404040404
#define FILED 0x0808080808080808
#define FILEE 0x1010101010101010
#define FILEF 0x2020202020202020
#define FILEG 0x4040404040404040
#define FILEH 0x8080808080808080

#define LEFT_WING  (FILE_A | FILE_B | FILE_C)
#define RIGHT_WING (FILE_F | FILE_G | FILE_H)

#define WHITE_SQUARES 0x55AA55AA55AA55AA
#define BLACK_SQUARES 0xAA55AA55AA55AA55

#define KING_HAS_CASTLED     (25)
#define KING_CAN_CASTLE      (10)

#define ROOK_OPEN_FILE_MID   (35)
#define ROOK_OPEN_FILE_END   (20)
#define ROOK_SEMI_FILE_MID   (12)
#define ROOK_SEMI_FILE_END   (12)
#define ROOK_ON_7TH_MID      (0)
#define ROOK_ON_7TH_END      (23)

#define PAWN_STACKED_MID     (10)
#define PAWN_STACKED_END     (32)
#define PAWN_ISOLATED_MID    (3)
#define PAWN_ISOLATED_END    (4)

const int PawnBackwards[2][2] = { {7, -3}, {-11, -11} };
const int KnightRammedPawns[2] = { 0, 5 };
const int BishopRammedPawns[2] = { -11, -12 };

const int KingDefenders[12][PHASE_NB] = {
    { -39,  -4}, { -23,   5}, {   0,   1}, {  10,  -1},
    {  25,  -1}, {  36,   3}, {  39,   7}, {  33, -76},
    {  12,   6}, {  12,   6}, {  12,   6}, {  12,   6},
};

int pieceValues[5][2] = {
    {100, 121}, {459, 390}, {465, 412}, {630, 711}, {1272, 1317}
};

//KingShelter[2][FILE_NB][RANK_NB][PHASE_NB]

const int KingShelter[2][8][RANK_NB][PHASE_NB] = {
  {{{ -15,  17}, {   6,  -8}, {  14,   4}, {  19,   5}, {   3,   0}, {  10,  -2}, { -19, -41}, { -33,   1}},
   {{   2,   7}, {  16,  -5}, {  17,  -8}, {   0, -11}, { -35,  -3}, { -74,  85}, {  47,  73}, { -34,   0}},
   {{  14,  14}, {   9,   0}, { -17,   0}, { -11,   0}, { -31,  -2}, {   9, -14}, { -40,  53}, { -16,   1}},
   {{  14,  26}, {  16,   0}, {  -3,  -8}, {  18, -12}, {  16, -31}, { -26, -33}, { -96,  19}, {  -2,   0}},
   {{  -8,  18}, {   1,   1}, { -27,   1}, { -14,   2}, { -39, -15}, { -34, -23}, {   2,   0}, { -15,   0}},
   {{  22,   2}, {  18,  -4}, { -18,  -1}, {  -3, -20}, {   3, -32}, {  14, -47}, {  70, -36}, { -15,   0}},
   {{  20,   1}, {   3,  -9}, { -26,  -9}, { -21, -13}, { -25, -16}, { -46,  -1}, { -29,  32}, { -32,   9}},
   {{ -13,  -3}, {   0,  -8}, {   5,   0}, {   1,   3}, { -14,  11}, {  -3,  30}, {-136,  69}, { -19,  15}}},
  {{{   0,   0}, {  -1, -17}, {   2, -17}, { -63,  13}, {  14, -14}, { -30,  30}, {-136,  19}, { -58,   9}},
   {{   0,   0}, {  16,  -5}, {   6,  -5}, {  -1,  -4}, {   6, -25}, {   2,  81}, {-197,  28}, { -46,   3}},
   {{   0,   0}, {  24,   1}, {   2,  -5}, {  19, -25}, {  13,  -4}, { -94,  47}, {-133, -84}, { -21,  -1}},
   {{   0,   0}, {  -2,   9}, {  -6,  13}, { -17,   0}, { -29,  -5}, {-105,  -1}, {  29, -22}, { -25,   0}},
   {{   0,   0}, {   6,   4}, {   9,  -7}, {  21,  -6}, {   7, -18}, { -52,  13}, { -67, -92}, {  -6,  -3}},
   {{   0,   0}, {  10,   1}, { -10,  -3}, { -22, -13}, {  11, -32}, { -36,   2}, {  -6,  21}, { -30,   0}},
   {{   0,   0}, {  13,  -1}, {  -1,   0}, { -19,  -6}, { -25, -16}, {   9,  -5}, { -93, -44}, { -46,  13}},
   {{   0,   0}, {   8, -27}, {  10, -14}, { -26,   1}, { -32,  -2}, {   1, -24}, {-187, -53}, { -47,  17}}},
};

int distanceBetween(int s1, int s2) {
    return DistanceBetween[s1][s2];
}

int evaluatePosition(Board* position) {

    uint64_t white = position->pieceBB[WHITE];
    uint64_t black = position->pieceBB[BLACK];
    uint64_t pawns = (position->pieceBB[WHITE_PAWN] | position->pieceBB[BLACK_PAWN]);
    uint64_t knights = (position->pieceBB[WHITE_KNIGHT] | position->pieceBB[BLACK_KNIGHT]);
    uint64_t bishops = (position->pieceBB[WHITE_BISHOP] | position->pieceBB[BLACK_BISHOP]);
    uint64_t rooks = (position->pieceBB[WHITE_ROOK] | position->pieceBB[BLACK_ROOK]);
    uint64_t queens = (position->pieceBB[WHITE_QUEEN] | position->pieceBB[BLACK_QUEEN]);
    uint64_t kings = (position->pieceBB[WHITE_KING] | position->pieceBB[BLACK_KING]);

    // Check for recognized draws
    if ((pawns | rooks | queens) == 0ULL) {

        // K v K
        if (kings == (white | black))
            return 0;

        if ((white & kings) == white) {

            // K vs K+B or K vs K+N
            if (bitCount(black & (knights | bishops)) <= 1)
                return 0;

            // K vs K+N+N
            if (bitCount(black & knights) == 2 && (black & bishops) == 0ULL)
                return 0;
        }

        if ((black & kings) == black) {

            // K+B vs K or K+N vs K
            if (bitCount(white & (knights | bishops)) <= 1)
                return 0;

            // K+N+N vs K
            if (bitCount(white & knights) == 2 && (white & bishops) == 0ULL)
                return 0;
        }
    }

    return evaluatePieces(position);
}

#define kingAttacks(sq, tg) (KingMap[(sq)] & (tg))

U64 pawnAdvance(U64 pawns, U64 occupied, int color) {
    return ~occupied & (color == WHITE ? (pawns << 8) : (pawns >> 8));
}

int evaluatePieces(Board* position) {

    uint64_t white = position->pieceBB[WHITE];
    uint64_t black = position->pieceBB[BLACK];
    uint64_t pawns = (position->pieceBB[WHITE_PAWN] | position->pieceBB[BLACK_PAWN]);
    uint64_t knights = (position->pieceBB[WHITE_KNIGHT] | position->pieceBB[BLACK_KNIGHT]);
    uint64_t bishops = (position->pieceBB[WHITE_BISHOP] | position->pieceBB[BLACK_BISHOP]);
    uint64_t rooks = (position->pieceBB[WHITE_ROOK] | position->pieceBB[BLACK_ROOK]);
    uint64_t queens = (position->pieceBB[WHITE_QUEEN] | position->pieceBB[BLACK_QUEEN]);
    uint64_t kings = (position->pieceBB[WHITE_KING] | position->pieceBB[BLACK_KING]);

    uint64_t myPieces, myPawns, enemyPawns, passedPawns = 0ULL;
    uint64_t tempPawns, tempKnights, tempBishops, tempRooks, tempQueens;
    uint64_t occupiedMinusMyBishops, occupiedMinusMyRooks;
    uint64_t attacks, mobilityArea, destination, defenders;

    int mg = 0, eg = 0;
    //int pawnmg = 0, pawneg = 0;
    int eval, curPhase;
    int mobilityCount, defended;
    int colour, bit, rank, semi;
    int canAdvance, safeAdvance, count;

    U64 whiteKingBitboard = (position->pieceBB[WHITE_KING]);
    int wKingSq = PopBit(&whiteKingBitboard);
    U64 blackKingBitboard = (position->pieceBB[BLACK_KING]);
    int bKingSq = PopBit(&blackKingBitboard);

    mg += PSQTMidgame[WHITE_KING][wKingSq];
    eg += PSQTEndgame[WHITE_KING][wKingSq];

    mg -= PSQTMidgame[BLACK_KING][bKingSq];
    eg -= PSQTEndgame[BLACK_KING][bKingSq];

    uint64_t whitePawns = white & pawns;
    uint64_t blackPawns = black & pawns;
    uint64_t notEmpty = white | black;

    uint64_t pawnAttacks[ColourNb] = {
        (whitePawns << 9 & ~FILEA) | (whitePawns << 7 & ~FILEH),
        (blackPawns >> 9 & ~FILEH) | (blackPawns >> 7 & ~FILEA)
    };

    uint64_t blockedPawns[ColourNb] = {
        (whitePawns << 8 & black) >> 8,
        (blackPawns >> 8 & white) << 8,
    };

    uint64_t kingAreas[ColourNb] = {
        KingMap[wKingSq] | (1ULL << wKingSq) | (KingMap[wKingSq] << 8),
        KingMap[bKingSq] | (1ULL << bKingSq) | (KingMap[bKingSq] >> 8)
    };

    U64 allAttackBoards[2];
    U64 attackedNoQueen[2];

    allAttackBoards[WHITE] = attackedNoQueen[WHITE] = kingAttacks(wKingSq, ~0ULL);
    allAttackBoards[BLACK] = attackedNoQueen[BLACK] = kingAttacks(bKingSq, ~0ULL);

    U64 rammedPawns[2] = {
        pawnAdvance(blackPawns, ~whitePawns, BLACK),
        pawnAdvance(whitePawns, ~blackPawns, WHITE)
    };
    
    int attackCounts[ColourNb] = { 0, 0 };
    int attackerCounts[ColourNb] = { 0, 0 };

    for (colour = BLACK; colour >= WHITE; colour--) {

        // Negate the scores so that the scores are from
        // White's perspective after the loop completes
        mg = -mg;
        eg = -eg;

        myPieces = position->pieceBB[colour];
        myPawns = myPieces & pawns;
        enemyPawns = pawns ^ myPawns;

        tempPawns = myPawns;
        tempKnights = myPieces & knights;
        tempBishops = myPieces & bishops;
        tempRooks = myPieces & rooks;
        tempQueens = myPieces & queens;

        occupiedMinusMyBishops = notEmpty ^ (myPieces & (bishops | queens));
        occupiedMinusMyRooks = notEmpty ^ (myPieces & (rooks | queens));

        // Don't include squares that are attacked by enemy pawns, 
        // occupied by our king, or occupied with our blocked pawns
        // in our mobilityArea. This definition of mobilityArea is
        // derived directly from Stockfish's evaluation features. 
        mobilityArea = ~(
            pawnAttacks[!colour] | (myPieces & kings) | blockedPawns[colour]
            );

        // Bishop gains a bonus for pawn wings
        if (tempBishops && (myPawns & LEFT_WING) && (myPawns & RIGHT_WING)) {
            mg += BishopHasWings[MG];
            eg += BishopHasWings[EG];
        }

        // Bishop gains a bonus for being in a pair
        if ((tempBishops & WHITE_SQUARES) && (tempBishops & BLACK_SQUARES)) {
            mg += BishopPair[MG];
            eg += BishopPair[EG];
        }

        // King gains a bonus if it has castled since the root
        if (position->hasCastled[colour]) {
            mg += KING_HAS_CASTLED;
            eg += KING_HAS_CASTLED;
        }

        // King gains a bonus if it still may castle
        else if (position->castlePermissions & (3 << (2 * colour))) {
            mg += KING_CAN_CASTLE;
            eg += KING_CAN_CASTLE;
        }

        if (colour == WHITE)
        {
            defenders = (position->pieceBB[WHITE_PAWN])
                | (position->pieceBB[WHITE_KNIGHT])
                | (position->pieceBB[WHITE_BISHOP]);
        }
        else
        {
            defenders = (position->pieceBB[BLACK_PAWN])
                | (position->pieceBB[BLACK_KNIGHT])
                | (position->pieceBB[BLACK_BISHOP]);
        }

        // Get the attack board for the pawns
        attacks = pawnAttacks[colour] & kingAreas[!colour];
        allAttackBoards[colour] |= pawnAttacks[colour];

        // Update the counters for the safety evaluation
        if (attacks) {
            attackCounts[colour] += 2 * bitCount(attacks);
            attackerCounts[colour] += 1;
        }

        const int forward = (colour == WHITE) ? 8 : -8;

        while (tempPawns) {

            int bit = PopBit(&tempPawns);

            if (colour == WHITE)
            {
                mg += PSQTMidgame[WHITE_PAWN][bit];
                eg += PSQTEndgame[WHITE_PAWN][bit];
            }
            else
            {
                mg += PSQTMidgame[BLACK_PAWN][bit];
                eg += PSQTEndgame[BLACK_PAWN][bit];
            }

            // Save the fact that this pawn is passed. We will
            // use it later in order to apply a proper bonus
            if (!(PassedPawnMasks[colour][bit] & enemyPawns))
                passedPawns |= (1ULL << bit);

            // Apply a penalty if the pawn is isolated
            if (!(IsolatedPawnMasks[bit] & tempPawns)) {
                mg -= PAWN_ISOLATED_MID;
                eg -= PAWN_ISOLATED_END;
            }

            // Apply a penalty if the pawn is stacked
            if (FILES[File(bit)] & tempPawns) {
                mg -= PAWN_STACKED_MID;
                eg -= PAWN_STACKED_END;
            }

            // Apply a penalty if the pawn is backward
            if (!(PassedPawnMasks[!colour][bit] & myPawns)
                && (pawnAttacks[!colour] & (1ULL << (bit + forward)))) {
                semi = !(FILES[fileOf(bit)] & enemyPawns);
                mg += PawnBackwards[semi][MG];
                eg += PawnBackwards[semi][EG];
            }

            // Apply a bonus if the pawn is connected
            if (PawnConnectedMasks[colour][bit] & myPawns) {
                mg += PawnConnected[colour][bit];
                eg += PawnConnected[colour][bit];
            }

        }

        while (tempKnights) {

            int bit = PopBit(&tempKnights);

            if (colour == WHITE)
            {
                mg += PSQTMidgame[WHITE_KNIGHT][bit];
                eg += PSQTEndgame[WHITE_KNIGHT][bit];
            }
            else
            {
                mg += PSQTMidgame[BLACK_KNIGHT][bit];
                eg += PSQTEndgame[BLACK_KNIGHT][bit];
            }

            attacks = knightAttacks[bit];
            allAttackBoards[colour] |= attacks;
            attackedNoQueen[colour] |= attacks;

            // Apply a penalty if the knight is being attacked by a pawn
            if (pawnAttacks[!colour] & (1ULL << bit)) {
                mg += KnightAttackedByPawn[MG];
                eg += KnightAttackedByPawn[EG];
            }

            // Knight is in an outpost square, unable to be
            // attacked by enemy pawns, on or between ranks
            // four through seven, relative to it's colour
            if (OutpostRanks[colour] & (1ULL << bit)
                && !(OutpostSquareMasks[colour][bit] & enemyPawns)) {

                defended = (pawnAttacks[colour] & (1ULL << bit)) != 0ULL;

                mg += KnightOutpost[defended][MG];
                eg += KnightOutpost[defended][EG];
            }

            // Apply a bonus if the knight is behind a pawn
            if (testBit(pawnAdvance((myPawns | enemyPawns), 0ULL, !colour), bit)) {
                mg += KnightBehindPawn[MG];
                eg += KnightBehindPawn[EG];
            }

            // Knight gains a mobility bonus based off of the number
            // of attacked or defended squares within the mobility area
            mobilityCount = bitCount((mobilityArea & attacks));
            mg += KnightMobility[mobilityCount][MG];
            eg += KnightMobility[mobilityCount][EG];

            // Get the attack counts for this Knight
            attacks = attacks & kingAreas[!colour];
            if (attacks) {
                attackCounts[colour] += 2 * bitCount(attacks);
                attackerCounts[colour]++;
            }
        }

        while (tempBishops) {

            int bit = PopBit(&tempBishops);

            if (colour == WHITE)
            {
                mg += PSQTMidgame[WHITE_BISHOP][bit];
                eg += PSQTEndgame[WHITE_BISHOP][bit];
            }
            else
            {
                mg += PSQTMidgame[BLACK_BISHOP][bit];
                eg += PSQTEndgame[BLACK_BISHOP][bit];
            }

            attacks = bishopAttacks(position, bit);
            allAttackBoards[colour] |= attacks;
            attackedNoQueen[colour] |= attacks;

            // Apply a penalty if the bishop is being attacked by a pawn
            if (pawnAttacks[!colour] & (1ULL << bit)) {
                mg += BishopAttackedByPawn[MG];
                eg += BishopAttackedByPawn[EG];
            }

            // Apply a penalty for the bishop based on number of rammed pawns
            // of our own colour, which reside on the same shade of square as the bishop
            count = bitCount(rammedPawns[colour] & (((1ULL << bit) & WHITE_SQUARES ? WHITE_SQUARES : BLACK_SQUARES)));
            mg += count * BishopRammedPawns[MG];
            eg += count * BishopRammedPawns[EG];

            // Bishop is in an outpost square, unable to be
            // attacked by enemy pawns, on or between ranks
            // four through seven, relative to it's colour
            if (OutpostRanks[colour] & (1ULL << bit)
                && !(OutpostSquareMasks[colour][bit] & enemyPawns)) {

                defended = (pawnAttacks[colour] & (1ULL << bit)) != 0ULL;

                mg += BishopOutpost[defended][MG];
                eg += BishopOutpost[defended][EG];
            }

            // Bishop gains a mobility bonus based off of the number
            // of attacked or defended squares within the mobility area
            mobilityCount = bitCount((mobilityArea & attacks));
            mg += BishopMobility[mobilityCount][MG];
            eg += BishopMobility[mobilityCount][EG];

            // Get the attack counts for this Bishop
            attacks = attacks & kingAreas[!colour];
            if (attacks) {
                attackCounts[colour] += 2 * bitCount(attacks);
                attackerCounts[colour]++;
            }
        }


        while (tempRooks) {

            int bit = PopBit(&tempRooks);

            if (colour == WHITE)
            {
                mg += PSQTMidgame[WHITE_ROOK][bit];
                eg += PSQTEndgame[WHITE_ROOK][bit];
            }
            else
            {
                mg += PSQTMidgame[BLACK_ROOK][bit];
                eg += PSQTEndgame[BLACK_ROOK][bit];
            }

            attacks = rookAttacks(position, bit);
            allAttackBoards[colour] |= attacks;
            attackedNoQueen[colour] |= attacks;

            // Rook is on a semi-open file if there are no
            // pawns of the Rook's colour on the file. If
            // there are no pawns at all, it is an open file
            if (!(myPawns & FILES[File(bit)])) {

                if (!(enemyPawns & FILES[File(bit)])) {
                    mg += ROOK_OPEN_FILE_MID;
                    eg += ROOK_OPEN_FILE_END;
                }

                else {
                    mg += ROOK_SEMI_FILE_MID;
                    eg += ROOK_SEMI_FILE_END;
                }
            }

            // Rook gains a bonus for being located
            // on seventh rank relative to its colour
            if (Rank(bit) == (colour == BLACK ? 1 : 6)) {
                mg += ROOK_ON_7TH_MID;
                eg += ROOK_ON_7TH_END;
            }

            // Rook gains a mobility bonus based off of the number
            // of attacked or defended squares within the mobility area
            mobilityCount = bitCount((mobilityArea & attacks));
            mg += RookMobility[mobilityCount][MG];
            eg += RookMobility[mobilityCount][EG];

            // Get the attack counts for this Rook
            attacks = attacks & kingAreas[!colour];
            if (attacks) {
                attackCounts[colour] += 3 * bitCount(attacks);
                attackerCounts[colour]++;
            }
        }


        while (tempQueens) {

            int bit = PopBit(&tempQueens);

            if (colour == WHITE)
            {
                mg += PSQTMidgame[WHITE_QUEEN][bit];
                eg += PSQTEndgame[WHITE_QUEEN][bit];
            }
            else
            {
                mg += PSQTMidgame[BLACK_QUEEN][bit];
                eg += PSQTEndgame[BLACK_QUEEN][bit];
            }

            // Generate the attack board
            attacks = (rookAttacks(position, bit) | bishopAttacks(position, bit));
            allAttackBoards[colour] |= attacks;

            // Apply a bonus if the queen is under an attack threat
            if ((1ULL << bit) & attackedNoQueen[!colour]) {
                eg += QueenChecked[MG];
                mg += QueenChecked[EG];
            }

            // Apply a penalty if the queen is under attack by a pawn
            if ((1ULL << bit) & pawnAttacks[!colour]) {
                eg += QueenCheckedByPawn[MG];
                mg += QueenCheckedByPawn[EG];
            }

            // Queen gains a mobility bonus based off of the number
            // of attacked or defended squares within the mobility area
            mobilityCount = bitCount((mobilityArea & attacks));
            mg += QueenMobility[mobilityCount][MG];
            eg += QueenMobility[mobilityCount][EG];

            // Get the attack counts for this Queen
            attacks = attacks & kingAreas[!colour];
            if (attacks) {
                attackCounts[colour] += 4 * bitCount(attacks);
                attackerCounts[colour]++;
            }
        }
    }

    // Evaluate the passed pawns for both colours
    for (colour = BLACK; colour >= WHITE; colour--) {

        // Negate the scores so that the scores are from
        // White's perspective after the loop completes
        mg = -mg; eg = -eg;

        tempPawns = position->pieceBB[colour] & passedPawns;

        while (tempPawns) {

            // Pop off the next Passed Pawn
            bit = PopBit(&tempPawns);

            // Determine the releative  rank
            rank = (colour == BLACK) ? (7 - Rank(bit)) : Rank(bit);

            // Determine where we would advance to
            destination = (colour == BLACK)
                ? ((1ULL << bit) >> 8)
                : ((1ULL << bit) << 8);

            canAdvance = (destination & notEmpty) == 0ULL;
            safeAdvance = (destination & allAttackBoards[!colour]) == 0ULL;

            mg += PassedPawn[canAdvance][safeAdvance][rank][MG];;
            eg += PassedPawn[canAdvance][safeAdvance][rank][EG];
        }
    }

    int kingSq, kingFile, kingRank, file, distance;
    U64 filePawns;

    for (colour = BLACK; colour >= WHITE; colour--) {

        // Negate the scores so that the scores are from
        // White's perspective after the loop completes
        mg = -mg; eg = -eg;

        if (attackerCounts[!colour] >= 2) {

            // Dont allow attack count to exceed 99
            if (attackCounts[!colour] >= 100)
                attackCounts[!colour] = 99;

            // Reduce attack count if there are no enemy queens 
            if (!(position->pieceBB[!colour] & queens))
                attackCounts[!colour] *= .5;

            // Reduce attack count if there are no enemy rooks
            if (!(position->pieceBB[!colour] & rooks))
                attackCounts[!colour] *= .8;

            mg -= SafetyTable[attackCounts[!colour]];
            eg -= SafetyTable[attackCounts[!colour]];
        }

        if (colour == WHITE)
        {
            kingSq = wKingSq;
        }
        else
        {
            kingSq = bKingSq;
        }
        kingFile = File(kingSq);
        kingRank = Rank(kingSq);

        myPawns = pawns & (position->pieceBB[colour]);

        // Evaluate Pawn Shelter. We will look at the King's file and any adjacent files
        // to the King's file. We evaluate the distance between the king and the most backward
        // pawn. We will not look at pawns behind the king, and will consider that as having
        // no pawn on the file. No pawn on a file is used with distance equals 7, as no pawn
        // can ever be a distance of 7 from the king. Different bonus is in order when we are
        // looking at the file on which the King sits.

        for (file = MAX(0, kingFile - 1); file <= MIN(7, kingFile + 1); file++) {

            filePawns = myPawns & FILES[file] & RanksAtOrAboveMasks[colour][kingRank];

            distance = filePawns ?
                colour == WHITE ? Rank(peekBit(filePawns)) - kingRank
                : kingRank - Rank(peekBitReverse(filePawns))
                : 7;

            mg += KingShelter[file == kingFile][file][distance][MG];
            eg += KingShelter[file == kingFile][file][distance][EG];
        }
    }

    mg += (position->side == WHITE) ? Tempo[MG] : -Tempo[MG];
    eg += (position->side == WHITE) ? Tempo[EG] : -Tempo[EG];

    int evalpiecesMG, evalpiecesEG;
    evalpiecesMG = (bitCount(position->pieceBB[WHITE_PAWN]) - bitCount(position->pieceBB[BLACK_PAWN])) * pieceValues[0][0];
    evalpiecesMG += (bitCount(position->pieceBB[WHITE_ROOK]) - bitCount(position->pieceBB[BLACK_ROOK])) * pieceValues[3][0];
    evalpiecesMG += (bitCount(position->pieceBB[WHITE_KNIGHT]) - bitCount(position->pieceBB[BLACK_KNIGHT])) * pieceValues[1][0];
    evalpiecesMG += (bitCount(position->pieceBB[WHITE_BISHOP]) - bitCount(position->pieceBB[BLACK_BISHOP])) * pieceValues[2][0];
    evalpiecesMG += (bitCount(position->pieceBB[WHITE_QUEEN]) - bitCount(position->pieceBB[BLACK_QUEEN])) * pieceValues[4][0];

    evalpiecesEG = (bitCount(position->pieceBB[WHITE_PAWN]) - bitCount(position->pieceBB[BLACK_PAWN])) * pieceValues[0][1];
    evalpiecesEG += (bitCount(position->pieceBB[WHITE_ROOK]) - bitCount(position->pieceBB[BLACK_ROOK])) * pieceValues[3][1];
    evalpiecesEG += (bitCount(position->pieceBB[WHITE_KNIGHT]) - bitCount(position->pieceBB[BLACK_KNIGHT])) * pieceValues[1][1];
    evalpiecesEG += (bitCount(position->pieceBB[WHITE_BISHOP]) - bitCount(position->pieceBB[BLACK_BISHOP])) * pieceValues[2][1];
    evalpiecesEG += (bitCount(position->pieceBB[WHITE_QUEEN]) - bitCount(position->pieceBB[BLACK_QUEEN])) * pieceValues[4][1];
    
    mg = mg + evalpiecesMG;
    eg = eg + evalpiecesEG;

    curPhase = 24 - (bitCount(knights | bishops))
        - (bitCount(rooks) << 1)
        - (bitCount(queens) << 2);

    curPhase = (curPhase * 256 + 12) / 24;

    eval = ((mg * (256 - curPhase)) + (eg * curPhase)) / 256;

    return position->side == WHITE ? eval : -eval;
}

//board functions

U64 generateHashKey(Board* position) {
    U64 key = 0ULL;
    for (int i = 2; i < 14; i++) {
        U64 bb = position->pieceBB[i];
        for (int j = 0; j < 64; j++) {
            if (bb & setMask[j]) key ^= pieceKeys[i][j];
        }
    }
    if (position->side == WHITE) key ^= sideKey;
    if (position->enPassantSquare < 64) key ^= enPassantKeys[position->enPassantSquare];
    key ^= castleKeys[position->castlePermissions];
    return key;
}

// Calculates the position key after a move. Fails
// for special moves.
Key KeyAfter(const Board* position, const Move move) {

    int from = from(move);
    int to = to(move);
    int piece = moving(move);
    int capt = captured(move);
    Key key = position->key ^ sideKey;

    if (capt)
        key ^= pieceKeys[capt][to];

    return key ^ pieceKeys[piece][from] ^ pieceKeys[piece][to];
}

void initBoard(Board* position, char* fen)
{
    for (int i = 0; i < 14; i++)
    {
        position->pieceBB[i] = 0ULL;
    }

    position->ply = 0;
    position->gamePly = 0;

    position->enPassantSquare = 64;
    position->fiftyMoveCounter = 0;

    position->key = 0ULL;

    int rank = RANK_8;
    int file = FILE_A;
    int piece = -1;
    int count = 0;
    int i = 0;

    while ((rank >= RANK_1) && *fen)
    {
        count = 1;
        switch (*fen)
        {
        case 'r': piece = BLACK_ROOK; break;
        case 'n': piece = BLACK_KNIGHT; break;
        case 'b': piece = BLACK_BISHOP; break;
        case 'q': piece = BLACK_QUEEN; break;
        case 'k': piece = BLACK_KING; break;
        case 'p': piece = BLACK_PAWN; break;
        case 'R': piece = WHITE_ROOK; break;
        case 'N': piece = WHITE_KNIGHT; break;
        case 'B': piece = WHITE_BISHOP; break;
        case 'Q': piece = WHITE_QUEEN; break;
        case 'K': piece = WHITE_KING; break;
        case 'P': piece = WHITE_PAWN; break;

        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
            piece = -1;
            count = *fen - '0';
            break;

        case '/':
        case ' ':
            rank--;
            file = FILE_A;
            fen++;
            continue;

        default:
            printf("FEN error \n");
            fflush(stdout);
            return;
        }

        for (i = 0; i < count; i++)
        {
            if (piece > 0)
            {
                setBit(position->pieceBB[piece], getSquare(file, rank));
            }
            file++;
        }
        fen++;
    }

    position->side = ((*fen == 'w') ? WHITE : BLACK);

    fen += 2;

    for (int i = 0; i < 4; i++)
    {
        if (*fen == ' ')
        {
            break;
        }
        switch (*fen)
        {
        case 'K': position->castlePermissions |= WHITE_KING_CASTLE; position->hasCastled[0] = 1; break;
        case 'Q': position->castlePermissions |= WHITE_QUEEN_CASTLE; position->hasCastled[0] = 1; break;
        case 'k': position->castlePermissions |= BLACK_KING_CASTLE; position->hasCastled[1] = 1; break;
        case 'q': position->castlePermissions |= BLACK_QUEEN_CASTLE; position->hasCastled[1] = 1; break;
        default: break;
        }
        fen++;
    }
    fen++;

    if (*fen != '-')
    {
        file = fen[0] - 'a';
        rank = fen[1] - '1';

        position->enPassantSquare = getSquare(file, rank);
    }

    position->pieceBB[WHITE] = 0ULL | position->pieceBB[WHITE_ROOK] | position->pieceBB[WHITE_KNIGHT] | position->pieceBB[WHITE_BISHOP] | position->pieceBB[WHITE_QUEEN] | position->pieceBB[WHITE_KING] | position->pieceBB[WHITE_PAWN];
    position->pieceBB[BLACK] = 0ULL | position->pieceBB[BLACK_ROOK] | position->pieceBB[BLACK_KNIGHT] | position->pieceBB[BLACK_BISHOP] | position->pieceBB[BLACK_QUEEN] | position->pieceBB[BLACK_KING] | position->pieceBB[BLACK_PAWN];
    position->occupiedBB = 0ULL | position->pieceBB[WHITE] | position->pieceBB[BLACK];
    position->emptyBB = ~position->occupiedBB;

    position->key = generateHashKey(position);

}

int checkHashKey(Board* position) {
    int valid;
    if (generateHashKey(position) == position->key)
    {
        valid = 1;
    }
    else
    {
        valid = 0;
    }
    if (!valid) {
        for (int i = 0; i < position->gamePly; i++) {
            debugMove(position->gameHistory[i].move);
        }
    }
    return valid;
}

//io

int parseMove(Board* position, char* ptrChar) {
    int from = getSquare(ptrChar[0] - 'a', ptrChar[1] - '1');
    int to = getSquare(ptrChar[2] - 'a', ptrChar[3] - '1');

    MoveList list[1];
    //generateMoves(position, list);
    GenAllMoves(position, list);

    for (int i = 0; i < list->count; ++i) {
        int moveTest = list->moves[i].move;
        if (from(moveTest) == from && to(moveTest) == to) {
            if (isPromote(moveTest)) {
                char promote = ptrChar[4];
                int promotedPiece = promoted(moveTest);
                switch (promote) {
                case 'b': if (promotedPiece == WHITE_BISHOP || promotedPiece == BLACK_BISHOP) return moveTest; break;
                case 'n': if (promotedPiece == WHITE_KNIGHT || promotedPiece == BLACK_KNIGHT) return moveTest; break;
                case 'r': if (promotedPiece == WHITE_ROOK || promotedPiece == BLACK_ROOK) return moveTest; break;
                case 'q': if (promotedPiece == WHITE_QUEEN || promotedPiece == BLACK_QUEEN) return moveTest; break;
                default: break;
                }
                continue;
            }
            return moveTest;
        }
    }
    return NOMOVE;
}

void printBoard(Board* position) {
    char ids[14] = { '!','!','P','N','B','R','Q','K','p','n','b','r','q','k' };

    int rank;

    for (int r = RANK_8; r >= RANK_1; r--) {
        printf("%d  ", r + 1);
        for (int f = FILE_A; f <= FILE_H; f++) {
            int square = getSquare(f, r);
            int hit = 0;
            for (int piece = 2; piece < 14; piece++) {
                if (position->pieceBB[piece] & setMask[square]) {
                    hit = 1;
                    printf(" %c ", ids[piece]);
                    break;
                }
            }
            if (!hit) printf(" . ");

        }
        printf("\n");
    }
    printf("    a  b  c  d  e  f  g  h ");
    printf("\nside: ");
    if (position->side == WHITE) printf("WHITE");
    else printf("BLACK");
    printf("\ncastle permissions: ");
    int castlePermissions = position->castlePermissions;
    if (castlePermissions & WHITE_KING_CASTLE) printf("K");
    if (castlePermissions & WHITE_QUEEN_CASTLE) printf("Q");
    if (castlePermissions & BLACK_KING_CASTLE) printf("k");
    if (castlePermissions & BLACK_QUEEN_CASTLE) printf("q");
    printf("\nen passant: %d", position->enPassantSquare);
    printf("\nPosKey:%llX\n", position->key);
}

//misc

int InputWaiting()
{
    static int init = 0, pipe;
    static HANDLE inh;
    DWORD dw;

    if (!init) {
        init = 1;
        inh = GetStdHandle(STD_INPUT_HANDLE);
        pipe = !GetConsoleMode(inh, &dw);
        if (!pipe) {
            SetConsoleMode(inh, dw & ~(ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT));
            FlushConsoleInputBuffer(inh);
        }
    }
    if (pipe) {
        if (!PeekNamedPipe(inh, NULL, 0, NULL, &dw, NULL)) return 1;
        return dw;
    }
    else {
        GetNumberOfConsoleInputEvents(inh, &dw);
        return dw <= 1 ? 0 : dw;
    }
}

void readInput(SearchInfo* info) {
    int             bytes;
    char            input[256] = "", * endc;

    if (InputWaiting()) {
        do {
            bytes = read(fileno(stdin), input, 256);
        } while (bytes < 0);
        endc = strchr(input, '\n');
        if (endc) *endc = 0;

        if (strlen(input) > 0) {
            if (!strncmp(input, "quit", 4)) {
                info->quit = 1;
            }
        }
        return;
    }
}

//perft

long leafNodes;

void perft(Board* position, int depth) {
    if (depth == 0) {
        leafNodes++;
        return;
    }

    MoveList list[1];
    GenAllMoves(position, list);
    for (int i = 0; i < list->count; i++) {
        int move = list->moves[i].move;
        if (!makeMove(position, move)) continue;
        perft(position, depth - 1);
        unmakeMove(position);
    }

    return;
}

bool MoveIsPseudoLegal(Board* position, int move);

void PerftTest(int depth, Board* position) {

    printBoard(position);
    printf("\nStarting Test To Depth:%d\n", depth);
    leafNodes = 0;
    int start = GetTickCount();
    MoveList list[1];
    GenAllMoves(position, list);

    int move;
    int MoveNum = 0;
    for (MoveNum = 0; MoveNum < list->count; ++MoveNum) {
        move = list->moves[MoveNum].move;
        if (!makeMove(position, move)) {
            continue;
        }
        long cumnodes = leafNodes;
        perft(position, depth - 1);
        unmakeMove(position);
        long oldnodes = leafNodes - cumnodes;
        printf("move %d : %s : %ld\n", MoveNum + 1, PrMove(move), oldnodes);
        if (MoveIsPseudoLegal(position, move))
        {
            printf("true\n");
        }
        else
        {
            printf("false\n");
        }
        
    }

    printf("\nTest Complete : %ld nodes visited in %dms\n", leafNodes, GetTickCount() - start);

    return;
}

//move.c

const int EMPTY = 0;

static const U64 bitB1C1D1 = (1ULL << B1) | (1ULL << C1) | (1ULL << D1);
static const U64 bitB8C8D8 = (1ULL << B8) | (1ULL << C8) | (1ULL << D8);
static const U64 bitF1G1 = (1ULL << F1) | (1ULL << G1);
static const U64 bitF8G8 = (1ULL << F8) | (1ULL << G8);

int colorOf(Board* position, int piece)
{
    if ((2 <= piece) && (piece <= 7))
    {
        return WHITE;
    }
    else if ((8 <= piece) && (piece <= 13))
    {
        return BLACK;
    }
    else
    {
        return 2;
    }

}

bool SqAttacked(int sq, int color, Board* position)
{
    if (color == WHITE)
    {
        return (attackedByWhite(position, sq) == 1);
    }
    else
    {
        return (attackedByBlack(position, sq) == 1);
    }
}

int PieceOnSquare(int sq, Board* position)
{
    int i = 2, piece = 0;
    while (i < 13)
    {
        if (((position->pieceBB[i]) & setMask[sq]) == 0ULL)
        {
            i++;
        }
        else
        {
            piece = i;
            break;
        }
    }
    return piece;
}

// Functions that add moves to the movelist for NextMove

// Checks whether a move is pseudo-legal (assuming it is pseudo-legal in some position)
bool MoveIsPseudoLegal(Board* position, int move) {

    int fr = from(move);
    int tosq = to(move);
    int piece = moving(move);
    int color;
    if ((position->pieceBB[WHITE]) & setMask[fr])
    {
        color = WHITE;
    }
    else
    {
        color = BLACK;
    }
    //printf("from %d to %d piece %d color %d\n", fr, tosq, piece, color);

    int capt1 = captured(move);
    int capt2 = PieceOnSquare(tosq, position);
    //const int capt2 = pos->board[to];

    // Easy sanity tests
    if (piece == EMPTY
        || color != position->side
        || capt1 != capt2
        || move == NOMOVE)
        return false;

    U64 occupied = position->occupiedBB;
    U64 toBB = 1ULL << tosq;

    /*if ((piece == WHITE_PAWN) && !isCapture(move) && !(move & FLAG_EP) && !(move & FLAG_PS))
    {
        printf("\n"); debugMove(move); printf("\n");
    }*/

    // Make sure the piece at 'from' can move to 'to' (ignoring pins/moving into check)
    switch (piece) {
    case WHITE_KNIGHT: return toBB & knightAttacks[fr];
    case BLACK_KNIGHT: return toBB & knightAttacks[fr];
    case WHITE_BISHOP: return toBB & bishopAttacks(position, fr);
    case BLACK_BISHOP: return toBB & bishopAttacks(position, fr);
    case WHITE_ROOK: return toBB & rookAttacks(position, fr);
    case BLACK_ROOK: return toBB & rookAttacks(position, fr);
    case WHITE_QUEEN: return toBB & (bishopAttacks(position, fr) | rookAttacks(position, fr));
    case BLACK_QUEEN: return toBB & (bishopAttacks(position, fr) | rookAttacks(position, fr));
    case WHITE_PAWN:
        if (move & FLAG_EP)
        {
            return (tosq == position->enPassantSquare);
        }
        else if (move & FLAG_PS)
        {
            return (((position->occupiedBB) & (1ULL << (tosq + 8 - 16 * color))) == 0ULL);
        }
        else if isCapture(move)
        {
            return toBB & pawn_attacks[color][fr];
        }
        else
        {
            return (((position->occupiedBB) & (1ULL << tosq)) == 0ULL);
        }
    case BLACK_PAWN:
        if (move & FLAG_EP)
        {
            return (tosq == position->enPassantSquare);
        }
        else if (move & FLAG_PS)
        {
            return (((position->occupiedBB) & (1ULL << (tosq + 8 - 16 * color))) == 0ULL);
        }
        else if isCapture(move)
        {
            return toBB & pawn_attacks[color][fr];
        }
        else
        { 
            return (((position->occupiedBB) & (1ULL << tosq)) == 0ULL);
        }
    case WHITE_KING:
        if (move & FLAG_CA)
        {
            switch (tosq) {
            case C1: return (position->castlePermissions & WHITE_QUEEN_CASTLE)
                && !(occupied & bitB1C1D1)
                && !SqAttacked(E1, BLACK, position)
                && !SqAttacked(D1, BLACK, position);
            case G1: return (position->castlePermissions & WHITE_KING_CASTLE)
                && !(occupied & bitF1G1)
                && !SqAttacked(E1, BLACK, position)
                && !SqAttacked(F1, BLACK, position);
            }
        }
        else
        {
            return toBB & kingAttacks[fr];
        }
    case BLACK_KING:
        if (move & FLAG_CA)
        {
            switch (tosq) {
            case C8: return (position->castlePermissions & BLACK_QUEEN_CASTLE)
                && !(occupied & bitB8C8D8)
                && !SqAttacked(E8, WHITE, position)
                && !SqAttacked(D8, WHITE, position);
            case G8: return (position->castlePermissions & BLACK_KING_CASTLE)
                && !(occupied & bitF8G8)
                && !SqAttacked(E8, WHITE, position)
                && !SqAttacked(F8, WHITE, position);
            }
        }
        else
        { 
            return toBB & kingAttacks[fr];
        }
    }
    return false;
}

void AddQuiet(Board* position, int from, int to, int movingPiece, int promo, int flag, MoveList* list) {

    int* moveScore = &list->moves[list->count].score;

    int move = generateMove(from, to, EMPTY, movingPiece, promo, flag);

    // Add scores to help move ordering based on search history heuristics
    if (killer1 == move)
        *moveScore = 900000;
    else if (killer2 == move)
        *moveScore = 800000;
    else
        *moveScore = position->history[movingPiece][to];

    list->moves[list->count++].move = move;
}
void AddCapture(Board* position, int from, int to, int capturedPiece, int movingPiece, int promo, MoveList* list) {

    int* moveScore = &list->moves[list->count].score;

    int move = generateMove(from, to, capturedPiece, movingPiece, promo, 0);

    *moveScore = MvvLvaScores[capturedPiece][movingPiece];

    list->moves[list->count++].move = move;
}
void AddEnPas(int move, MoveList* list) {
    list->moves[list->count].move = move;
    list->moves[list->count].score = 105 + 1000000;
    list->count++;
}
void AddWPromo(Board* position, int from, int to, MoveList* list) {

    AddQuiet(position, from, to, WHITE_PAWN, WHITE_QUEEN, 0, list);
    AddQuiet(position, from, to, WHITE_PAWN, WHITE_KNIGHT, 0, list);
    AddQuiet(position, from, to, WHITE_PAWN, WHITE_ROOK, 0, list);
    AddQuiet(position, from, to, WHITE_PAWN, WHITE_BISHOP, 0, list);
}
void AddWPromoCapture(Board* position, int from, int to, int capturedPiece, MoveList* list) {

    AddCapture(position, from, to, capturedPiece, WHITE_PAWN, WHITE_QUEEN, list);
    AddCapture(position, from, to, capturedPiece, WHITE_PAWN, WHITE_KNIGHT, list);
    AddCapture(position, from, to, capturedPiece, WHITE_PAWN, WHITE_ROOK, list);
    AddCapture(position, from, to, capturedPiece, WHITE_PAWN, WHITE_BISHOP, list);
}
void AddBPromo(Board* position, int from, int to, MoveList* list) {

    AddQuiet(position, from, to, BLACK_PAWN, BLACK_QUEEN, 0, list);
    AddQuiet(position, from, to, BLACK_PAWN, BLACK_KNIGHT, 0, list);
    AddQuiet(position, from, to, BLACK_PAWN, BLACK_ROOK, 0, list);
    AddQuiet(position, from, to, BLACK_PAWN, BLACK_BISHOP, 0, list);
}
void AddBPromoCapture(Board* position, int from, int to, int capturedPiece, MoveList* list) {

    AddCapture(position, from, to, capturedPiece, BLACK_PAWN, BLACK_QUEEN, list);
    AddCapture(position, from, to, capturedPiece, BLACK_PAWN, BLACK_KNIGHT, list);
    AddCapture(position, from, to, capturedPiece, BLACK_PAWN, BLACK_ROOK, list);
    AddCapture(position, from, to, capturedPiece, BLACK_PAWN, BLACK_BISHOP, list);
}

/* Generators for specific color/piece combinations - called by generic generators*/

// King
void GenWCastling(Board* position, MoveList* list, U64 occupied) {

    // King side castle
    if (position->castlePermissions & WHITE_KING_CASTLE)
        if (!(occupied & bitF1G1))
            if (!SqAttacked(E1, BLACK, position) && !SqAttacked(F1, BLACK, position))
            {
                AddQuiet(position, E1, G1, WHITE_KING, EMPTY, FLAG_CA, list);
                position->hasCastled[0] = 0;
            }
                

    // Queen side castle
    if (position->castlePermissions & WHITE_QUEEN_CASTLE)
        if (!(occupied & bitB1C1D1))
            if (!SqAttacked(E1, BLACK, position) && !SqAttacked(D1, BLACK, position))
            {
                AddQuiet(position, E1, C1, WHITE_KING, EMPTY, FLAG_CA, list);
                position->hasCastled[0] = 0;
            }
}
void GenBCastling(Board* position, MoveList* list, U64 occupied) {

    // King side castle
    if (position->castlePermissions & BLACK_KING_CASTLE)
        if (!(occupied & bitF8G8))
            if (!SqAttacked(E8, WHITE, position) && !SqAttacked(F8, WHITE, position))
            {
                AddQuiet(position, E8, G8, BLACK_KING, EMPTY, FLAG_CA, list);
                position->hasCastled[1] = 0;
            }

    // Queen side castle
    if (position->castlePermissions & BLACK_QUEEN_CASTLE)
        if (!(occupied & bitB8C8D8))
            if (!SqAttacked(E8, WHITE, position) && !SqAttacked(D8, WHITE, position))
            {
                AddQuiet(position, E8, C8, BLACK_KING, EMPTY, FLAG_CA, list);
                position->hasCastled[1] = 0;
            }
}
void GenKingQuiet(Board* position, MoveList* list, int sq, U64 empty) {

    U64 moves = kingAttacks[sq] & empty;
    int piece;

    if((position->pieceBB[WHITE]) & (1ULL << sq))
    { 
        piece = WHITE_KING;
    }
    else
    {
        piece = BLACK_KING;
    }
    while (moves) {
        AddQuiet(position, sq, PopBit(&moves), piece, EMPTY, 0, list);
    }
}
void GenKingNoisy(Board* position, MoveList* list, int sq, U64 enemies) {

    U64 attacks = kingAttacks[sq] & enemies;
    int piece, toSq, capturedPiece;
    if ((position->pieceBB[WHITE]) & setMask[sq])
    {
        piece = WHITE_KING;
    }
    else
    {
        piece = BLACK_KING;
    }
    while (attacks)
    {
        toSq = PopBit(&attacks);
        capturedPiece = PieceOnSquare(toSq, position);
        AddCapture(position, sq, toSq, capturedPiece, piece, 0, list);
    }
}

const U64 rank1BB = 0xFF;
const U64 rank2BB = 0xFF00;
const U64 rank3BB = 0xFF0000;
const U64 rank4BB = 0xFF000000;
const U64 rank5BB = 0xFF00000000;
const U64 rank6BB = 0xFF0000000000;
const U64 rank7BB = 0xFF000000000000;
const U64 rank8BB = 0xFF00000000000000;

// White pawn
void GenWPawnNoisy(Board* position, MoveList* list, U64 enemies, U64 empty) {

    int sq, capturedPiece, piece;
    U64 enPassers;

    U64 pawns = position->pieceBB[WHITE_PAWN];
    U64 lAttacks = ((pawns & ~FILEA) << 7) & enemies;
    U64 rAttacks = ((pawns & ~FILEH) << 9) & enemies;
    U64 lNormalCap = lAttacks & ~rank8BB;
    U64 lPromoCap = lAttacks & rank8BB;
    U64 rNormalCap = rAttacks & ~rank8BB;
    U64 rPromoCap = rAttacks & rank8BB;
    U64 promotions = empty & (pawns & rank7BB) << 8;

    // Promoting captures
    while (lPromoCap) {
        sq = PopBit(&lPromoCap);
        capturedPiece = PieceOnSquare(sq, position);
        AddWPromoCapture(position, sq - 7, sq, capturedPiece, list);
    }
    while (rPromoCap) {
        sq = PopBit(&rPromoCap);
        capturedPiece = PieceOnSquare(sq, position);
        AddWPromoCapture(position, sq - 9, sq, capturedPiece, list);
    }
    // Promotions
    while (promotions) {
        sq = PopBit(&promotions);
        AddWPromo(position, (sq - 8), sq, list);
    }
    // Captures
    while (lNormalCap) {
        sq = PopBit(&lNormalCap);
        capturedPiece = PieceOnSquare(sq, position);
        AddCapture(position, sq - 7, sq, capturedPiece, WHITE_PAWN, EMPTY, list);
    }
    while (rNormalCap) {
        sq = PopBit(&rNormalCap);
        capturedPiece = PieceOnSquare(sq, position);
        AddCapture(position, sq - 9, sq, capturedPiece, WHITE_PAWN, EMPTY, list);
    }
    // En passant
    if (position->enPassantSquare < 64) {
        enPassers = pawns & pawn_attacks[BLACK][position->enPassantSquare];
        while (enPassers)
            AddEnPas(generateMove(PopBit(&enPassers), position->enPassantSquare, BLACK_PAWN, WHITE_PAWN, EMPTY, FLAG_EP), list);
    }
}
void GenWPawnQuiet(Board* position, MoveList* list, U64 empty) {

    int sq;
    U64 pawnMoves, pawnStarts, pawnsNot7th;

    pawnsNot7th = position->pieceBB[WHITE_PAWN] & ~rank7BB;

    pawnMoves = empty & pawnsNot7th << 8;
    pawnStarts = empty & (pawnMoves & rank3BB) << 8;

    // Normal pawn moves
    while (pawnMoves) {
        sq = PopBit(&pawnMoves);
        AddQuiet(position, (sq - 8), sq, WHITE_PAWN, EMPTY, 0, list);
    }
    // Pawn starts
    while (pawnStarts) {
        sq = PopBit(&pawnStarts);
        AddQuiet(position, (sq - 16), sq, WHITE_PAWN, EMPTY, FLAG_PS, list);
    }
}
// Black pawn
void GenBPawnNoisy(Board* position, MoveList* list, U64 enemies, U64 empty) {

    int sq, capturedPiece;
    U64 enPassers;

    U64 pawns = position->pieceBB[BLACK_PAWN];
    U64 lAttacks = ((pawns & ~FILEH) >> 7)& enemies;
    U64 rAttacks = ((pawns & ~FILEA) >> 9)& enemies;
    U64 lNormalCap = lAttacks & ~rank1BB;
    U64 lPromoCap = lAttacks & rank1BB;
    U64 rNormalCap = rAttacks & ~rank1BB;
    U64 rPromoCap = rAttacks & rank1BB;
    U64 promotions = empty & (pawns & rank2BB) >> 8;

    // Promoting captures
    while (lPromoCap) {
        sq = PopBit(&lPromoCap);
        capturedPiece = PieceOnSquare(sq, position);
        AddBPromoCapture(position, sq + 7, sq, capturedPiece, list);
    }
    while (rPromoCap) {
        sq = PopBit(&rPromoCap);
        capturedPiece = PieceOnSquare(sq, position);
        AddBPromoCapture(position, sq + 9, sq, capturedPiece, list);
    }
    // Promotions
    while (promotions) {
        sq = PopBit(&promotions);
        AddBPromo(position, (sq + 8), sq, list);
    }
    // Captures
    while (lNormalCap) {
        sq = PopBit(&lNormalCap);
        capturedPiece = PieceOnSquare(sq, position);
        AddCapture(position, sq + 7, sq, capturedPiece, BLACK_PAWN, EMPTY, list);
    }
    while (rNormalCap) {
        sq = PopBit(&rNormalCap);
        capturedPiece = PieceOnSquare(sq, position);
        AddCapture(position, sq + 9, sq, capturedPiece, BLACK_PAWN, EMPTY, list);
    }
    // En passant
    if (position->enPassantSquare < 64) {
        enPassers = pawns & pawn_attacks[WHITE][position->enPassantSquare];
        while (enPassers)
            AddEnPas(generateMove(PopBit(&enPassers), position->enPassantSquare, WHITE_PAWN, BLACK_PAWN, EMPTY, FLAG_EP), list);
    }
}
void GenBPawnQuiet(Board* position, MoveList* list, U64 empty) {

    int sq;
    U64 pawnMoves, pawnStarts, pawnsNot7th;

    pawnsNot7th = position->pieceBB[BLACK_PAWN] & ~rank2BB;

    pawnMoves = empty & pawnsNot7th >> 8;
    pawnStarts = empty & (pawnMoves & rank6BB) >> 8;

    // Normal pawn moves
    while (pawnMoves) {
        sq = PopBit(&pawnMoves);
        AddQuiet(position, (sq + 8), sq, BLACK_PAWN, EMPTY, 0, list);
    }
    // Pawn starts
    while (pawnStarts) {
        sq = PopBit(&pawnStarts);
        AddQuiet(position, (sq + 16), sq, BLACK_PAWN, EMPTY, FLAG_PS, list);
    }
}

// White knight
void GenWKnightQuiet(Board* position, MoveList* list, U64 empty) {

    int sq;
    U64 moves;

    U64 knights = position->pieceBB[WHITE_KNIGHT];

    while (knights) {

        sq = PopBit(&knights);

        moves = knightAttacks[sq] & empty;
        while (moves)
            AddQuiet(position, sq, PopBit(&moves), WHITE_KNIGHT, EMPTY, 0, list);
    }
}
void GenWKnightNoisy(Board* position, MoveList* list, U64 enemies) {

    int sq, tosq, capturedPiece;
    U64 attacks;

    U64 knights = position->pieceBB[WHITE_KNIGHT];

    while (knights) {

        sq = PopBit(&knights);

        attacks = knightAttacks[sq] & enemies;
        while (attacks) {
            tosq = PopBit(&attacks);
            capturedPiece = PieceOnSquare(tosq, position);
            AddCapture(position, sq, tosq, capturedPiece, WHITE_KNIGHT, EMPTY, list);
        }
    }
}
// Black knight
void GenBKnightQuiet(Board* position, MoveList* list, U64 empty) {

    int sq;
    U64 moves;

    U64 knights = position->pieceBB[BLACK_KNIGHT];

    while (knights) {

        sq = PopBit(&knights);

        moves = knightAttacks[sq] & empty;
        while (moves)
            AddQuiet(position, sq, PopBit(&moves), BLACK_KNIGHT, EMPTY, 0, list);
    }
}
void GenBKnightNoisy(Board* position, MoveList* list, U64 enemies) {

    int sq, tosq, capturedPiece;
    U64 attacks;

    U64 knights = position->pieceBB[BLACK_KNIGHT];

    while (knights) {

        sq = PopBit(&knights);

        attacks = knightAttacks[sq] & enemies;
        while (attacks) {
            tosq = PopBit(&attacks);
            capturedPiece = PieceOnSquare(tosq, position);
            AddCapture(position, sq, tosq, capturedPiece, BLACK_KNIGHT, 0, list);
        }
    }
}

// White bishop
void GenWBishopQuiet(Board* position, MoveList* list, U64 empty, U64 occupied) {

    int sq;
    U64 moves;

    U64 bishops = position->pieceBB[WHITE_BISHOP];

    while (bishops) {

        sq = PopBit(&bishops);

        moves = bishopAttacks(position, sq) & empty;
        while (moves)
            AddQuiet(position, sq, PopBit(&moves), WHITE_BISHOP, EMPTY, 0, list);
    }
}
void GenWBishopNoisy(Board* position, MoveList* list, U64 enemies, U64 occupied) {

    int sq, tosq, capturedPiece;
    U64 attacks;

    U64 bishops = position->pieceBB[WHITE_BISHOP];

    while (bishops) {

        sq = PopBit(&bishops);

        attacks = bishopAttacks(position, sq) & enemies;
        while (attacks) {
            tosq = PopBit(&attacks);
            capturedPiece = PieceOnSquare(tosq, position);
            AddCapture(position, sq, tosq, capturedPiece, WHITE_BISHOP, 0, list);
        }
    }
}
// Black bishop
void GenBBishopQuiet(Board* position, MoveList* list, U64 empty, U64 occupied) {

    int sq;
    U64 moves;

    U64 bishops = position->pieceBB[BLACK_BISHOP];

    while (bishops) {

        sq = PopBit(&bishops);

        moves = bishopAttacks(position, sq) & empty;
        while (moves)
            AddQuiet(position, sq, PopBit(&moves), BLACK_BISHOP, EMPTY, 0, list);
    }
}
void GenBBishopNoisy(Board* position, MoveList* list, U64 enemies, U64 occupied) {

    int sq, tosq, capturedPiece;
    U64 attacks;

    U64 bishops = position->pieceBB[BLACK_BISHOP];

    while (bishops) {

        sq = PopBit(&bishops);

        attacks = bishopAttacks(position, sq) & enemies;
        while (attacks) {
            tosq = PopBit(&attacks);
            capturedPiece = PieceOnSquare(tosq, position);
            AddCapture(position, sq, tosq, capturedPiece, BLACK_BISHOP, 0, list);
        }
    }
}

// White rook
void GenWRookQuiet(Board* position, MoveList* list, U64 empty, U64 occupied) {

    int sq;
    U64 moves;

    U64 rooks = position->pieceBB[WHITE_ROOK];

    while (rooks) {

        sq = PopBit(&rooks);

        moves = rookAttacks(position, sq) & empty;
        while (moves)
            AddQuiet(position, sq, PopBit(&moves), WHITE_ROOK, EMPTY, 0, list);
    }
}
void GenWRookNoisy(Board* position, MoveList* list, U64 enemies, U64 occupied) {

    int sq, tosq, capturedPiece;
    U64 attacks;

    U64 rooks = position->pieceBB[WHITE_ROOK];

    while (rooks) {

        sq = PopBit(&rooks);

        attacks = rookAttacks(position, sq) & enemies;
        while (attacks) {
            tosq = PopBit(&attacks);
            capturedPiece = PieceOnSquare(tosq, position);
            AddCapture(position, sq, tosq, capturedPiece, WHITE_ROOK, 0, list);
        }
    }
}
// Black rook
void GenBRookQuiet(Board* position, MoveList* list, U64 empty, U64 occupied) {

    int sq;
    U64 moves;

    U64 rooks = position->pieceBB[BLACK_ROOK];

    while (rooks) {

        sq = PopBit(&rooks);

        moves = rookAttacks(position, sq) & empty;
        while (moves)
            AddQuiet(position, sq, PopBit(&moves), BLACK_ROOK, EMPTY, 0, list);
    }
}
void GenBRookNoisy(Board* position, MoveList* list, U64 enemies, U64 occupied) {

    int sq, tosq, capturedPiece;
    U64 attacks;

    U64 rooks = position->pieceBB[BLACK_ROOK];

    while (rooks) {

        sq = PopBit(&rooks);

        attacks = rookAttacks(position, sq) & enemies;
        while (attacks) {
            tosq = PopBit(&attacks);
            capturedPiece = PieceOnSquare(tosq, position);
            AddCapture(position, sq, tosq, capturedPiece, BLACK_ROOK, 0, list);
        }
    }
}

// White queen
void GenWQueenQuiet(Board* position, MoveList* list, U64 empty, U64 occupied) {

    int sq;
    U64 moves;

    U64 queens = position->pieceBB[WHITE_QUEEN];

    while (queens) {

        sq = PopBit(&queens);

        moves = (bishopAttacks(position, sq) | rookAttacks(position, sq)) & empty;
        while (moves)
            AddQuiet(position, sq, PopBit(&moves), WHITE_QUEEN, EMPTY, 0, list);
    }
}
void GenWQueenNoisy(Board* position, MoveList* list, U64 enemies, U64 occupied) {

    int sq, tosq, capturedPiece;
    U64 attacks;

    U64 queens = position->pieceBB[WHITE_QUEEN];

    while (queens) {

        sq = PopBit(&queens);

        attacks = (bishopAttacks(position, sq) | rookAttacks(position, sq)) & enemies;
        while (attacks) {
            tosq = PopBit(&attacks);
            capturedPiece = PieceOnSquare(tosq, position);
            AddCapture(position, sq, tosq, capturedPiece, WHITE_QUEEN, 0, list);
        }
    }
}
// Black queen
void GenBQueenQuiet(Board* position, MoveList* list, U64 empty, U64 occupied) {

    int sq;
    U64 moves;

    U64 queens = position->pieceBB[BLACK_QUEEN];

    while (queens) {

        sq = PopBit(&queens);

        moves = (bishopAttacks(position, sq) | rookAttacks(position, sq)) & empty;
        while (moves)
            AddQuiet(position, sq, PopBit(&moves), BLACK_QUEEN, EMPTY, 0, list);
    }
}
void GenBQueenNoisy(Board* position, MoveList* list, U64 enemies, U64 occupied) {

    int sq, tosq, capturedPiece;
    U64 attacks;

    U64 queens = position->pieceBB[BLACK_QUEEN];

    while (queens) {

        sq = PopBit(&queens);

        attacks = (bishopAttacks(position, sq) | rookAttacks(position, sq)) & enemies;
        while (attacks) {
            tosq = PopBit(&attacks);
            capturedPiece = PieceOnSquare(tosq, position);
            AddCapture(position, sq, tosq, capturedPiece, BLACK_QUEEN, 0, list);
        }
    }
}

/* Generic generators */

// Generate all quiet moves
void GenQuietMoves(Board* position, MoveList* list) {

    int side = position->side;

    U64 occupied = position->occupiedBB;
    U64 empty = position->emptyBB;

    if (side == WHITE) {
        GenWCastling(position, list, occupied);
        GenWPawnQuiet(position, list, empty);
        GenWKnightQuiet(position, list, empty);
        GenWRookQuiet(position, list, empty, occupied);
        GenWBishopQuiet(position, list, empty, occupied);
        GenWQueenQuiet(position, list, empty, occupied);
    }
    else {
        GenBCastling(position, list, occupied);
        GenBPawnQuiet(position, list, empty);
        GenBKnightQuiet(position, list, empty);
        GenBRookQuiet(position, list, empty, occupied);
        GenBBishopQuiet(position, list, empty, occupied);
        GenBQueenQuiet(position, list, empty, occupied);
    }

    if (side == WHITE) {

        U64 whiteKingBitboard = (position->pieceBB[WHITE_KING]);
        int whiteKingSquare = PopBit(&whiteKingBitboard);
        GenKingQuiet(position, list, whiteKingSquare, empty);
    }
    else
    {
        U64 blackKingBitboard = (position->pieceBB[BLACK_KING]);
        int blackKingSquare = PopBit(&blackKingBitboard);
        GenKingQuiet(position, list, blackKingSquare, empty);
    }
    
}

// Generate all noisy moves
void GenNoisyMoves(Board* position, MoveList* list) {

    int side = position->side;

    U64 occupied = position->occupiedBB;
    U64 enemies;
    if (side == WHITE)
    {
        enemies = position->pieceBB[BLACK];
    }
    else
    {
        enemies = position->pieceBB[WHITE];
    }
    U64 empty = position->emptyBB;

    // Pawns
    if (side == WHITE) {
        GenWPawnNoisy(position, list, enemies, empty);
        GenWKnightNoisy(position, list, enemies);
        GenWRookNoisy(position, list, enemies, occupied);
        GenWBishopNoisy(position, list, enemies, occupied);
        GenWQueenNoisy(position, list, enemies, occupied);

    }
    else {
        GenBPawnNoisy(position, list, enemies, empty);
        GenBKnightNoisy(position, list, enemies);
        GenBRookNoisy(position, list, enemies, occupied);
        GenBBishopNoisy(position, list, enemies, occupied);
        GenBQueenNoisy(position, list, enemies, occupied);
    }

    if (side == WHITE) {

        U64 whiteKingBitboard = (position->pieceBB[WHITE_KING]);
        //int whiteKingSquare = PopBit(&(position->pieceBB[WHITE_KING]));
        int whiteKingSquare = PopBit(&whiteKingBitboard);
        GenKingNoisy(position, list, whiteKingSquare, enemies);
    }
    else
    {
        U64 blackKingBitboard = (position->pieceBB[BLACK_KING]);
        //int blackKingSquare = PopBit(&(position->pieceBB[BLACK_KING]));
        int blackKingSquare = PopBit(&blackKingBitboard);
        GenKingNoisy(position, list, blackKingSquare, enemies);
    }
}

// Generate all pseudo legal moves
void GenAllMoves(Board* position, MoveList* list) {

    list->count = list->next = 0;

    GenNoisyMoves(position, list);
    GenQuietMoves(position, list);
}

//search

void clearForSearch(Board* position, SearchInfo* info) {
    
    memset(position->history, 0, sizeof(position->history));
    memset(position->killers, 0, sizeof(position->killers));

    position->ply = 0;
    info->nodes = 0;
    info->seldepth = 0;

    // Mark TT as used
    TT.dirty = true;
}

// Check time situation
static bool OutOfTime(SearchInfo* info) {

    if ((info->nodes & 8192) == 0
        && info->timeSet
        && GetTickCount() >= info->endTime)

        return true;

    return false;
}

int isRepetition(Board* position) {
    for (int i = 2; i <= position->fiftyMoveCounter; i += 2) {
        if (position->key == history(-i).posKey) {
            return 1;
        }
    }
    return 0;
}

enum PieceValue {
    P_MG = 100, P_EG = 121,
    N_MG = 459, N_EG = 390,
    B_MG = 465, B_EG = 412,
    R_MG = 630, R_EG = 711,
    Q_MG = 1272, Q_EG = 1317
};

int EGPieceValue(int piece) {
    if ((piece == WHITE_PAWN) || (piece == BLACK_PAWN))
    {
        return 121;
    }
    else if ((piece == WHITE_KNIGHT) || (piece == BLACK_KNIGHT))
    {
        return 390;
    }
    else if ((piece == WHITE_BISHOP) || (piece == BLACK_BISHOP))
    {
        return 412;
    }
    else if ((piece == WHITE_ROOK) || (piece == BLACK_ROOK))
    {
        return 711;
    }
    else
    {
        return 1317;
    }
}

// Print thinking
static void PrintThinking(const SearchInfo* info, Board* position) {

    int score = info->score;
    
    // Determine whether we have a centipawn or mate score
    char* type = abs(score) > ISMATE ? "mate" : "cp";

    // Convert score to mate score when applicable
    score = score > ISMATE ? ((infinite - score) / 2) + 1
        : score < -ISMATE ? -((infinite + score) / 2)
        : score * 100 / P_MG;

    int depth = info->IDDepth;
    int seldepth = info->seldepth;
    int elapsed = GetTickCount() - info->startTime;
    int hashFull = HashFull(position);
    int nps = (int)(1000 * (info->nodes / (elapsed + 1)));
    uint64_t nodes = info->nodes;

    // Basic info
    printf("info depth %d seldepth %d score %s %d time %d nodes %" PRId64 " nps %d ",
        depth, seldepth, type, score, elapsed, nodes, nps);

    // Principal variation
    printf("pv");
    for (int i = 0; i < info -> pv.length; i++)
        printf(" %s", PrMove(info -> pv.input[i]));

    printf("\n");
    fflush(stdout);
}

// Print conclusion of search - best move and ponder move
static void PrintConclusion(const SearchInfo* info) {

    printf("bestmove %s", PrMove(info->bestMove));
    printf("\n\n");
    fflush(stdout);
}

// Return the next best move
int PickNextMove(MoveList* list, int ttMove) {

    if (list->next == list->count)
        return NOMOVE;

    int bestMove;
    int bestScore = 0;
    unsigned int moveNum = list->next++;
    unsigned int bestNum = moveNum;

    for (unsigned int i = moveNum; i < list->count; ++i)
        if (list->moves[i].score > bestScore) {
            bestScore = list->moves[i].score;
            bestNum = i;
        }

    bestMove = list->moves[bestNum].move;
    list->moves[bestNum] = list->moves[moveNum];

    // Avoid returning the ttMove again
    if (bestMove == ttMove)
        return PickNextMove(list, ttMove);

    return bestMove;
}

// Returns the next move to try in a position
int NextMove(MovePicker* mp) {

    int move;

    // Switch on stage, falls through to the next stage
    // if a move isn't returned in the current stage.
    switch (mp->stage) {

    case TTMOVE:
        mp->stage++;
        if (MoveIsPseudoLegal(mp->position, mp->ttMove))
            return mp->ttMove;

    case GEN_NOISY:
        GenNoisyMoves(mp->position, mp->list);
        mp->stage++;

    case NOISY:
        if (mp->list->next < mp->list->count)
            if ((move = PickNextMove(mp->list, mp->ttMove)))
                return move;

        mp->stage++;

    case GEN_QUIET:
        if (mp->onlyNoisy)
            return NOMOVE;

        GenQuietMoves(mp->position, mp->list);
        mp->stage++;

    case QUIET:
        if (mp->list->next < mp->list->count)
            if ((move = PickNextMove(mp->list, mp->ttMove)))
                return move;

        return NOMOVE;

    default:
        return NOMOVE;
    }
}

// Init normal movepicker
void InitNormalMP(MovePicker* mp, MoveList* list, Board* position, int ttMove) {
    list->count = list->next = 0;
    mp->list = list;
    mp->onlyNoisy = false;
    mp->position = position;
    mp->stage = TTMOVE;
    mp->ttMove = ttMove;
}

// Init noisy movepicker
void InitNoisyMP(MovePicker* mp, MoveList* list, Board* position) {
    list->count = list->next = 0;
    mp->list = list;
    mp->onlyNoisy = true;
    mp->position = position;
    mp->stage = GEN_NOISY;
    mp->ttMove = NOMOVE;
}

int MAX(int num1, int num2)
{
    if (num1 > num2)
    {
        return num1;
    }
    else
    {
        return num2;
    }
}

int MIN(int num1, int num2)
{
    if (num1 > num2)
    {
        return num2;
    }
    else
    {
        return num1;
    }
}

int RankOf(int square) {
    return square >> 3;
}

// Converts a rank into the rank relative to the given color
int RelativeRank(int color, int rank) {
    return color == 0 ? rank : 7 - rank;
}

static int Quiescence(int alpha, int beta, Board* position, SearchInfo* info) {

    MovePicker mp;
    MoveList list;
    
    // Check time situation
    if (OutOfTime(info))
        longjmp(info->jumpBuffer, true);

    info->nodes++;

    // Update selective depth
    if (position->ply > info->seldepth)
        info->seldepth = position->ply;

    if ((isRepetition(position) == 1) || position->fiftyMoveCounter >= 100) {
        return 0;
    }
    if (position->ply >= MAX_DEPTH) {
        return evaluatePosition(position);
    }

    int score = evaluatePosition(position);
    if (score >= beta)
        return score;
    if ((score + Q_MG * 2) < alpha) // Very pessimistic (forced by poor eval) delta pruning
        return alpha;
    if (score > alpha)
        alpha = score;

    int futility = score + 60;

    int InCheck = underCheck(position, position->side);

    InitNoisyMP(&mp, &list, position);

    int bestScore = score;

    // Move loop
    int move;
    while ((move = NextMove(&mp))) {

        if ((InCheck == 0)
            && futility + EGPieceValue(captured(move)) <= alpha
            && !(((moving(move) == WHITE_PAWN) || (moving(move) == BLACK_PAWN))
                && RelativeRank(position->side, RankOf(to(move))) > 5))
            continue;

        // Recursively search the positions after making the moves, skipping illegal ones
        if (!makeMove(position, move)) continue;
        score = -Quiescence(-beta, -alpha, position, info);
        unmakeMove(position);

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

int absoluteValue(int i)
{
    if (i > 0)
    {
        return i;
    }
    else
    {
        return -i;
    }
}

int BigPiecesExist(Board* position, int side)
{
    if (side == 0)
    {
        if (((position->pieceBB[WHITE_KNIGHT]) | (position->pieceBB[WHITE_BISHOP]) | (position->pieceBB[WHITE_ROOK]) | (position->pieceBB[WHITE_QUEEN])) == 0ULL)
        {
            return 0;
        }
        else
        {
            return 1;
        }
    }
    else
    {
        if (((position->pieceBB[BLACK_KNIGHT]) | (position->pieceBB[BLACK_BISHOP]) | (position->pieceBB[BLACK_ROOK]) | (position->pieceBB[BLACK_QUEEN])) == 0ULL)
        {
            return 0;
        }
        else
        {
            return 1;
        }
    }
}

// Move either has enpas flag or a captured piece
#define MOVE_IS_CAPTURE 0x10F000
#define MOVE_IS_NOISY   0x1FF000

//noisy move en passant or capture

bool MoveIsNoisy(int move) {
    if ((isEnPassant(move) != 0) || (isCapture(move) != 0) || (isPromote(move) != 0))
    {
        return true;
    }
    else
    {
        return false;
    }
}

bool MoveIsQuiet(int move) {
    if ((isEnPassant(move) == 0) && (isCapture(move) == 0) && (isPromote(move) == 0))
    {
        return true;
    }
    else
    {
        return false;
    }
}

int tempo = 15;

static int AlphaBeta(int alpha, int beta, Depth depth, Board* position, SearchInfo* info, PV* pv) {
    
    bool pvNode = alpha != beta - 1;
    const bool root = position->ply == 0;

    PV pv_from_here;
    pv->length = 0;

    MovePicker mp;
    MoveList list;

    int R;

    // pre-search checks

    int InCheck = underCheck(position, position->side);
    bool inCheck;
    if (InCheck == 0)
    {
        inCheck = false;
    }
    else
    {
        inCheck = true;
    }

    // Check Extension (Extend all checks before dropping into Quiescence)
    if ((InCheck == 1) && (depth + 1 < MAX_DEPTH)) {
        depth++;
    }

    if (depth <= 0) {
        return Quiescence(alpha, beta, position, info);
    }

    // Check time situation
    if (OutOfTime(info))
        longjmp(info->jumpBuffer, true);

    info->nodes++;

    // Update selective depth
    if (position->ply > info->seldepth)
        info->seldepth = position->ply;

    // Early exits (not in root node)
    if (!root) {

        if ((isRepetition(position) == 1) || position->fiftyMoveCounter >= 100) {
            return 0;
        }
        if (position->ply >= MAX_DEPTH) {
            return evaluatePosition(position);
        }

        // Mate Distance Pruning (finds shorter mates)
        alpha = MAX(alpha, -infinite + position->ply);
        beta = MIN(beta, infinite - position->ply-1);
        if (alpha >= beta) {
            return alpha;
        }

    }

    // Probe transposition table
    bool ttHit;
    Key posKey = position->key;
    TTEntry* tte = ProbeTT(posKey, &ttHit);

    int ttMove = ttHit ? tte->move : NOMOVE;
    int ttScore = ttHit ? ScoreFromTT(tte->score, position->ply) : 32501;// NOSCORE;

    // Trust the ttScore in non-pvNodes as long as the entry depth is equal or higher
    if (!pvNode && ttHit && tte->depth >= depth) {

        // Check if ttScore causes a cutoff
        if (ttScore >= beta ? tte->bound & BOUND_LOWER
                            : tte->bound & BOUND_UPPER)

            return ttScore;
    }

    int bestScore = -infinite;
    int score;

    // Do a static evaluation for pruning considerations
    int eval = history(0).eval = inCheck          ? 32501
                               : lastMoveNullMove ? -history(-1).eval + 2 * tempo
                                                  : evaluatePosition(position);

    // Use ttScore as eval if useful
    if (ttScore != 32501
        && (tte->bound & (ttScore > eval ? BOUND_LOWER : BOUND_UPPER)))
        eval = ttScore;
    
    // Improving if not in check, and current eval is higher than 2 plies ago
    bool improving = !inCheck && position->ply >= 2 && eval > history(-2).eval;
    
    // Skip pruning while in check and at the root
    if (inCheck || root)
        goto move_loop;

     // Razoring
     if (!pvNode && depth < 2 && eval + 640 < alpha)
     return Quiescence(alpha, beta, position, info);

     // Reverse Futility Pruning
     if (!pvNode && depth < 7 && eval - 225 * depth + 100 * improving >= beta)
         return eval;

     // Null Move Pruning
     if (!pvNode && history(-1).move != NOMOVE && eval >= beta && history(0).eval >= beta && BigPiecesExist(position, position->side) && depth >= 3 && (!ttHit || !(tte->bound & BOUND_UPPER) || ttScore >= beta)) {

          int R = 3 + depth / 5 + MIN(3, (eval - beta) / 256);
            
          MakeNullMove(position);
          score = -AlphaBeta(-beta, -beta + 1, depth - R, position, info, &pv_from_here);
          TakeNullMove(position);

          // Cutoff
          if (score >= beta) {
              // Don't return unproven mate scores
              if (score >= ISMATE)
                  score = beta;
              return score;
          }
     }

     // Internal iterative deepening
     if (depth >= 4 && !ttMove) {

        AlphaBeta(alpha, beta, MAX(1, MIN(depth / 2, depth - 4)), position, info, &pv_from_here);

        tte = ProbeTT(posKey, &ttHit);

        ttMove = ttHit ? tte->move : NOMOVE;
     }

     // Internal iterative reduction based on Rebel's idea
     if (depth >= 4 && !ttMove)
         depth--;

move_loop:

    InitNormalMP(&mp, &list, position, ttMove);

    int movesTried = 0;
    int quietCount = 0;
    int oldAlpha = alpha;
    Move bestMove = NOMOVE;
    score = -infinite;

    // Move loop
    Move move;
    while ((move = NextMove(&mp))) {

        //bool quiet = !MoveIsNoisy(move);
        bool quiet = MoveIsQuiet(move);

        // Late move pruning
        if (!pvNode && (InCheck == 0) && quietCount > (3 + 2 * depth * depth) / (2 - improving))
            break;

        GetEntry(KeyAfter(position, move));

        // Make the next predicted best move, skipping illegal ones
        if (!makeMove(position, move)) continue;

        movesTried++;
        if (quiet)
            quietCount++;

        const Depth newDepth = depth - 1;
        
        bool doLMR = depth > 2 && movesTried > (2 + pvNode);

        // Reduced depth zero-window search (-1 depth)
        if (doLMR) {
            // Base reduction
            R = Reductions[MIN(31, depth)][MIN(31, movesTried)];
            // Reduce more in non-pv nodes
            R -= pvNode;
            // Reduce less when improving
            R -= improving;
            // Reduce more for quiets
            R += quiet;

            // Depth after reductions, avoiding going straight to quiescence
            Depth RDepth = MAX(1, newDepth - MAX(R, 1));

            score = -AlphaBeta(-alpha - 1, -alpha, RDepth, position, info, &pv_from_here);
        }
        // Full depth zero-window search
        if (doLMR ? score > alpha : !pvNode || movesTried > 1)
            score = -AlphaBeta(-alpha - 1, -alpha, newDepth, position, info, &pv_from_here);

        // Full depth alpha-beta window search
        if (pvNode && ((score > alpha && score < beta) || movesTried == 1))
        {
            score = -AlphaBeta(-beta, -alpha, newDepth, position, info, &pv_from_here);
        }

        // Undo the move
        unmakeMove(position);

        // Found a new best move in this position
        if (score > bestScore) {

            bestScore = score;
            bestMove = move;

            // Update the Principle Variation
            if ((score > alpha && pvNode) || (root && movesTried == 1)) {

                pv->length = 1 + pv_from_here.length;
                pv->input[0] = move;
                memcpy(pv->input + 1, pv_from_here.input, sizeof(int) * pv_from_here.length);
            }

            // If score beats alpha we update alpha
            if (score > alpha) {

                alpha = score;

                // Update search history
                if (quiet)
                    position->history[moving(bestMove)][to(bestMove)] += depth * depth;

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

    // Checkmate or stalemate
    if (!movesTried)
    {
        return inCheck ? -infinite + position->ply : 0;
    }

    int flag = bestScore >= beta ? BOUND_LOWER
             : alpha != oldAlpha ? BOUND_EXACT
                                 : BOUND_UPPER;

    StoreTTEntry(tte, posKey, bestMove, ScoreToTT(bestScore, position->ply), depth, flag);

    return bestScore;
}

// Aspiration window
static int AspirationWindow(Board * position, SearchInfo * info) {

    const int score = info->score;
    // Dynamic bonus increasing initial window and relaxation delta
    const int bonus = score * score;
    const int initialWindow = 12 + bonus / 2048;
    const int delta = 64 + bonus / 256;
    // Initial window
    int alpha = MAX(score - initialWindow, -infinite);
    int beta = MIN(score + initialWindow, infinite);
    // Counter for failed searches, bounds are relaxed more for each successive fail
    unsigned fails = 0;

    while (true) {
        int result = AlphaBeta(alpha, beta, info->IDDepth, position, info, &info->pv);
        // Result within the bounds is accepted as correct
        if (result >= alpha && result <= beta)
            return result;
        // Failed low, relax lower bound and search again
        else if (result < alpha) {
            alpha -= delta << fails++;
            alpha = MAX(alpha, -infinite);
            // Failed high, relax upper bound and search again
        }
        else if (result > beta) {
            beta += delta << fails++;
            beta = MIN(beta, infinite);
        }
    }
}

void searchPosition(Board* position, SearchInfo* info)
{
    int score;
    unsigned depth;
    PV pv;
    
    clearForSearch(position, info);

    // Iterative deepening
    for (info->IDDepth = 1; info->IDDepth <= info->depth; ++info->IDDepth) {

        
        if (setjmp(info->jumpBuffer)) break;
        
        // Search position, using aspiration windows for higher depths
        if (info->IDDepth > 6)
            info->score = AspirationWindow(position, info);
        else
            info->score = AlphaBeta(-infinite, infinite, info->IDDepth, position, info, &info->pv);

        // Print thinking
        PrintThinking(info, position);

        info->bestMove = info->pv.input[0];
    }

    // Print conclusion
    PrintConclusion(info);

}

//uci

#define INPUTBUFFER 400 * 6

void parseGo(Board* position, SearchInfo* info, char* input) {

    int depth = -1, movesToGo = 30, movetime = -1;
    int time = -1, increment = 0;
    char* ptr = NULL;
    info->timeSet = 0;

    if ((ptr = strstr(input, "binc")) && position->side == BLACK) {
        increment = atoi(ptr + 5);
    }

    if ((ptr = strstr(input, "winc")) && position->side == WHITE) {
        increment = atoi(ptr + 5);
    }

    if ((ptr = strstr(input, "wtime")) && position->side == WHITE) {
        time = atoi(ptr + 6);
    }

    if ((ptr = strstr(input, "btime")) && position->side == BLACK) {
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
        info->depth = MAX_DEPTH;
    }

    printf("time:%d start:%d stop:%d depth:%d timeset:%d\n",
        time, info->startTime, info->endTime, info->depth, info->timeSet);
    searchPosition(position, info);
}

void parsePosition(Board* position, char* input) {

    input += 9;
    char* ptr = input;

    if (strncmp(input, "startpos", 8) == 0)
    {
        initBoard(position, STARTING_FEN);
    }
    else
    {
        ptr = strstr(input, "fen");
        if (ptr == NULL)
        {
            initBoard(position, STARTING_FEN);
        }
        else
        {
            ptr += 4;
            initBoard(position, ptr);
        }
    }

    ptr = strstr(input, "moves");
    int move;

    if (ptr != NULL) {
        ptr += 6;
        while (*ptr) {
            move = parseMove(position, ptr);
            if (move < 0) break;
            makeMove(position, move);
            position->ply = 0;
            while (*ptr && *ptr != ' ') {
                ptr++;
            }
            ptr++;
        }
    }
    printBoard(position);
}

void uci(Board* position, SearchInfo* info) {

    setbuf(stdin, NULL);
    setbuf(stdout, NULL);

    char input[INPUTBUFFER];
    printf("id name Novice2.0\n");
    printf("id author Jay Warendorff\n");
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
            parsePosition(position, input);
        }
        else if (!strncmp(input, "ucinewgame", 10)) {
            parsePosition(position, "position startpos\n");
        }
        else if (!strncmp(input, "go", 2)) {
            parseGo(position, info, input);
        }
        else if (!strncmp(input, "quit", 4)) {
            info->quit = 1;
        }
        else if (!strncmp(input, "uci", 3)) {
            printf("id name Novice2.0\n");
            printf("id author Jay Warendorff\n");
            printf("uciok\n");
        }
        if (info->quit) {
            break;
        }
    }
}

int main() {

    // debug mode variable
    int debug = 0;

    // if debugging - debug 1
    if (debug)
    {
        initAll();
        Board position[1];
        MoveList list[1];
        //initBoard(position, STARTING_FEN);
        initBoard(position, "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10");
        PerftTest(1, position);
        while (1);
    }
    else
    {
        initAll();

        Board position[1];
        SearchInfo info[1];
        info->quit = 0;
        TT.currentMB = 0;
        TT.requestedMB = 128;

        setbuf(stdin, NULL);
        setbuf(stdout, NULL);

        char input[3000];
        while (1) {
            memset(&input[0], 0, sizeof(input));

            fflush(stdout);
            if (!fgets(input, 256, stdin))
                continue;
            if (input[0] == '\n')
                continue;
            if (!strncmp(input, "uci", 3)) {
                uci(position, info);
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

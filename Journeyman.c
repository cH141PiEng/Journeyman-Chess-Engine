#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include <windows.h>
#include "math.h"
#include <stdbool.h>
//#include <unistd.h> //needed for compiling with gcc, but does not work on VS 2019

//Journeyman 1.6
//Uses _BitScanForward64, __popcnt64
//Modified version of the Video Instruction Chess Engine video #87
//Instead of a 120 array board, uses bitboards
//Increased history table ~128 MB
//Tapered evaluation
//Evaluates more aspects of the position
//Razoring, Reverse futility pruning and LMR as in CeeChess 1.3

typedef unsigned long long U64;

#define NAME "Journeyman 1.6"
#define BRD_SQ_NUM 64

#define MAXGAMEMOVES 2048//Max # of half moves expected in a game
//Needed for the history of the game
#define MAXPOSITIONMOVES 256
#define MAXDEPTH 64

#define START_FEN  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

#define infinite 30000
#define ISMATE (infinite - MAXDEPTH)

//Numbers for not a piece and the pieces:
enum { e = 0, P, N, B, R, Q, K, p, n, b, r, q, k };

enum { FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H };
enum { RANK_1, RANK_2, RANK_3, RANK_4, RANK_5, RANK_6, RANK_7, RANK_8 };

enum { WHITE, BLACK, BOTH };

enum {
    a1 = 0, b1, c1, d1, e1, f1, g1, h1,
    a2 = 8, b2, c2, d2, e2, f2, g2, h2,
    a3 = 16, b3, c3, d3, e3, f3, g3, h3,
    a4 = 24, b4, c4, d4, e4, f4, g4, h4,
    a5 = 32, b5, c5, d5, e5, f5, g5, h5,
    a6 = 40, b6, c6, d6, e6, f6, g6, h6,
    a7 = 48, b7, c7, d7, e7, f7, g7, h7,
    a8 = 56, b8, c8, d8, e8, f8, g8, h8, no_sq
};

#define FALSE 0
#define TRUE  1

/*
//enum whether castling is possible for the four types or not...represented using first 4 bits

    bin  dec

   0001    1  white king can castle to the king side
   0010    2  white king can castle to the queen side
   0100    4  black king can castle to the king side
   1000    8  black king can castle to the queen side

   examples

   1111       both sides an castle both directions
   1001       black king => queen side
              white king => king side

*/
enum { WKCA = 1, WQCA = 2, BKCA = 4, BQCA = 8 };

typedef struct {
    int move;
    int score;
} S_MOVE;

typedef struct {
    S_MOVE moves[MAXPOSITIONMOVES];
    int count;//count of # of moves in the move list
} S_MOVELIST;

enum { HFNONE, HFALPHA, HFBETA, HFEXACT };

typedef struct {
    U64 posKey;
    int move;
    int score;
    int depth;
    int flags;
} S_HASHENTRY;

typedef struct {
    S_HASHENTRY* pTable;
    int numEntries;
    int newWrite;
    int overWrite;
    int hit;
    int cut;
} S_HASHTABLE;

//To undo any move if we want to go back to a previous stage of the game 
typedef struct {

    int move;
    int castlePerm;
    int enPas;
    int fiftyMove;
    U64 posKey;

} S_UNDO;

typedef struct {

    int pieces[BRD_SQ_NUM];//Pieces corresponding to each squares
    U64 PieceBB[13];//bitboards corresponding to piece enumeration above
    U64 ColorBB[3];//bitboards corresponding to color enumeration above

    int side;//Current side to move
    int enPas;//Check for EnPass move
    int fiftyMove;//Uses half moves so when 100 game drawn

    int ply;//How many half moves we are into the current search
    int hisPly;//How many half moves in total game so far - needed for looking back and determining repetitions

    int castlePerm;//stores castle permissions

    U64 posKey;//A unique key/number generated for each position (move)
    //representing the position on the board, used to detect using
    //the history array if we have repetitions in the position
    //posKey idea introduced in video 11

    int bigPce[2];//number of all pieces except pawns

    S_UNDO history[MAXGAMEMOVES];//Stores all of the previous game states with things which are stated in the undo 
    //structure and also it helps to check if a move is repeated or not. Can do so by using the 
    //hisPly as the index and going back to all the states and see if the posKey is repeated or not

    S_HASHTABLE HashTable[1];//Keep one HashTable in the main structure for the current board position...in the form of 
                         // a pointer which is already given memory.
    int PvArray[MAXDEPTH];//Stores the best line of moves up to certain depth for the given position

    int searchHistory[13][BRD_SQ_NUM];//Used for move ordering for beta cutoff
    int searchKillers[2][MAXDEPTH];//Only used for move ordering. These are the
                                   //non capture moves which are causing the beta cutoff 

} S_BOARD;

//For use by the GUI 
typedef struct {

    int starttime;
    int stoptime;
    int depth;
    int timeset;
    int movestogo;

    long nodes;

    int quit;
    int stopped;

    float fh;
    float fhf;
    int nullCut;

} S_SEARCHINFO;

/* GAME MOVE */

/*
0000 0000 0000 0000 0000 0111 1111 -> From 0x7F
0000 0000 0000 0011 1111 1000 0000 -> To >> 7, 0x7F
0000 0000 0011 1100 0000 0000 0000 -> Captured >> 14, 0xF
0000 0000 0100 0000 0000 0000 0000 -> EP 0x40000
0000 0000 1000 0000 0000 0000 0000 -> Pawn Start 0x80000
0000 1111 0000 0000 0000 0000 0000 -> Promoted Piece >> 20, 0xF
0001 0000 0000 0000 0000 0000 0000 -> Castle 0x1000000
*/

//To find the masked value which will give the index of the move,
//captured type index and promoted type index 

#define FROMSQ(m) ((m) & 0x7F)
#define TOSQ(m) (((m)>>7) & 0x7F)
#define CAPTURED(m) (((m)>>14) & 0xF)
#define PROMOTED(m) (((m)>>20) & 0xF)

//The values which will &'d with moves to see if the corrsponding bits are set or not
//uses hexadecimal numbers 
#define MFLAGEP 0x40000
#define MFLAGPS 0x80000
#define MFLAGCA 0x1000000

//Flag to see if the capturing is happening or not
#define MFLAGCAP 0x7C000
#define MFLAGPROM 0xF00000

#define NOMOVE 0


/* MACROS */

//Returns the square #, given the file and rank
#define FR2SQ(f,r) ( (f) + ( (r) * 8 ) )
#define POP(b) PopBit(b)
#define CNT(b) CountBits(b)
#define CLRBIT(bb,sq) ((bb) &= ClearMask[(sq)])
#define SETBIT(bb,sq) ((bb) |= SetMask[(sq)])

#define bitCount(bb) ((int) (__popcnt64(bb)))

#define IsBQ(p) (PieceBishopQueen[(p)])
#define IsRQ(p) (PieceRookQueen[(p)])
#define IsKn(p) (PieceKnight[(p)])
#define IsKi(p) (PieceKing[(p)])

#define COL(sq) ((sq)&7)
#define ROW(pos) (((unsigned)pos)>>3)

/* GLOBALS */

//For setting and clearing bits in bitboards
U64 SetMask[64];
U64 ClearMask[64];

U64 PieceKeys[13][64];//Stores a random 64 bit number which states that that type of element is present on that sq number 
U64 SideKey;//Stores a random number if it is the turn of white in the game 
U64 CastleKeys[16];//Stores 64 random numbers which represent each possible type of castle positions which are 0 to 15

//Arrays for indexing and printing using the type of pieces in the ENUM as the indexes
char PceChar[];
char SideChar[];
char RankChar[];
char FileChar[];

int PieceBig[13];
int PieceVal[13];
int PieceCol[13];

int FilesBrd[BRD_SQ_NUM];
int RanksBrd[BRD_SQ_NUM];

U64 FileBBMask[8];
U64 RankBBMask[8];

// data.c

char PceChar[] = ".PNBRQKpnbrqk";
char SideChar[] = "wb-";
char RankChar[] = "12345678";
char FileChar[] = "abcdefgh";

int PieceBig[13] = { FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE };
int PieceVal[13] = { 0, 100, 325, 325, 550, 1000, 50000, 100, 325, 325, 550, 1000, 50000 };
int PieceCol[13] = { BOTH, WHITE, WHITE, WHITE, WHITE, WHITE, WHITE,
    BLACK, BLACK, BLACK, BLACK, BLACK, BLACK };

int PiecePawn[13] = { FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE };
int PieceKnight[13] = { FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE };
int PieceKing[13] = { FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE };
int PieceRookQueen[13] = { FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE };
int PieceBishopQueen[13] = { FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE };

//initialize

// 0000 000000000000000 000000000000000 000000000000000 000000000000000

//Include stdlib.h to use rand() which generates a 15 bit random #
//Fills 64 bits with random #s
#define RAND_64 	((U64)rand() | \
					(U64)rand() << 15 | \
					(U64)rand() << 30 | \
					(U64)rand() << 45 | \
					((U64)rand() & 0xf) << 60 )
//This is a modified rand function which generates 64 bit random numbers for our hashkey values

void InitFilesRanksBrd() {

    int index = 0;
    int file = FILE_A;
    int rank = RANK_1;
    int sq = a1;

    for (rank = RANK_1; rank <= RANK_8; ++rank) {
        for (file = FILE_A; file <= FILE_H; ++file) {
            sq = FR2SQ(file, rank);
            FilesBrd[sq] = file;
            RanksBrd[sq] = rank;
        }
    }
}

void InitHashKeys() {

    int index = 0;
    int index2 = 0;
    for (index = 0; index < 13; ++index) {
        for (index2 = 0; index2 < 64; ++index2) {
            PieceKeys[index][index2] = RAND_64;
        }
    }
    SideKey = RAND_64;
    for (index = 0; index < 16; ++index) {
        CastleKeys[index] = RAND_64;
    }
}

void InitBitMasks() {
    int index = 0;

    for (index = 0; index < 64; index++) {
        SetMask[index] = 0ULL;
        ClearMask[index] = 0ULL;
    }

    for (index = 0; index < 64; index++) {
        SetMask[index] |= (1ULL << index);
        ClearMask[index] = ~SetMask[index];
    }
}

U64 IsolatedPawnMasks[64];
U64 PassedPawnMasks[2][64];
U64 PawnAttackMasks[2][64];
U64 PawnAdvanceMasks[2][64];
U64 PawnConnectedMasks[2][64];
U64 OutpostSquareMasks[2][64];
U64 OutpostRanks[2];

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

void initalizeMasks() {

    int i, j, file, rank;
    U64 files;

    // INITALIZE ISOLATED PAWN MASKS
    for (i = 0; i < 64; i++) {
        file = i % 8;

        if (file > 0 && file < 7)
            IsolatedPawnMasks[i] = FILES[file + 1] | FILES[file - 1];
        else if (file > 0)
            IsolatedPawnMasks[i] = FILES[file - 1];
        else
            IsolatedPawnMasks[i] = FILES[file + 1];
    }

    // INITALIZE PASSED PAWN MASKS AND OUTPOST MASKS
    for (i = 0; i < 64; i++) {
        file = i % 8;
        rank = i / 8;

        files = IsolatedPawnMasks[i] | FILES[file];

        PassedPawnMasks[0][i] = files;
        for (j = rank; j >= 0; j--)
            PassedPawnMasks[0][i] &= ~(RANKS[j]);

        PassedPawnMasks[1][i] = files;
        for (j = rank; j <= 7; j++)
            PassedPawnMasks[1][i] &= ~(RANKS[j]);

        OutpostSquareMasks[0][i] = PassedPawnMasks[0][i] & ~FILES[file];
        OutpostSquareMasks[1][i] = PassedPawnMasks[1][i] & ~FILES[file];
    }

    // INITALIZE ATTACK-SQ PAWN MASKS
    for (i = 0; i < 64; i++) {
        file = i % 8;
        rank = i / 8;

        PawnAttackMasks[0][i] = 0ULL;
        PawnAttackMasks[1][i] = 0ULL;

        if (rank == 0) {
            PawnAttackMasks[1][i] |= (1ULL << i) << 7;
            PawnAttackMasks[1][i] |= (1ULL << i) << 9;
        }

        else if (rank == 7) {
            PawnAttackMasks[0][i] |= (1ULL << i) >> 7;
            PawnAttackMasks[0][i] |= (1ULL << i) >> 9;
        }

        else {
            PawnAttackMasks[0][i] |= (1ULL << i) >> 7;
            PawnAttackMasks[0][i] |= (1ULL << i) >> 9;
            PawnAttackMasks[1][i] |= (1ULL << i) << 7;
            PawnAttackMasks[1][i] |= (1ULL << i) << 9;
        }

        if (file == 0) {
            PawnAttackMasks[0][i] &= ~FILEH;
            PawnAttackMasks[1][i] &= ~FILEH;
        }

        else if (file == 7) {
            PawnAttackMasks[0][i] &= ~FILEA;
            PawnAttackMasks[1][i] &= ~FILEA;
        }
    }

    // INITALIZE PAWN-MAY-ADVANCE MASKS
    for (i = 0; i < 64; i++) {
        rank = i / 8;

        PawnAdvanceMasks[0][i] = 0ULL;
        PawnAdvanceMasks[1][i] = 0ULL;

        if (rank == 0)
            PawnAdvanceMasks[0][i] = (1ULL << i) << 8;
        else if (rank == 7)
            PawnAdvanceMasks[1][i] = (1ULL << i) >> 8;
        else {
            PawnAdvanceMasks[0][i] = (1ULL << i) << 8;
            PawnAdvanceMasks[1][i] = (1ULL << i) >> 8;
        }
    }

    OutpostRanks[WHITE] = RANK4 | RANK5 | RANK6;
    OutpostRanks[BLACK] = RANK3 | RANK4 | RANK5;

    // INITALIZE PAWN-CONNECTED MASKS
    for (i = 8; i < 56; i++) {
        file = i % 8;

        if (file == 0) {
            PawnConnectedMasks[0][i] = (1ULL << (i + 1)) | (1ULL << (i - 7));
            PawnConnectedMasks[1][i] = (1ULL << (i + 1)) | (1ULL << (i + 9));
        }

        else if (file == 7) {
            PawnConnectedMasks[0][i] = (1ULL << (i - 1)) | (1ULL << (i - 9));
            PawnConnectedMasks[1][i] = (1ULL << (i - 1)) | (1ULL << (i + 7));
        }

        else {
            PawnConnectedMasks[0][i] = (1ULL << (i - 1)) | (1ULL << (i - 9)) | (1ULL << (i + 1)) | (1ULL << (i - 7));
            PawnConnectedMasks[1][i] = (1ULL << (i - 1)) | (1ULL << (i + 7)) | (1ULL << (i + 1)) | (1ULL << (i + 9));
        }
    }
}

U64 KingAttacks[64];
U64 KnightAttacks[64];

int fileArray[64];
int rankArray[64];

U64 rookAttacksEmpty[64];
U64 bishopAttacksEmpty[64];

U64 bitRays[8][64];

void initFileRankArrays() {
    for (int square = 0; square < 64; square++) {
        fileArray[square] = square % 8;
        rankArray[square] = square / 8;
    }
}

#define getSquare(file, rank) (((rank) * 8) + (file))
#define onBoard(file, rank) (file >= FILE_A && file <= FILE_H && rank >= RANK_1 && rank <= RANK_8)

void initKnightAttacks() {
    for (int square = 0; square < 64; square++) {
        int originalFile = fileArray[square];
        int originalRank = rankArray[square];

        int file, rank;
        KnightAttacks[square] = 0ULL;

        file = originalFile + 2; rank = originalRank + 1;
        if (onBoard(file, rank)) SETBIT(KnightAttacks[square], getSquare(file, rank));
        rank = originalRank - 1;
        if (onBoard(file, rank)) SETBIT(KnightAttacks[square], getSquare(file, rank));
        file = originalFile + 1; rank = originalRank + 2;
        if (onBoard(file, rank)) SETBIT(KnightAttacks[square], getSquare(file, rank));
        rank = originalRank - 2;
        if (onBoard(file, rank)) SETBIT(KnightAttacks[square], getSquare(file, rank));
        file = originalFile - 1; rank = originalRank + 2;
        if (onBoard(file, rank)) SETBIT(KnightAttacks[square], getSquare(file, rank));
        rank = originalRank - 2;
        if (onBoard(file, rank)) SETBIT(KnightAttacks[square], getSquare(file, rank));
        file = originalFile - 2; rank = originalRank + 1;
        if (onBoard(file, rank)) SETBIT(KnightAttacks[square], getSquare(file, rank));
        rank = originalRank - 1;
        if (onBoard(file, rank)) SETBIT(KnightAttacks[square], getSquare(file, rank));
    }
}

void initKingAttacks() {
    for (int square = 0; square < 64; square++) {
        int file = fileArray[square];
        int rank = rankArray[square];

        KingAttacks[square] = 0ULL;

        if (onBoard(file + 1, rank + 1)) SETBIT(KingAttacks[square], getSquare(file + 1, rank + 1));
        if (onBoard(file + 1, rank)) SETBIT(KingAttacks[square], getSquare(file + 1, rank));
        if (onBoard(file + 1, rank - 1)) SETBIT(KingAttacks[square], getSquare(file + 1, rank - 1));
        if (onBoard(file, rank + 1)) SETBIT(KingAttacks[square], getSquare(file, rank + 1));
        if (onBoard(file, rank - 1)) SETBIT(KingAttacks[square], getSquare(file, rank - 1));
        if (onBoard(file - 1, rank + 1)) SETBIT(KingAttacks[square], getSquare(file - 1, rank + 1));
        if (onBoard(file - 1, rank)) SETBIT(KingAttacks[square], getSquare(file - 1, rank));
        if (onBoard(file - 1, rank - 1)) SETBIT(KingAttacks[square], getSquare(file - 1, rank - 1));
    }
}

void initRookAttacks() {
    for (int square = 0; square < 64; square++) {
        rookAttacksEmpty[square] = 0ULL;

        int file = fileArray[square];
        int rank = rankArray[square];

        for (int f = file + 1; f <= FILE_H; f++) {
            SETBIT(rookAttacksEmpty[square], getSquare(f, rank));
        }
        for (int f = file - 1; f >= FILE_A; f--) {
            SETBIT(rookAttacksEmpty[square], getSquare(f, rank));
        }
        for (int r = rank + 1; r <= RANK_8; r++) {
            SETBIT(rookAttacksEmpty[square], getSquare(file, r));
        }
        for (int r = rank - 1; r >= RANK_1; r--) {
            SETBIT(rookAttacksEmpty[square], getSquare(file, r));
        }
    }
}

void initBishopAttacks() {
    for (int square = 0; square < 64; square++) {
        bishopAttacksEmpty[square] = 0ULL;

        for (int tr = square + 9; (tr % 8 > 0) && (tr < 64); tr += 9) {
            SETBIT(bishopAttacksEmpty[square], tr);
        }
        for (int tl = square + 7; (tl % 8 < 7) && (tl < 64); tl += 7) {
            SETBIT(bishopAttacksEmpty[square], tl);
        }
        for (int br = square - 7; (br % 8 > 0) && (br >= 0); br -= 7) {
            SETBIT(bishopAttacksEmpty[square], br);
        }
        for (int bl = square - 9; (bl % 8 < 7) && (bl >= 0); bl -= 9) {
            SETBIT(bishopAttacksEmpty[square], bl);
        }
    }
}

enum { NORTH, NORTH_EAST, EAST, SOUTH_EAST, SOUTH, SOUTH_WEST, WEST, NORTH_WEST };

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
            SETBIT(bitRays[NORTH][i], sq);
        }
        for (int sq = i + 9; sq < 64; sq += 9) {
            if (fileArray[sq] == FILE_A) break;
            SETBIT(bitRays[NORTH_EAST][i], sq);
        }
        for (int sq = i + 1; sq < 64; sq += 1) {
            if (fileArray[sq] == FILE_A) break;
            SETBIT(bitRays[EAST][i], sq);
        }
        for (int sq = i - 7; sq >= 0; sq -= 7) {
            if (fileArray[sq] == FILE_A) break;
            SETBIT(bitRays[SOUTH_EAST][i], sq);
        }
        for (int sq = i - 8; sq >= 0; sq -= 8) {
            SETBIT(bitRays[SOUTH][i], sq);
        }
        for (int sq = i - 9; sq >= 0; sq -= 9) {
            if (fileArray[sq] == FILE_H) break;
            SETBIT(bitRays[SOUTH_WEST][i], sq);
        }
        for (int sq = i - 1; sq >= 0; sq -= 1) {
            if (fileArray[sq] == FILE_H) break;
            SETBIT(bitRays[WEST][i], sq);
        }
        for (int sq = i + 7; sq < 64; sq += 7) {
            if (fileArray[sq] == FILE_H) break;
            SETBIT(bitRays[NORTH_WEST][i], sq);
        }
    }
}

/*
PV Move
Cap -> MvvLVA
Killers
HistoryScore

*/

//For the move ordering method of most valuable victim least valuable attacker - example: a pawn captures queen initially make
//everything to zero so that the score of non capture moves remain zero
const int VictimScore[13] = { 0, 100, 200, 300, 400, 500, 600, 100, 200, 300, 400, 500, 600 };
static int MvvLvaScores[13][13];

//Move ordering depth 7 results:
// 16907660 nodes time 8688 no ordering
//   842385 nodes and time 484 with ordering

//Initialise the MvvLvaScores array for every combination of 2 pieces
void InitMvvLva() {
    int Attacker;
    int Victim;
    for (Attacker = P; Attacker <= k; ++Attacker) {
        for (Victim = P; Victim <= k; ++Victim) {
            MvvLvaScores[Victim][Attacker] = VictimScore[Victim] + 6 - (VictimScore[Attacker] / 100);
        }
    }
}

U64 KingMap[64];

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

int LMRTable[32][32];

void InitSearch() {
    // creating the LMR table entries (idea from Ethereal)
    for (int moveDepth = 1; moveDepth < 32; moveDepth++)
        for (int played = 1; played < 32; played++)
            LMRTable[moveDepth][played] = 0.75 + (log(moveDepth) * log(played) / 2.25);
}

void AllInit() {
    InitBitMasks();

    initalizeMasks();

    initFileRankArrays();

    InitHashKeys();

    InitFilesRanksBrd();

    initKnightAttacks();
    initKingAttacks();
    initRookAttacks();
    initBishopAttacks();

    initBitRays();
    InitMvvLva();

    generateKingMap();
    InitSearch();
}

// bitboards

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

int Msb(const U64 bb) {
    unsigned long index = -1;
    _BitScanReverse64(&index, bb);
    return index;
}

#define CountBits(bb) ((int) (__popcnt64(bb)))

void PrintBitBoard(U64 bb) {

    U64 shiftMe = 1ULL;

    int rank = 0;
    int file = 0;
    int sq = 0;

    printf("\n");
    for (rank = RANK_8; rank >= RANK_1; --rank) {
        for (file = FILE_A; file <= FILE_H; ++file) {
            sq = FR2SQ(file, rank);

            if ((shiftMe << sq) & bb)
                printf("X");
            else
                printf("-");

        }
        printf("\n");
    }
    printf("\n\n");
}

int ColorOf(S_BOARD* pos, int sq)
{
    if (((pos->ColorBB[0]) & SetMask[sq]) != 0ULL)
    {
        return WHITE;
    }
    else if (((pos->ColorBB[1]) & SetMask[sq]) != 0ULL)
    {
        return BLACK;
    }
    else
    {
        return 2;
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

U64 RookAttacks(const S_BOARD* pos, int sq) {
    U64 occ = pos->ColorBB[2] | 0x8000000000000001;
    int n = peekBit(occ & (bitRays[NORTH][sq] | SetMask[63]));
    int e = peekBit(occ & (bitRays[EAST][sq] | SetMask[63]));
    int s = peekBitReverse(occ & (bitRays[SOUTH][sq] | SetMask[0]));
    int w = peekBitReverse(occ & (bitRays[WEST][sq] | SetMask[0]));
    return rookAttacksEmpty[sq] ^ bitRays[NORTH][n] ^ bitRays[EAST][e] ^ bitRays[SOUTH][s] ^ bitRays[WEST][w];
}

U64 BishopAttacks(const S_BOARD* pos, int sq) {
    U64 occ = pos->ColorBB[2] | 0x8000000000000001;
    int nw = peekBit(occ & (bitRays[NORTH_WEST][sq] | SetMask[63]));
    int ne = peekBit(occ & (bitRays[NORTH_EAST][sq] | SetMask[63]));
    int sw = peekBitReverse(occ & (bitRays[SOUTH_WEST][sq] | SetMask[0]));
    int se = peekBitReverse(occ & (bitRays[SOUTH_EAST][sq] | SetMask[0]));
    return bishopAttacksEmpty[sq] ^ bitRays[NORTH_WEST][nw] ^ bitRays[NORTH_EAST][ne] ^ bitRays[SOUTH_WEST][sw] ^ bitRays[SOUTH_EAST][se];
}

const U64 FILE_A_MASK = 0x0101010101010101;
const U64 FILE_H_MASK = 0x8080808080808080;
const U64 NOT_FILE_A_MASK = 0xFEFEFEFEFEFEFEFE;
const U64 NOT_FILE_H_MASK = 0x7F7F7F7F7F7F7F7F;
const U64 RANK_1_MASK = 0x00000000000000FF;
const U64 RANK_3_MASK = 0x0000000000FF0000;
const U64 RANK_6_MASK = 0x0000FF0000000000;
const U64 RANK_8_MASK = 0xFF00000000000000;
const U64 NOT_RANK_8_MASK = 0x00FFFFFFFFFFFFFF;

int AttackedByWhite(const S_BOARD* pos, int sq)
{
    if (KnightAttacks[sq] & pos->PieceBB[N]) return 1;
    if (BishopAttacks(pos, sq) & (pos->PieceBB[B] | pos->PieceBB[Q])) return 1;
    if (RookAttacks(pos, sq) & (pos->PieceBB[R] | pos->PieceBB[Q])) return 1;
    if ((((pos->PieceBB[P] << 7) & NOT_FILE_H_MASK) | ((pos->PieceBB[P] << 9) & NOT_FILE_A_MASK)) & SetMask[sq]) return 1;
    if (KingAttacks[sq] & pos->PieceBB[K]) return 1;
    return 0;
}

int AttackedByBlack(const S_BOARD* pos, int sq) {
    if (KnightAttacks[sq] & pos->PieceBB[n])
    {
        return 1;
    }
    if (BishopAttacks(pos, sq) & (pos->PieceBB[b] | pos->PieceBB[q]))
    { 
        return 1;
    }
    if (RookAttacks(pos, sq) & (pos->PieceBB[r] | pos->PieceBB[q]))
    {
        return 1;
    }
    if ((((pos->PieceBB[p] >> 9)& NOT_FILE_H_MASK) | ((pos->PieceBB[p] >> 7)& NOT_FILE_A_MASK))& SetMask[sq])
    { 
        return 1;
    }
    if (KingAttacks[sq] & pos->PieceBB[k])
    { 
        return 1;
    }
    return 0;
}

int UnderCheck(const S_BOARD* pos, int side) {
    if (side == WHITE) return AttackedByBlack(pos, peekBit(pos->PieceBB[K]));
    else return AttackedByWhite(pos, peekBit(pos->PieceBB[k]));
}

// makemove

#define HASH_PCE(pce,sq) (pos->posKey ^= (PieceKeys[(pce)][(sq)]))
#define HASH_CA (pos->posKey ^= (CastleKeys[(pos->castlePerm)]))
#define HASH_SIDE (pos->posKey ^= (SideKey))
#define HASH_EP (pos->posKey ^= (PieceKeys[e][(pos->enPas)]))

const int CastlePerm[120] = {
    13, 15, 15, 15, 12, 15, 15, 14,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    7, 15, 15, 15,  3, 15, 15, 11
};

static void ClearPiece(const int sq, S_BOARD* pos) {

    int pce = pos->pieces[sq];

    int col = PieceCol[pce];

    HASH_PCE(pce, sq);

    pos->pieces[sq] = e;

    CLRBIT(pos->PieceBB[pce], sq);
    CLRBIT(pos->ColorBB[col], sq);
    CLRBIT(pos->ColorBB[BOTH], sq);

    if (PieceBig[pce]) {
        pos->bigPce[col]--;
    }

}

static void AddPiece(const int sq, S_BOARD* pos, const int pce) {

    int col = PieceCol[pce];

    HASH_PCE(pce, sq);

    pos->pieces[sq] = pce;

    SETBIT(pos->PieceBB[pce], sq);
    SETBIT(pos->ColorBB[col], sq);
    SETBIT(pos->ColorBB[BOTH], sq);

    if (PieceBig[pce]) {
        pos->bigPce[col]++;
    }
}

static void MovePiece(const int from, const int to, S_BOARD* pos) {

    int index = 0;
    int pce = pos->pieces[from];
    int col = PieceCol[pce];

    HASH_PCE(pce, from);
    pos->pieces[from] = e;

    HASH_PCE(pce, to);
    pos->pieces[to] = pce;

    CLRBIT(pos->PieceBB[pce], from);
    CLRBIT(pos->ColorBB[col], from);
    CLRBIT(pos->ColorBB[BOTH], from);

    SETBIT(pos->PieceBB[pce], to);
    SETBIT(pos->ColorBB[col], to);
    SETBIT(pos->ColorBB[BOTH], to);
}

void TakeMove(S_BOARD* pos);
int SqAttacked(int sq, int side, S_BOARD* pos);
void PrintBoard(const S_BOARD* pos);

int MakeMove(S_BOARD* pos, int move) {

    int from = FROMSQ(move);
    int to = TOSQ(move);
    int side = pos->side;

    pos->history[pos->hisPly].posKey = pos->posKey;

    if (move & MFLAGEP) {
        if (side == WHITE) {
            ClearPiece(to - 8, pos);
        }
        else {
            ClearPiece(to + 8, pos);
        }
    }
    else if (move & MFLAGCA) {
        switch (to) {
        case c1:
            MovePiece(a1, d1, pos);
            break;
        case c8:
            MovePiece(a8, d8, pos);
            break;
        case g1:
            MovePiece(h1, f1, pos);
            break;
        case g8:
            MovePiece(h8, f8, pos);
            break;
        default:
            break;
        }
    }

    if (pos->enPas != no_sq) HASH_EP;
    HASH_CA;

    pos->history[pos->hisPly].move = move;
    pos->history[pos->hisPly].fiftyMove = pos->fiftyMove;
    pos->history[pos->hisPly].enPas = pos->enPas;
    pos->history[pos->hisPly].castlePerm = pos->castlePerm;

    pos->castlePerm &= CastlePerm[from];
    pos->castlePerm &= CastlePerm[to];
    pos->enPas = no_sq;

    HASH_CA;

    int captured = CAPTURED(move);
    pos->fiftyMove++;

    if (captured != e) {
        ClearPiece(to, pos);
        pos->fiftyMove = 0;
    }

    pos->hisPly++;
    pos->ply++;

    if (((pos->pieces[from]) == P) || ((pos->pieces[from]) == p)){
        pos->fiftyMove = 0;
        if (move & MFLAGPS) {
            if (side == WHITE) {
                pos->enPas = from + 8;
            }
            else {
                pos->enPas = from - 8;
            }
            HASH_EP;
        }
    }

    MovePiece(from, to, pos);

    int prPce = PROMOTED(move);
    if (prPce != e) {
        ClearPiece(to, pos);
        AddPiece(to, pos, prPce);
    }

    pos->ColorBB[BOTH] = pos->ColorBB[WHITE] | pos->ColorBB[BLACK];
    pos->PieceBB[e] = ~pos->ColorBB[BOTH];

    pos->side ^= 1;
    HASH_SIDE;

    if (UnderCheck(pos, pos->side^1)) {
        TakeMove(pos);
        return FALSE;
    }

    return TRUE;
}

void TakeMove(S_BOARD* pos) {

    pos->hisPly--;
    pos->ply--;

    int move = pos->history[pos->hisPly].move;
    int from = FROMSQ(move);
    int to = TOSQ(move);

    if (pos->enPas != no_sq) HASH_EP;
    HASH_CA;

    pos->castlePerm = pos->history[pos->hisPly].castlePerm;
    pos->fiftyMove = pos->history[pos->hisPly].fiftyMove;
    pos->enPas = pos->history[pos->hisPly].enPas;

    if (pos->enPas != no_sq) HASH_EP;
    HASH_CA;

    pos->side ^= 1;
    HASH_SIDE;

    if (MFLAGEP & move) {
        if (pos->side == WHITE) {
            AddPiece(to - 8, pos, p);
        }
        else {
            AddPiece(to + 8, pos, P);
        }
    }
    else if (MFLAGCA & move) {
        switch (to) {
        case c1: MovePiece(d1, a1, pos); break;
        case c8: MovePiece(d8, a8, pos); break;
        case g1: MovePiece(f1, h1, pos); break;
        case g8: MovePiece(f8, h8, pos); break;
        default: break;
        }
    }

    MovePiece(to, from, pos);

    int captured = CAPTURED(move);
    if (captured != e) {
        AddPiece(to, pos, captured);
    }

    if (PROMOTED(move) != e) {
        ClearPiece(from, pos);
        AddPiece(from, pos, (PieceCol[PROMOTED(move)] == WHITE ? P : p));
    }
}

void MakeNullMove(S_BOARD* pos) {

    pos->ply++;
    pos->history[pos->hisPly].posKey = pos->posKey;

    if (pos->enPas != 64) HASH_EP;

    pos->history[pos->hisPly].move = NOMOVE;
    pos->history[pos->hisPly].fiftyMove = pos->fiftyMove;
    pos->history[pos->hisPly].enPas = pos->enPas;
    pos->history[pos->hisPly].castlePerm = pos->castlePerm;
    pos->enPas = 64;

    pos->side ^= 1;
    pos->hisPly++;
    HASH_SIDE;

    return;
}

void TakeNullMove(S_BOARD* pos) {

    pos->hisPly--;
    pos->ply--;

    if (pos->enPas != 64) HASH_EP;

    pos->castlePerm = pos->history[pos->hisPly].castlePerm;
    pos->fiftyMove = pos->history[pos->hisPly].fiftyMove;
    pos->enPas = pos->history[pos->hisPly].enPas;

    if (pos->enPas != 64) HASH_EP;
    pos->side ^= 1;
    HASH_SIDE;
}

// attack

// is square attacked by side? - needed in particular to check if king is in check or squares
// in between king and rook to castle with are attacked by any piece of opposite color.
int SqAttacked(int sq, int side, S_BOARD* pos) {

    if(side == WHITE)
    {
        return AttackedByWhite(pos, sq);
    }
    else
    {
        return AttackedByBlack(pos, sq);
    }
}

// movegen

#define MOVE(f,t,ca,pro,fl) ( (f) | ((t) << 7) | ( (ca) << 14 ) | ( (pro) << 20 ) | (fl))

void GenerateAllMoves(S_BOARD* pos, S_MOVELIST* list);

//checks if a given move is possible on the board or not
int MoveExists(S_BOARD* pos, const int move) {

    S_MOVELIST list[1];
    GenerateAllMoves(pos, list);

    int MoveNum = 0;
    for (MoveNum = 0; MoveNum < list->count; ++MoveNum) {

        if (!MakeMove(pos, list->moves[MoveNum].move)) {
            continue;
        }
        TakeMove(pos);
        if (list->moves[MoveNum].move == move) {
            return TRUE;
        }
    }
    return FALSE;
}

static void AddQuietMove(S_BOARD* pos, int move, S_MOVELIST* list) {

    list->moves[list->count].move = move;

    if (pos->searchKillers[0][pos->ply] == move) {
        list->moves[list->count].score = 900000;
    }
    else if (pos->searchKillers[1][pos->ply] == move) {
        list->moves[list->count].score = 800000;
    }
    else {
        list->moves[list->count].score = pos->searchHistory[pos->pieces[FROMSQ(move)]][TOSQ(move)];
    }
    list->count++;
}

static void AddCaptureMove(S_BOARD* pos, int move, S_MOVELIST* list) {

    list->moves[list->count].move = move;
    list->moves[list->count].score = MvvLvaScores[CAPTURED(move)][pos->pieces[FROMSQ(move)]] + 1000000;
    list->count++;
}

static void AddEnPassantMove(S_BOARD* pos, int move, S_MOVELIST* list) {

    list->moves[list->count].move = move;
    list->moves[list->count].score = 105 + 1000000;
    list->count++;
}

static void AddWhitePawnCapMove(S_BOARD* pos, const int from, const int to, const int cap, S_MOVELIST* list) {

    if (RanksBrd[from] == RANK_7) {
        AddCaptureMove(pos, MOVE(from, to, cap, Q, 0), list);
        AddCaptureMove(pos, MOVE(from, to, cap, R, 0), list);
        AddCaptureMove(pos, MOVE(from, to, cap, B, 0), list);
        AddCaptureMove(pos, MOVE(from, to, cap, N, 0), list);
    }
    else {
        AddCaptureMove(pos, MOVE(from, to, cap, e, 0), list);
    }
}

static void AddWhitePawnMove(S_BOARD* pos, const int from, const int to, S_MOVELIST* list) {

    if (RanksBrd[from] == RANK_7) {
        AddQuietMove(pos, MOVE(from, to, e, Q, 0), list);
        AddQuietMove(pos, MOVE(from, to, e, R, 0), list);
        AddQuietMove(pos, MOVE(from, to, e, B, 0), list);
        AddQuietMove(pos, MOVE(from, to, e, N, 0), list);
    }
    else {
        AddQuietMove(pos, MOVE(from, to, e, e, 0), list);
    }
}

static void AddBlackPawnCapMove(S_BOARD* pos, const int from, const int to, const int cap, S_MOVELIST* list) {

    if (RanksBrd[from] == RANK_2) {
        AddCaptureMove(pos, MOVE(from, to, cap, q, 0), list);
        AddCaptureMove(pos, MOVE(from, to, cap, r, 0), list);
        AddCaptureMove(pos, MOVE(from, to, cap, b, 0), list);
        AddCaptureMove(pos, MOVE(from, to, cap, n, 0), list);
    }
    else {
        AddCaptureMove(pos, MOVE(from, to, cap, e, 0), list);
    }
}

static void AddBlackPawnMove(S_BOARD* pos, const int from, const int to, S_MOVELIST* list) {

    if (RanksBrd[from] == RANK_2) {
        AddQuietMove(pos, MOVE(from, to, e, q, 0), list);
        AddQuietMove(pos, MOVE(from, to, e, r, 0), list);
        AddQuietMove(pos, MOVE(from, to, e, b, 0), list);
        AddQuietMove(pos, MOVE(from, to, e, n, 0), list);
    }
    else {
        AddQuietMove(pos, MOVE(from, to, e, e, 0), list);
    }
}

static void GenerateMajorPieceMoves(S_BOARD* pos, int piece, int color, S_MOVELIST* list) {
    U64 PieceBoard = pos->PieceBB[piece];
    
    while (PieceBoard) {
        int from = PopBit(&PieceBoard);
        U64 AttackBoard;

        if (color == WHITE)
        {
            switch (piece) {
            case N: AttackBoard = KnightAttacks[from]; break;
            case B: AttackBoard = BishopAttacks(pos, from); break;
            case R: AttackBoard = RookAttacks(pos, from); break;
            case Q: AttackBoard = BishopAttacks(pos, from) | RookAttacks(pos, from); break;
            }
        }
        else
        {
            switch (piece) {
            case n: AttackBoard = KnightAttacks[from]; break;
            case b: AttackBoard = BishopAttacks(pos, from); break;
            case r: AttackBoard = RookAttacks(pos, from); break;
            case q: AttackBoard = BishopAttacks(pos, from) | RookAttacks(pos, from); break;
            }
        }

        int to;
        U64 NonattackingMoves = AttackBoard & ~(pos->ColorBB[BOTH]);
        while (NonattackingMoves) {
            to = PopBit(&NonattackingMoves);
            AddQuietMove(pos, MOVE(from, to, 0, 0, 0), list);
        }

        U64 AttackingMoves;
        if (color == WHITE)
        { 
            AttackingMoves = AttackBoard & pos->ColorBB[BLACK];
        }
        else
        {
            AttackingMoves = AttackBoard & pos->ColorBB[WHITE];
        }
        while (AttackingMoves) {
            int to = PopBit(&AttackingMoves);
            int CapturedPiece = pos->pieces[to];
            AddCaptureMove(pos, MOVE(from, to, CapturedPiece, 0, 0), list);
        }
    }
}

static void GenerateMajorPieceCaptures(S_BOARD* pos, int piece, int color, S_MOVELIST* list) {
    U64 PieceBoard = pos->PieceBB[piece], AttackBoard, AttackingMoves;
    int from, to, CapturedPiece;
    while (PieceBoard)
    {
        from = PopBit(&PieceBoard);
        if (color == WHITE)
        {
            switch (piece) {
            case N: AttackBoard = KnightAttacks[from]; break;
            case B: AttackBoard = BishopAttacks(pos, from); break;
            case R: AttackBoard = RookAttacks(pos, from); break;
            case Q: AttackBoard = BishopAttacks(pos, from) | RookAttacks(pos, from); break;
            }
        }
        else
        {
            switch (piece) {
            case n: AttackBoard = KnightAttacks[from]; break;
            case b: AttackBoard = BishopAttacks(pos, from); break;
            case r: AttackBoard = RookAttacks(pos, from); break;
            case q: AttackBoard = BishopAttacks(pos, from) | RookAttacks(pos, from); break;
            }
        }

        if (color == WHITE)
        {
            AttackingMoves = AttackBoard & pos->ColorBB[BLACK];
        }
        else
        {
            AttackingMoves = AttackBoard & pos->ColorBB[WHITE];
        }
        while (AttackingMoves) {
            to = PopBit(&AttackingMoves);
            int CapturedPiece = pos->pieces[to];
            AddCaptureMove(pos, MOVE(from, to, CapturedPiece, 0, 0), list);
        }
    }
}

#define FileOf(sq) ((sq) % (8))
//remainder on dividing
#define RankOf(sq) ((sq) / (8))

#define bbColor(color) (pos -> ColorBB[color])
#define bbPiece(piece) (pos -> PieceBB[piece])
#define PieceOnSquare(sq) (pos->pieces[sq])
#define Side() (pos->side)

int GetWhitePieceAt(S_BOARD* pos, U64 targetSquare) {
    for (int piece = P; piece <= K; piece++) {
        if ((pos->PieceBB[piece] & targetSquare) != 0ULL) return piece;
    }
    return -1;
}

int GetBlackPieceAt(S_BOARD* pos, U64 targetSquare) {
    for (int piece = p; piece <= k; piece++) {
        if ((pos->PieceBB[piece] & targetSquare) != 0ULL) return piece;
    }
    return -1;
}

static void GenerateWhitePawnMoves(S_BOARD* pos, S_MOVELIST* list) {
    U64 advance1 = (pos->PieceBB[P] << 8) & ~(pos->ColorBB[2]);
    U64 advance2 = ((advance1 & RANK_3_MASK) << 8) & ~(pos->ColorBB[2]);
    int to, from, captured; U64 leftAttacks, rightAttacks;
    while (advance1 & NOT_RANK_8_MASK) {
        to = peekBit(advance1 & NOT_RANK_8_MASK); PopBit(&advance1);
        AddWhitePawnMove(pos, to - 8, to, list);

    }
    while (advance1) {
        to = PopBit(&advance1);
        from = to - 8;
        AddWhitePawnMove(pos, from, to, list);
    }
    while (advance2) {
        to = PopBit(&advance2);
        AddQuietMove(pos, MOVE(to - 16, to, e, e, MFLAGPS), list);
    }

    leftAttacks = (pos->PieceBB[P] << 7) & NOT_FILE_H_MASK & (pos->ColorBB[BLACK] | SetMask[pos->enPas]);
    while (leftAttacks & NOT_RANK_8_MASK) {
        to = peekBit(leftAttacks & NOT_RANK_8_MASK); PopBit(&leftAttacks);
        from = to - 7;
        if (to == pos->enPas) {
            AddEnPassantMove(pos, MOVE(from, to, e, e, MFLAGEP), list);
        }
        else {
            captured = GetBlackPieceAt(pos, 1ULL << to);
            AddWhitePawnCapMove(pos, from, to, captured, list);
        }
    }
    while (leftAttacks) {
        to = PopBit(&leftAttacks);
        from = to - 7;
        captured = GetBlackPieceAt(pos, 1ULL << to);
        AddWhitePawnCapMove(pos, from, to, captured, list);

    }

    rightAttacks = (pos->PieceBB[P] << 9) & NOT_FILE_A_MASK & (pos->ColorBB[BLACK] | SetMask[pos->enPas]);
    while (rightAttacks & NOT_RANK_8_MASK) {
        to = peekBit(rightAttacks & NOT_RANK_8_MASK); PopBit(&rightAttacks);
        from = to - 9;
        if (to == pos->enPas) {
            AddEnPassantMove(pos, MOVE(from, to, e, e, MFLAGEP), list);
        }
        else {
            captured = GetBlackPieceAt(pos, 1ULL << to);
            AddWhitePawnCapMove(pos, from, to, captured, list);
        }
    }
    while (rightAttacks) {
        to = PopBit(&rightAttacks);
        from = to - 9;
        captured = GetBlackPieceAt(pos, 1ULL << to);
        AddWhitePawnCapMove(pos, from, to, captured, list);
    }
}
static void GenerateWhitePawnCaptures(S_BOARD* pos, S_MOVELIST* list) {
    int to, from, captured;
    U64 leftAttacks = (pos->PieceBB[P] << 7) & NOT_FILE_H_MASK & (pos->ColorBB[BLACK] | SetMask[pos->enPas]);
    while (leftAttacks & NOT_RANK_8_MASK) {
        to = peekBit(leftAttacks & NOT_RANK_8_MASK); PopBit(&leftAttacks);
        from = to - 7;
        if (to == pos->enPas) {
            AddEnPassantMove(pos, MOVE(from, to, e, e, MFLAGEP), list);
        }
        else {
            captured = GetBlackPieceAt(pos, 1ULL << to);
            AddWhitePawnCapMove(pos, from, to, captured, list);
        }
    }
    while (leftAttacks) {
        to = peekBit(leftAttacks); PopBit(&leftAttacks);
        from = to - 7;
        captured = GetBlackPieceAt(pos, 1ULL << to);
        AddWhitePawnCapMove(pos, from, to, captured, list);
    }
    U64 rightAttacks = (pos->PieceBB[P] << 9) & NOT_FILE_A_MASK & (pos->ColorBB[BLACK] | SetMask[pos->enPas]);
    while (rightAttacks & NOT_RANK_8_MASK) {
        to = peekBit(rightAttacks & NOT_RANK_8_MASK); PopBit(&rightAttacks);
        from = to - 9;
        if (to == pos->enPas) {
            AddEnPassantMove(pos, MOVE(from, to, e, e, MFLAGEP), list);
        }
        else {
            captured = GetBlackPieceAt(pos, 1ULL << to);
            AddWhitePawnCapMove(pos, from, to, captured, list);
        }
    }
    while (rightAttacks) {
        to = PopBit(&rightAttacks);
        from = to - 9;
        captured = GetBlackPieceAt(pos, 1ULL << to);
        AddWhitePawnCapMove(pos, from, to, captured, list);
    }
}

static void GenerateBlackPawnMoves(S_BOARD* pos, S_MOVELIST* list) {
    U64 advance1 = (pos->PieceBB[p] >> 8) & ~(pos->ColorBB[2]);
    U64 advance2 = ((advance1 & RANK_6_MASK) >> 8) & ~(pos->ColorBB[2]);
    int to, from, captured; U64 leftAttacks, rightAttacks;
    while (advance1 & RANK_1_MASK) {
        to = peekBit(advance1 & RANK_1_MASK); PopBit(&advance1);
        AddBlackPawnMove(pos, to + 8, to, list);

    }
    while (advance1) {
        to = PopBit(&advance1);
        from = to + 8;
        AddBlackPawnMove(pos, from, to, list);
    }
    while (advance2) {
        to = PopBit(&advance2);
        AddQuietMove(pos, MOVE(to + 16, to, e, e, MFLAGPS), list);
    }

    leftAttacks = (pos->PieceBB[p] >> 7)& NOT_FILE_A_MASK & (pos->ColorBB[WHITE] | SetMask[pos->enPas]);
    while (leftAttacks & RANK_1_MASK) {
        to = peekBit(leftAttacks & RANK_1_MASK); PopBit(&leftAttacks);
        from = to + 7;
        captured = GetWhitePieceAt(pos, 1ULL << to);
        AddBlackPawnCapMove(pos, from, to, captured, list);
    }

    
    while (leftAttacks) {
        to = PopBit(&leftAttacks);
        from = to + 7;
        if (to == pos->enPas) {
            AddEnPassantMove(pos, MOVE(from, to, e, e, MFLAGEP), list);
        }
        else {
            captured = GetWhitePieceAt(pos, 1ULL << to);
            AddBlackPawnCapMove(pos, from, to, captured, list);
        }
    }
    
    rightAttacks = (pos->PieceBB[p] >> 9) & NOT_FILE_H_MASK& (pos->ColorBB[WHITE] | SetMask[pos->enPas]);
    while (rightAttacks & RANK_1_MASK) {
        to = peekBit(rightAttacks & RANK_1_MASK); PopBit(&rightAttacks);
        from = to + 9;
        captured = GetWhitePieceAt(pos, 1ULL << to);
        AddBlackPawnCapMove(pos, from, to, captured, list);
    }
    while (rightAttacks) {
        to = PopBit(&rightAttacks);
        from = to + 9;
        if (to == pos->enPas) {
            AddEnPassantMove(pos, MOVE(from, to, e, e, MFLAGEP), list);
        }
        else {
            captured = GetWhitePieceAt(pos, 1ULL << to);
            AddBlackPawnCapMove(pos, from, to, captured, list);
        }
    }
}

static void GenerateBlackPawnCaptures(S_BOARD* pos, S_MOVELIST* list) {
    int to, from, captured; U64 leftAttacks, rightAttacks;
    leftAttacks = (pos->PieceBB[p] >> 7) & NOT_FILE_A_MASK & (pos->ColorBB[WHITE] | SetMask[pos->enPas]);
    while (leftAttacks & RANK_1_MASK) {
        to = peekBit(leftAttacks & RANK_1_MASK); PopBit(&leftAttacks);
        from = to + 7;
        captured = GetWhitePieceAt(pos, 1ULL << to);
        AddBlackPawnCapMove(pos, from, to, captured, list);
    }

    while (leftAttacks) {
        to = PopBit(&leftAttacks);
        from = to + 7;
        if (to == pos->enPas) {
            AddEnPassantMove(pos, MOVE(from, to, e, e, MFLAGEP), list);
        }
        else {
            captured = GetWhitePieceAt(pos, 1ULL << to);
            AddBlackPawnCapMove(pos, from, to, captured, list);
        }
    }

    rightAttacks = (pos->PieceBB[p] >> 9)& NOT_FILE_H_MASK & (pos->ColorBB[WHITE] | SetMask[pos->enPas]);
    while (rightAttacks & RANK_1_MASK) {
        to = peekBit(rightAttacks & RANK_1_MASK); PopBit(&rightAttacks);
        from = to + 9;
        captured = GetWhitePieceAt(pos, 1ULL << to);
        AddBlackPawnCapMove(pos, from, to, captured, list);
    }
    while (rightAttacks) {
        to = PopBit(&rightAttacks);
        from = to + 9;
        if (to == pos->enPas) {
            AddEnPassantMove(pos, MOVE(from, to, e, e, MFLAGEP), list);
        }
        else {
            captured = GetWhitePieceAt(pos, 1ULL << to);
            AddBlackPawnCapMove(pos, from, to, captured, list);
        }
    }
}

static void GenerateKingMoves(S_BOARD* pos, int color, S_MOVELIST* list) {
    int from, to, captured;
    U64 moves, attacks;

    if (color == WHITE)
    {
       from = Lsb(pos->PieceBB[K]);
    }
    else
    {
        from = Lsb(pos->PieceBB[k]);
    }

    moves = KingAttacks[from] & ~(pos->ColorBB[2]);
    while (moves) {
        to = PopBit(&moves);
        AddQuietMove(pos, MOVE(from, to, e, e, 0), list);
    }

    if (color == WHITE)
    {
        attacks = KingAttacks[from] & pos->ColorBB[BLACK];
    }
    else
    {
        attacks = KingAttacks[from] & pos->ColorBB[WHITE];
    }
    
    while (attacks) {
        to = PopBit(&attacks);
        if (color == WHITE)
        {
            captured = GetBlackPieceAt(pos, 1ULL << to);
            AddCaptureMove(pos, MOVE(from, to, captured, e, 0), list);
        }
        else
        {
            captured = GetWhitePieceAt(pos, 1ULL << to);
            AddCaptureMove(pos, MOVE(from, to, captured, e, 0), list);
        }
        
    }
    if (color == WHITE)
    {
        if ((pos->castlePerm & WKCA) && ((~(pos->ColorBB[2]) & 0x0000000000000060) == 0x0000000000000060)) {
            if (!AttackedByBlack(pos, e1) && !AttackedByBlack(pos, f1)) {
                AddQuietMove(pos, MOVE(e1, g1, e, e, MFLAGCA), list);
            }
        }
        if ((pos->castlePerm & WQCA) && ((~(pos->ColorBB[2]) & 0x000000000000000E) == 0x000000000000000E)) {
            if (!AttackedByBlack(pos, e1) && !AttackedByBlack(pos, d1)) {
                AddQuietMove(pos, MOVE(e1, c1, e, e, MFLAGCA), list);
            }
        }
    }
    else
    {
        if ((pos->castlePerm & BKCA) && ((~(pos->ColorBB[2]) & 0x6000000000000000) == 0x6000000000000000)) {
            if (!AttackedByWhite(pos, e8) && !AttackedByWhite(pos, f8)) {
                AddQuietMove(pos, MOVE(e8, g8, e, e, MFLAGCA), list);
            }
        }
        if ((pos->castlePerm & BQCA) && ((~(pos->ColorBB[2]) & 0x0E00000000000000) == 0x0E00000000000000)) {
            if (!AttackedByWhite(pos, e8) && !AttackedByWhite(pos, d8)) {
                AddQuietMove(pos, MOVE(e8, c8, e, e, MFLAGCA), list);
            }
        }
    }
}

static void GenerateKingCaptures(S_BOARD* pos, int color, S_MOVELIST* list) {
    int from, to, captured;
    U64 moves, attacks;

    if (color == WHITE)
    {
        from = Lsb(pos->PieceBB[K]);
    }
    else
    {
        from = Lsb(pos->PieceBB[k]);
    }

    if (color == WHITE)
    {
        attacks = KingAttacks[from] & pos->ColorBB[BLACK];
    }
    else
    {
        attacks = KingAttacks[from] & pos->ColorBB[WHITE];
    }

    while (attacks) {
        to = PopBit(&attacks);
        if (color == WHITE)
        {
            captured = GetBlackPieceAt(pos, 1ULL << to);
            AddCaptureMove(pos, MOVE(from, to, captured, e, 0), list);
        }
        else
        {
            captured = GetWhitePieceAt(pos, 1ULL << to);
            AddCaptureMove(pos, MOVE(from, to, captured, e, 0), list);
        }

    }
}

void GenerateAllMoves(S_BOARD* pos, S_MOVELIST* list) {
    list->count = 0;
    if (pos->side == WHITE) {
        GenerateWhitePawnMoves(pos, list);
        GenerateKingMoves(pos, WHITE, list);
        GenerateMajorPieceMoves(pos, N, WHITE, list);
        GenerateMajorPieceMoves(pos, B, WHITE, list);
        GenerateMajorPieceMoves(pos, R, WHITE, list);
        GenerateMajorPieceMoves(pos, Q, WHITE, list);
    }
    else {
        GenerateBlackPawnMoves(pos, list);
        GenerateKingMoves(pos, BLACK, list);
        GenerateMajorPieceMoves(pos, n, BLACK, list);
        GenerateMajorPieceMoves(pos, b, BLACK, list);
        GenerateMajorPieceMoves(pos, r, BLACK, list);
        GenerateMajorPieceMoves(pos, q, BLACK, list);
    }
}

//Mostly the same function as GenerateAllMoves, but it only generates the capture moves which are required in the Quiescence search function

void GenerateAllCaps(S_BOARD* pos, S_MOVELIST* list) {
    list->count = 0;
    if (pos->side == WHITE) {
        GenerateWhitePawnCaptures(pos, list);
        GenerateKingCaptures(pos, WHITE, list);
        GenerateMajorPieceCaptures(pos, N, WHITE, list);
        GenerateMajorPieceCaptures(pos, B, WHITE, list);
        GenerateMajorPieceCaptures(pos, R, WHITE, list);
        GenerateMajorPieceCaptures(pos, Q, WHITE, list);
    }
    else {
        GenerateBlackPawnCaptures(pos, list);
        GenerateKingCaptures(pos, BLACK, list);
        GenerateMajorPieceCaptures(pos, n, BLACK, list);
        GenerateMajorPieceCaptures(pos, b, BLACK, list);
        GenerateMajorPieceCaptures(pos, r, BLACK, list);
        GenerateMajorPieceCaptures(pos, q, BLACK, list);
    }
}

// board

void ResetBoard(S_BOARD* pos);
U64 GeneratePosKey(const S_BOARD* pos);
void UpdateListsMaterial(S_BOARD* pos);

//This function takes in a FEN notation character array, and a pointer to S_BOARD structure

int ParseFen(char* fen, S_BOARD* pos) {

    int  rank = RANK_8;
    int  file = FILE_A;
    int  piece = 0;
    int  count = 0;
    int  i = 0;
    int  sq = 0;

    ResetBoard(pos);

    while ((rank >= RANK_1) && *fen) {
        count = 1;
        switch (*fen) {
        case 'p': piece = p; break;
        case 'r': piece = r; break;
        case 'n': piece = n; break;
        case 'b': piece = b; break;
        case 'k': piece = k; break;
        case 'q': piece = q; break;
        case 'P': piece = P; break;
        case 'R': piece = R; break;
        case 'N': piece = N; break;
        case 'B': piece = B; break;
        case 'K': piece = K; break;
        case 'Q': piece = Q; break;

        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
            piece = e;
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
            return -1;
        }

        for (i = 0; i < count; i++) {
            sq = rank * 8 + file;
            if (piece != e) {
                pos->pieces[sq] = piece;
            }
            file++;
        }
        fen++;
    }

    pos->side = (*fen == 'w') ? WHITE : BLACK;
    fen += 2;

    for (i = 0; i < 4; i++) {
        if (*fen == ' ') {
            break;
        }
        switch (*fen) {
        case 'K': pos->castlePerm |= WKCA; break;
        case 'Q': pos->castlePerm |= WQCA; break;
        case 'k': pos->castlePerm |= BKCA; break;
        case 'q': pos->castlePerm |= BQCA; break;
        default:	     break;
        }
        fen++;
    }
    fen++;

    if (*fen != '-') {
        file = fen[0] - 'a';
        rank = fen[1] - '1';

        pos->enPas = FR2SQ(file, rank);
    }

    pos->posKey = GeneratePosKey(pos);

    UpdateListsMaterial(pos);

    return 0;
}

void UpdateListsMaterial(S_BOARD* pos)
{
    int i = 0, piece;
    while (i < 64)
    {
        piece = pos->pieces[i];
        if (piece == p)
        {
            SETBIT(pos->PieceBB[p], i);
            SETBIT(pos->ColorBB[BOTH], i);
            SETBIT(pos->ColorBB[1], i);
        }
        else if(piece == r)
        {
           SETBIT(pos->PieceBB[r], i);
           SETBIT(pos->ColorBB[BOTH], i);
           SETBIT(pos->ColorBB[1], i);
           pos->bigPce[BLACK]++;
        }
        else if (piece == n)
        {
            SETBIT(pos->PieceBB[n], i);
            SETBIT(pos->ColorBB[BOTH], i);
            SETBIT(pos->ColorBB[1], i);
            pos->bigPce[BLACK]++;
        }
        else if (piece == b)
        {
            SETBIT(pos->PieceBB[b], i);
            SETBIT(pos->ColorBB[BOTH], i);
            SETBIT(pos->ColorBB[1], i);
            pos->bigPce[BLACK]++;
        }
        else if (piece == q)
        {
            SETBIT(pos->PieceBB[q], i);
            SETBIT(pos->ColorBB[BOTH], i);
            SETBIT(pos->ColorBB[1], i);
            pos->bigPce[BLACK]++;
        }
        else if (piece == k)
        {
            SETBIT(pos->PieceBB[k], i);
            SETBIT(pos->ColorBB[BOTH], i);
            SETBIT(pos->ColorBB[1], i);
            pos->bigPce[BLACK]++;
        }
        else if (piece == P)
        {
            SETBIT(pos->PieceBB[P], i);
            SETBIT(pos->ColorBB[BOTH], i);
            SETBIT(pos->ColorBB[0], i);
        }
        else if (piece == N)
        {
            SETBIT(pos->PieceBB[N], i);
            SETBIT(pos->ColorBB[BOTH], i);
            SETBIT(pos->ColorBB[0], i);
            pos->bigPce[WHITE]++;
        }
        else if (piece == B)
        {
            SETBIT(pos->PieceBB[B], i);
            SETBIT(pos->ColorBB[BOTH], i);
            SETBIT(pos->ColorBB[0], i);
            pos->bigPce[WHITE]++;
        }
        else if (piece == R)
        {
            SETBIT(pos->PieceBB[R], i);
            SETBIT(pos->ColorBB[BOTH], i);
            SETBIT(pos->ColorBB[0], i);
            pos->bigPce[WHITE]++;
        }
        else if (piece == Q)
        {
            SETBIT(pos->PieceBB[Q], i);
            SETBIT(pos->ColorBB[BOTH], i);
            SETBIT(pos->ColorBB[0], i);
            pos->bigPce[WHITE]++;
        }
        else if (piece == K)
        {
            SETBIT(pos->PieceBB[K], i);
            SETBIT(pos->ColorBB[BOTH], i);
            SETBIT(pos->ColorBB[0], i);
            pos->bigPce[WHITE]++;
        }
        i++;
    }
}

void ResetBoard(S_BOARD* pos) {

    int index = 0;

    for (index = 0; index < 64; ++index) {
        pos->pieces[index] = e;
    }

    //start with empty piece and color bitboards
    for (int i = 0; i < 13; i++)
    {
        pos->PieceBB[i] = 0ULL;
    }

    pos->ColorBB[0] = 0ULL;
    pos->ColorBB[1] = 0ULL;
    pos->ColorBB[2] = 0ULL;

    pos->bigPce[0] = 0;
    pos->bigPce[1] = 0;

    pos->side = BOTH;
    pos->enPas = no_sq;
    pos->fiftyMove = 0;

    pos->ply = 0;
    pos->hisPly = 0;

    pos->castlePerm = 0;

    pos->posKey = 0ULL;

}

//Prints the board when it has been set up by in FEN notation
void PrintBoard(const S_BOARD* pos) {

    int sq, file, rank, piece;

    printf("\nGame Board:\n\n");

    for (rank = RANK_8; rank >= RANK_1; rank--) {
        printf("%d  ", rank + 1);
        for (file = FILE_A; file <= FILE_H; file++) {
            sq = FR2SQ(file, rank);
            piece = pos->pieces[sq];
            printf("%3c", PceChar[piece]);
        }
        printf("\n");
    }

    printf("\n   ");
    for (file = FILE_A; file <= FILE_H; file++) {
        printf("%3c", 'a' + file);
    }
    printf("\n");
    printf("side:%c\n", SideChar[pos->side]);
    printf("enPas:%d\n", pos->enPas);
    printf("castle:%c%c%c%c\n",
        pos->castlePerm & WKCA ? 'K' : '-',
        pos->castlePerm & WQCA ? 'Q' : '-',
        pos->castlePerm & BKCA ? 'k' : '-',
        pos->castlePerm & BQCA ? 'q' : '-'
    );
    printf("PosKey:%llX\n", pos->posKey);
}

// io

//Print the file and rank of a current square using the file and rank arrays
char* PrSq(const int sq) {

    static char SqStr[3];

    int file = FilesBrd[sq];
    int rank = RanksBrd[sq];



    sprintf(SqStr, "%c%c", ('a' + file), ('1' + rank));

    return SqStr;

}

//Print the from, to and promoted bit values of the move
char* PrMove(const int move) {

    static char MvStr[6];

    int ff = FilesBrd[FROMSQ(move)];
    int rf = RanksBrd[FROMSQ(move)];
    int ft = FilesBrd[TOSQ(move)];
    int rt = RanksBrd[TOSQ(move)];

    int promoted = PROMOTED(move);

    if (promoted) {
        char pchar = 'q';
        if (IsKn(promoted)) {
            pchar = 'n';
        }
        else if (IsRQ(promoted) && !IsBQ(promoted)) {
            pchar = 'r';
        }
        else if (!IsRQ(promoted) && IsBQ(promoted)) {
            pchar = 'b';
        }
        sprintf(MvStr, "%c%c%c%c%c", ('a' + ff), ('1' + rf), ('a' + ft), ('1' + rt), pchar);
    }
    else {
        sprintf(MvStr, "%c%c%c%c", ('a' + ff), ('1' + rf), ('a' + ft), ('1' + rt));
    }

    return MvStr;
}

int ParseMove(char* ptrChar, S_BOARD* pos) {

    if (ptrChar[1] > '8' || ptrChar[1] < '1') return NOMOVE;
    if (ptrChar[3] > '8' || ptrChar[3] < '1') return NOMOVE;
    if (ptrChar[0] > 'h' || ptrChar[0] < 'a') return NOMOVE;
    if (ptrChar[2] > 'h' || ptrChar[2] < 'a') return NOMOVE;

    int from = FR2SQ(ptrChar[0] - 'a', ptrChar[1] - '1');
    int to = FR2SQ(ptrChar[2] - 'a', ptrChar[3] - '1');

    S_MOVELIST list[1];
    GenerateAllMoves(pos, list);
    int MoveNum = 0;
    int Move = 0;
    int PromPce = e;

    for (MoveNum = 0; MoveNum < list->count; ++MoveNum) {
        Move = list->moves[MoveNum].move;
        if (FROMSQ(Move) == from && TOSQ(Move) == to) {
            PromPce = PROMOTED(Move);
            if (PromPce != e) {
                if (IsRQ(PromPce) && !IsBQ(PromPce) && ptrChar[4] == 'r') {
                    return Move;
                }
                else if (!IsRQ(PromPce) && IsBQ(PromPce) && ptrChar[4] == 'b') {
                    return Move;
                }
                else if (IsRQ(PromPce) && IsBQ(PromPce) && ptrChar[4] == 'q') {
                    return Move;
                }
                else if (IsKn(PromPce) && ptrChar[4] == 'n') {
                    return Move;
                }
                continue;
            }
            return Move;
        }
    }

    return NOMOVE;
}

void PrintMoveList(const S_MOVELIST* list) {
    int index = 0;
    int score = 0;
    int move = 0;
    printf("MoveList:\n", list->count);

    for (index = 0; index < list->count; ++index) {

        move = list->moves[index].move;
        score = list->moves[index].score;

        printf("Move:%d > %s (score:%d)\n", index + 1, PrMove(move), score);
    }
    printf("MoveList Total %d Moves:\n\n", list->count);
}

// pvtable

int ProbePvMove(const S_BOARD* pos);

//Get the best pvline stored in the PvArray for a given depth but we may also get a result greater than the depth
int GetPvLine(const int depth, S_BOARD* pos) {

    int move = ProbePvMove(pos);
    int count = 0;

    while (move != NOMOVE && count < depth) {

        if (MoveExists(pos, move)) {
            MakeMove(pos, move);
            pos->PvArray[count++] = move;
        }
        else {
            break;
        }
        move = ProbePvMove(pos);
    }

    while (pos->ply > 0) {
        TakeMove(pos);
    }

    return count;

}

void ClearHashTable(S_HASHTABLE* table) {

    S_HASHENTRY* tableEntry;

    for (tableEntry = table->pTable; tableEntry < table->pTable + table->numEntries; tableEntry++) {
        tableEntry->posKey = 0ULL;
        tableEntry->move = NOMOVE;
        tableEntry->depth = 0;
        tableEntry->score = 0;
        tableEntry->flags = 0;
    }
    table->newWrite = 0;
}

void InitHashTable(S_HASHTABLE* table, const int MB) {

    int HashSize = 0x100000 * MB;
    table->numEntries = HashSize / sizeof(S_HASHENTRY);
    table->numEntries -= 2;

    if (table->pTable != NULL) {
        free(table->pTable);
    }

    table->pTable = (S_HASHENTRY*)malloc(table->numEntries * sizeof(S_HASHENTRY));
    if (table->pTable == NULL) {
        printf("Hash Allocation Failed, trying %dMB...\n", MB / 2);
        InitHashTable(table, MB / 2);
    }
    else {
        ClearHashTable(table);
        printf("HashTable init complete with %d entries\n", table->numEntries);
    }

}

int ProbeHashEntry(S_BOARD* pos, int* move, int* score, int alpha, int beta, int depth) {

    int index = pos->posKey % pos->HashTable->numEntries;

    if (pos->HashTable->pTable[index].posKey == pos->posKey) {
        *move = pos->HashTable->pTable[index].move;
        if (pos->HashTable->pTable[index].depth >= depth) {
            pos->HashTable->hit++;

            *score = pos->HashTable->pTable[index].score;
            if (*score > ISMATE) *score -= pos->ply;
            else if (*score < -ISMATE) *score += pos->ply;

            switch (pos->HashTable->pTable[index].flags) {

            case HFALPHA: if (*score <= alpha) {
                *score = alpha;
                return TRUE;
            }
                        break;
            case HFBETA: if (*score >= beta) {
                *score = beta;
                return TRUE;
            }
                       break;
            case HFEXACT:
                return TRUE;
                break;
            //default: ASSERT(FALSE); break;
            default: break;
            }
        }
    }

    return FALSE;
}

void StoreHashEntry(S_BOARD* pos, const int move, int score, const int flags, const int depth) {

    int index = pos->posKey % pos->HashTable->numEntries;

    if (pos->HashTable->pTable[index].posKey == 0) {
        pos->HashTable->newWrite++;
    }
    else {
        pos->HashTable->overWrite++;
    }

    if (score > ISMATE) score += pos->ply;
    else if (score < -ISMATE) score -= pos->ply;

    pos->HashTable->pTable[index].move = move;
    pos->HashTable->pTable[index].posKey = pos->posKey;
    pos->HashTable->pTable[index].flags = flags;
    pos->HashTable->pTable[index].score = score;
    pos->HashTable->pTable[index].depth = depth;
}

int ProbePvMove(const S_BOARD* pos) {

    int index = pos->posKey % pos->HashTable->numEntries;

    if (pos->HashTable->pTable[index].posKey == pos->posKey) {
        return pos->HashTable->pTable[index].move;
    }

    return NOMOVE;
}

//evaluate

// Piece Square Tables (by Lyudmil)
const int PawnMG[64] =
{
    0,   0,   0,   0,   0,   0,   0,   0,
    -5,  -2,   4,   5,   5,   4,  -2,  -5,
    -4,  -2,   5,   7,   7,   5,  -2,  -4,
    -2,  -1,   9,  13,  13,   9,  -1,  -2,
    2,   4,  13,  21,  21,  13,   4,   2,
    10,  21,  25,  29,  29,  25,  21,  10,
    1,   2,   5,   9,   9,   5,   2,   1,             // Pawns 7 Rank
    0,   0,   0,   0,   0,   0,   0,   0
};

const int PawnEG[64] =
{
    0,   0,   0,   0,   0,   0,   0,   0,
    -3,  -1,   2,   3,   3,   2,  -1,  -3,
    -2,  -1,   3,   4,   4,   3,  -1,  -2,
    -1,   0,   4,   7,   7,   4,   0,  -1,
    1,   2,   7,  11,  11,   7,   2,   1,
    5,  11,  13,  14,  14,  13,  11,   5,
    0,   1,   3,   5,   5,   3,   1,   0,
    0,   0,   0,   0,   0,   0,   0,   0
};

const int KnightMG[64] =
{
    -31, -23, -20, -16, -16, -20, -23, -31,
    -23, -16, -12,  -8,  -8, -12, -16, -23,
    -8,  -4,   0,   8,   8,   0,  -4,  -8,
    -4,   8,  12,  16,  16,  12,   8,  -4,
    8,  16,  20,  23,  23,  20,  16,   8,
    23,  27,  31,  35,  35,  31,  27,  23,
    4,   8,  12,  16,  16,  12,   8,   4,
    4,   4,   4,   4,   4,   4,   4,   4,
};

const int KnightEG[64] =
{
    -39, -27, -23, -20, -20, -23, -27, -39,
    -27, -20, -12,  -8,  -8, -12, -20, -27,
    -8,  -4,   0,   8,   8,   0,  -4,  -8,
    -4,   8,  12,  16,  16,  12,   8,  -4,
    8,  16,  20,  23,  23,  20,  16,   8,
    12,  23,  27,  31,  31,  27,  23,  12,
    -2,   2,   4,   8,   8,   4,   2,  -2,
    -16,  -8,  -4,  -4,  -4,  -4,  -8, -16,
};

const int BishopMG[64] =
{
    -31, -23, -20, -16, -16, -20, -23, -31,
    -23, -16, -12,  -8,  -8, -12, -16, -23,
    -8,  -4,   0,   8,   8,   0,  -4,  -8,
    -4,   8,  12,  16,  16,  12,   8,  -4,
    8,  16,  20,  23,  23,  20,  16,   8,
    23,  27,  31,  35,  35,  31,  27,  23,
    4,   8,  12,  16,  16,  12,   8,   4,
    4,   4,   4,   4,   4,   4,   4,   4,
};

const int BishopEG[64] =
{
    -39, -27, -23, -20, -20, -23, -27, -39,
    -27, -20, -12,  -8,  -8, -12, -20, -27,
    -8,  -4,   0,   8,   8,   0,  -4,  -8,
    -4,   8,  12,  16,  16,  12,   8,  -4,
    8,  16,  20,  23,  23,  20,  16,   8,
    12,  23,  27,  31,  31,  27,  23,  12,
    -2,   2,   4,   8,   8,   4,   2,  -2,
    -16,  -8,  -4,  -4,  -4,  -4,  -8, -16,
};

const int RookMG[64] =
{
    -10,  -8,  -6,  -4,  -4,  -6,  -8, -10,
    -8,  -6,  -4,  -2,  -2,  -4,  -6,  -8,
    -4,  -2,   0,   4,   4,   0,  -2,  -4,
    -2,   2,   4,   8,   8,   4,   2,  -2,
    2,   4,   8,  12,  12,   8,   4,   2,
    4,   8,   12, 16,  16,  12,   8,   4,
    20,  21,   23, 23,  23,  23,  21,  20,
    18,  18,   20, 20,  20,  20,  18,  18,
};

const int RookEG[64] =
{
    -10,  -8,  -6,  -4,  -4,  -6,  -8, -10,
    -8,  -6,  -4,  -2,  -2,  -4,  -6,  -8,
    -4,  -2,   0,   4,   4,   0,  -2,  -4,
    -2,   2,   4,   8,   8,   4,   2,  -2,
    2,   4,   8,  12,  12,   8,   4,   2,
    4,   8,  12,  16,  16,  12,   8,   4,
    20,  21,  23,  23,  23,  23,  21,  20,
    18,  18,  20,  20,  20,  20,  18,  18,
};

const int QueenMG[64] =
{
    -23, -20, -16, -12, -12, -16, -20, -23,
    -18, -14, -12,  -8,  -8, -12, -14, -18,
    -16,  -8,   0,   8,   8,   0,  -8, -16,
    -8,   0,  12,  16,  16,  12,   0,  -8,
    4,  12,  16,  23,  23,  16,  12,   4,
    16,  23,  27,  31,  31,  27,  23,  16,
    4,  12,  16,  23,  23,  16,  12,   4,
    2,   8,  12,  12,  12,  12,   8,   2,
};

const int QueenEG[64] =
{
    -23, -20, -16, -12, -12, -16, -20, -23,
    -18, -14, -12,  -8,  -8, -12, -14, -18,
    -16,  -8,   0,   8,   8,   0,  -8, -16,
    -8,   0,  12,  16,  16,  12,   0,  -8,
    4,  12,  16,  23,  23,  16,  12,   4,
    16,  23,  27,  31,  31,  27,  23,  16,
    4,  12,  16,  23,  23,  16,  12,   4,
    2,   8,  12,  12,  12,  12,   8,   2,
};

const int KingMG[64] =
{
    40,  50,  30,  10,  10,  30,  50,  40,
    30,  40,  20,   0,   0,  20,  40,  30,
    10,  20,   0, -20, -20,   0,  20,  10,
    0,  10, -10, -30, -30, -10,  10,   0,
    -10,   0, -20, -40, -40, -20,   0, -10,
    -20, -10, -30, -50, -50, -30, -10, -20,
    -30, -20, -40, -60, -60, -40, -20, -30,
    -40, -30, -50, -70, -70, -50, -30, -40 ,
};

const int KingEG[64] =
{
    -34, -30, -28, -27, -27, -28, -30, -34,
    -17, -13, -11, -10, -10, -11, -13, -17,
    -2,   2,   4,   5,   5,   4,   2,  -2,
    11,  15,  17,  18,  18,  17,  15,  11,
    22,  26,  28,  29,  29,  28,  26,  22,
    31,  34,  37,  38,  38,  37,  34,  31,
    38,  41,  44,  45,  45,  44,  41,  38,
    42,  46,  48,  50,  50,  48,  46,  42,
};

const int mirror[64] = {
  56,  57,  58,  59,  60,  61,  62,  63,
  48,  49,  50,  51,  52,  53,  54,  55,
  40,  41,  42,  43,  44,  45,  46,  47,
  32,  33,  34,  35,  36,  37,  38,  39,
  24,  25,  26,  27,  28,  29,  30,  31,
  16,  17,  18,  19,  20,  21,  22,  23,
   8,   9,  10,  11,  12,  13,  14,  15,
   0,   1,   2,   3,   4,   5,   6,   7
};

#define MG 0
#define EG 1
#define PhaseNb 2
#define RANK_NB 8

// [PHASE][DEFENDED]
const int KnightOutpostValues[PhaseNb][2] = { {20, 40}, {10, 20} };
const int BishopOutpostValues[PhaseNb][2] = { {15, 30}, { 3,  5} };

// Definition of evaluation terms related to Passed Pawns

const int PassedPawn[2][2][RANK_NB][PhaseNb] = {
  {{{   0,   0}, { -33, -30}, { -24,   8}, { -13,  -2}, {  24,   0}, {  66,  -5}, { 160,  32}, {   0,   0}},
   {{   0,   0}, {  -2,   1}, { -14,  23}, { -15,  35}, {   7,  44}, {  72,  60}, { 194, 129}, {   0,   0}}},
  {{{   0,   0}, {  -7,  12}, { -12,   6}, { -10,  27}, {  27,  32}, {  86,  63}, { 230, 149}, {   0,   0}},
   {{   0,   0}, {  -5,   8}, { -12,  17}, { -21,  52}, { -14, 109}, {  28, 202}, { 119, 369}, {   0,   0}}},
};

const int KnightAttackedByPawn[PhaseNb] = { -34, -30 };

const int KnightBehindPawn[2] = { 4, 18 };

int KnightMobility[PhaseNb][9] = {
    {-30, -25, -10,   0,  10,  18,  26,  34,  42},
    {-30, -25,   0,   9,  15,  21,  28,  35,  36}
};

const int BishopAttackedByPawn[PhaseNb] = { -33, -29 };

int BishopMobility[PhaseNb][14] = {
    {-30, -20, -15,   0, 15,  21,  26,  31,  34,  36,  37,  38,  38,  38},
    {-30, -20, -15,   0, 15,  21,  26,  31,  34,  36,  37,  38,  38,  38},
};

const int BishopBehindPawn[2] = { 3, 13 };

int RookMobility[PhaseNb][15] = {
    {-30, -25, -10,  -5,  -3,  -1,   6,  11,  15,  19,  23,  25,  26,  27, 27},
    {-35, -20, -10,   0,  10,  19,  27,  33,  39,  41,  43,  45,  47,  48, 48}
};

const int QueenChecked[PhaseNb] = { -50, -29 };

const int QueenCheckedByPawn[PhaseNb] = { -60, -37 };

const int QueenMobility[PhaseNb][28] = {
    {-50, -40, -20,   0,   2,   4,   6,
       8,  11,  15,  19,  20,  21,  22,
      24,  24,  24,  24,  24,  24,  24,
      24,  24,  24,  24,  24,  24,  24},

    {-50, -40, -20, -10,   0,   4,   8,
      12,  15,  18,  21,  24,  27,  30,
      35,  43,  43,  43,  43,  43,  43,
      43,  43,  43,  43,  43,  43,  43}
};

const int BishopHasWings[PhaseNb] = { 13, 36 };

const int BishopPair[PhaseNb] = { 46, 64 };

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

#define LEFT_WING  (FILEA | FILEB | FILEC)
#define RIGHT_WING (FILEF | FILEG | FILEH)

#define WHITE_SQUARES 0x55AA55AA55AA55AA
#define BLACK_SQUARES 0xAA55AA55AA55AA55

#define KING_HAS_CASTLED     (25)
#define KING_CAN_CASTLE      (10)

#define ROOK_OPEN_FILE_MID   (35)
#define ROOK_OPEN_FILE_END   (20)
#define ROOK_SEMI_FILE_MID   (12)
#define ROOK_SEMI_FILE_END   (12)
#define ROOK_ON_7TH_MID      (10)
#define ROOK_ON_7TH_END      (15)

#define PAWN_STACKED_MID     (10)
#define PAWN_STACKED_END     (20)
#define PAWN_ISOLATED_MID    (10)
#define PAWN_ISOLATED_END    (20)

const int PawnBackwards[2][2] = { {7, -3}, {-11, -11} };
const int KnightRammedPawns[2] = { 0, 5 };
const int BishopRammedPawns[2] = { -11, -12 };

int KingDefenders[12][2] = {
    {-32, -3}, {-15, 7}, {0, 1}, {9, -1},
    {23, -6}, {34, 3}, {32, 12}, {24, 0},
    {12, 6}, {12, 6}, {12, 6}, {12, 6},
};

#define FILE_NB 8

U64 pawnAdvance(U64 pawns, U64 occupied, int color) {
    return ~occupied & (color == WHITE ? (pawns << 8) : (pawns >> 8));
}

bool testBit(U64 b, int i) {
    return b & (1ULL << i);
}

static inline int fileOf(int square) {
    return square & 7;
}

static inline int rankOf(int square) {
    return square >> 3;
}

#define kingAttacks(sq, tg) (KingMap[(sq)] & (tg))

#define File(sq) ((sq) & 7)
#define Rank(sq) ((sq) >> 3)

int pieceValues[5][2] = {
    {100, 120}, {400, 380}, {420, 400}, {650, 600}, {1350, 1270}
};

int Evalpos(S_BOARD* pos) {

    U64 white = pos->ColorBB[0];
    U64 black = pos->ColorBB[1];
    U64 pawns = (pos->PieceBB[P] | pos->PieceBB[p]);
    U64 knights = (pos->PieceBB[N] | pos->PieceBB[n]);
    U64 bishops = (pos->PieceBB[B] | pos->PieceBB[b]);
    U64 rooks = (pos->PieceBB[R] | pos->PieceBB[r]);
    U64 queens = (pos->PieceBB[Q] | pos->PieceBB[q]);
    U64 kings = (pos->PieceBB[K] | pos->PieceBB[k]);

    U64 myPieces, myPawns, enemyPawns, passedPawns = 0ULL;
    U64 tempPawns, tempKnights, tempBishops, tempRooks, tempQueens;
    U64 attacks, mobilityArea, destination, defenders;

    int mg = 0, eg = 0;
    int eval, curPhase;
    int color, bit, defended, mobilityCount, semi, rank;
    int count, canAdvance, safeAdvance;

    U64 whiteKingBitboard = (pos->PieceBB[K]);
    int wKingSq = PopBit(&whiteKingBitboard);
    U64 blackKingBitboard = (pos->PieceBB[k]);
    int bKingSq = PopBit(&blackKingBitboard);

    mg += KingMG[wKingSq];
    eg += KingEG[wKingSq];

    mg -= KingMG[mirror[bKingSq]];
    eg -= KingEG[mirror[bKingSq]];

    U64 whitePawns = white & pawns;
    U64 blackPawns = black & pawns;
    U64 notEmpty = white | black;

    U64 pawnAttacks[2] = {
        (whitePawns << 9 & ~FILEA) | (whitePawns << 7 & ~FILEH),
        (blackPawns >> 9 & ~FILEH) | (blackPawns >> 7 & ~FILEA)
    };

    U64 blockedPawns[2] = {
        (whitePawns << 8 & black) >> 8,
        (blackPawns >> 8 & white) << 8,
    };

    U64 kingAreas[2] = {
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

    int attackCounts[2] = { 0, 0 };
    int attackerCounts[2] = { 0, 0 };

    for (color = BLACK; color >= WHITE; color--) {

        // Negate the scores so that the scores are from
        // White's perspective after the loop completes
        mg = -mg;
        eg = -eg;

        myPieces = pos->ColorBB[color];
        myPawns = myPieces & pawns;
        enemyPawns = pawns ^ myPawns;

        tempPawns = myPawns;
        tempKnights = myPieces & knights;
        tempBishops = myPieces & bishops;
        tempRooks = myPieces & rooks;
        tempQueens = myPieces & queens;

        // Don't include squares that are attacked by enemy pawns, 
        // occupied by our king, or occupied with our blocked pawns
        // in our mobilityArea. This definition of mobilityArea is
        // derived directly from Stockfish's evaluation features. 
        mobilityArea = ~(
            pawnAttacks[!color] | (myPieces & kings) | blockedPawns[color]
            );

        // Bishop gains a bonus for pawn wings
        if (tempBishops && (myPawns & LEFT_WING) && (myPawns & RIGHT_WING)) {
            mg += BishopHasWings[MG];
            eg += BishopHasWings[EG];
        }

        if ((tempBishops & WHITE_SQUARES) && (tempBishops & BLACK_SQUARES)) {
            mg += BishopPair[MG];
            eg += BishopPair[EG];
        }

        else if (pos->castlePerm & (3 << (2 * color))) {
            mg += KING_CAN_CASTLE;
            eg += KING_CAN_CASTLE;
        }

        if (color == WHITE)
        {
            defenders = (pos->PieceBB[P])
                | (pos->PieceBB[N])
                | (pos->PieceBB[B]);
        }
        else
        {
            defenders = (pos->PieceBB[p])
                | (pos->PieceBB[n])
                | (pos->PieceBB[b]);
        }

        // Bonus for our pawns and minors sitting within our king area
        count = bitCount(defenders & kingAreas[color]);
        mg += KingDefenders[count][MG];
        eg += KingDefenders[count][EG];

        // Get the attack board for the pawns
        attacks = pawnAttacks[color] & kingAreas[!color];
        allAttackBoards[color] |= pawnAttacks[color];
        attackedNoQueen[color] |= attacks;

        // Update the counters for the safety evaluation
        if (attacks) {
            attackCounts[2] += 2 * bitCount(attacks);
            attackerCounts[2] += 1;
        }

        const int forward = (color == WHITE) ? 8 : -8;

        while (tempPawns) {

            int bit = PopBit(&tempPawns);

            if (color == WHITE)
            {
                mg += PawnMG[bit];
                eg += PawnEG[bit];
            }
            else
            {
                mg += PawnMG[mirror[bit]];
                eg += PawnEG[mirror[bit]];
            }

            // Save the fact that this pawn is passed. We will
            // use it later in order to apply a proper bonus
            if (!(PassedPawnMasks[color][bit] & enemyPawns))
                passedPawns |= (1ULL << bit);

            if (!(IsolatedPawnMasks[bit] & tempPawns)) {
                mg -= PAWN_ISOLATED_MID;
                eg -= PAWN_ISOLATED_END;
            }

            if (FILES[File(bit)] & tempPawns) {
                mg -= PAWN_STACKED_MID;
                eg -= PAWN_STACKED_END;
            }

            // Apply a penalty if the pawn is backward
            if (!(PassedPawnMasks[!color][bit] & myPawns)
                && (pawnAttacks[!color] & (1ULL << (bit + forward)))) {
                semi = !(FILES[fileOf(bit)] & enemyPawns);
                mg += PawnBackwards[semi][MG];
                eg += PawnBackwards[semi][EG];
            }

            // Apply a bonus if the pawn is connected
            if (PawnConnectedMasks[color][bit] & myPawns) {
                mg += PawnConnected[color][bit];
                eg += PawnConnected[color][bit];
            }

        }

        while (tempKnights) {

            int bit = PopBit(&tempKnights);

            if (color == WHITE)
            {
                mg += KnightMG[bit];
                eg += KnightEG[bit];
            }
            else
            {
                mg += KnightMG[mirror[bit]];
                eg += KnightEG[mirror[bit]];
            }

            attacks = KnightAttacks[bit];
            allAttackBoards[color] |= attacks;
            attackedNoQueen[color] |= attacks;

            // Apply a penalty if the knight is being attacked by a pawn
            if (pawnAttacks[!color] & (1ULL << bit)) {
                mg += KnightAttackedByPawn[MG];
                eg += KnightAttackedByPawn[EG];
            }

            // Knight is in an outpost square, unable to be
            // attacked by enemy pawns, on or between ranks
            // four through seven, relative to it's colour
            if (OutpostRanks[color] & (1ULL << bit)
                && !(OutpostSquareMasks[color][bit] & enemyPawns)) {

                defended = (pawnAttacks[color] & (1ULL << bit)) != 0ULL;

                mg += KnightOutpostValues[MG][defended];
                eg += KnightOutpostValues[EG][defended];
            }

            // Apply a bonus if the knight is behind a pawn
            if (testBit(pawnAdvance((myPawns | enemyPawns), 0ULL, !color), bit)) {
                mg += KnightBehindPawn[MG];
                eg += KnightBehindPawn[EG];
            }

            // Knight gains a mobility bonus based off of the number
            // of attacked or defended squares within the mobility area
            mobilityCount = bitCount((mobilityArea & attacks));
            mg += KnightMobility[MG][mobilityCount];
            eg += KnightMobility[EG][mobilityCount];

            attacks = attacks & kingAreas[!color];
            if (attacks) {
                attackCounts[color] += 2 * bitCount(attacks);
                attackerCounts[color]++;
            }
        }

        while (tempBishops) {

            int bit = PopBit(&tempBishops);

            if (color == WHITE)
            {
                mg += BishopMG[bit];
                eg += BishopEG[bit];
            }
            else
            {
                mg += BishopMG[mirror[bit]];
                eg += BishopEG[mirror[bit]];
            }

            attacks = BishopAttacks(pos, bit);
            allAttackBoards[color] |= attacks;
            attackedNoQueen[color] |= attacks;

            // Apply a penalty if the bishop is being attacked by a pawn
            if (pawnAttacks[!color] & (1ULL << bit)) {
                mg += BishopAttackedByPawn[MG];
                eg += BishopAttackedByPawn[EG];
            }

            // Apply a penalty for the bishop based on number of rammed pawns
            // of our own colour, which reside on the same shade of square as the bishop
            count = bitCount(rammedPawns[color] & (((1ULL << bit) & WHITE_SQUARES ? WHITE_SQUARES : BLACK_SQUARES)));
            mg += count * BishopRammedPawns[MG];
            eg += count * BishopRammedPawns[EG];

            // Bishop is in an outpost square, unable to be
            // attacked by enemy pawns, on or between ranks
            // four through seven, relative to it's colour
            if (OutpostRanks[color] & (1ULL << bit)
                && !(OutpostSquareMasks[color][bit] & enemyPawns)) {

                defended = (pawnAttacks[color] & (1ULL << bit)) != 0ULL;

                mg += BishopOutpostValues[MG][defended];
                eg += BishopOutpostValues[EG][defended];
            }

            // Apply a bonus if the bishop is behind a pawn
            if (testBit(pawnAdvance((myPawns | enemyPawns), 0ULL, !color), bit)) {
                mg += BishopBehindPawn[MG];
                eg += BishopBehindPawn[EG];
            }

            // Bishop gains a mobility bonus based off of the number
            // of attacked or defended squares within the mobility area
            mobilityCount = bitCount((mobilityArea & attacks));
            mg += BishopMobility[MG][mobilityCount];
            eg += BishopMobility[EG][mobilityCount];

            attacks = attacks & kingAreas[!color];
            if (attacks) {
                attackCounts[color] += 2 * bitCount(attacks);
                attackerCounts[color]++;
            }
        }


        while (tempRooks) {

            int bit = PopBit(&tempRooks);

            if (color == WHITE)
            {
                mg += RookMG[bit];
                eg += RookEG[bit];
            }
            else
            {
                mg += RookMG[mirror[bit]];
                eg += RookEG[mirror[bit]];
            }

            attacks = RookAttacks(pos, bit);
            allAttackBoards[color] |= attacks;
            attackedNoQueen[color] |= attacks;

            // Rook is on a semi-open file if there are no
            // pawns of the Rook's color on the file. If
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
            // on seventh rank relative to its color
            if (Rank(bit) == (color == BLACK ? 1 : 6)) {
                mg += ROOK_ON_7TH_MID;
                eg += ROOK_ON_7TH_END;
            }

            // Rook gains a mobility bonus based off of the number
            // of attacked or defended squares within the mobility area
            mobilityCount = bitCount((mobilityArea & attacks));
            mg += RookMobility[MG][mobilityCount];
            eg += RookMobility[EG][mobilityCount];

            attacks = attacks & kingAreas[!color];
            if (attacks) {
                attackCounts[color] += 3 * bitCount(attacks);
                attackerCounts[color]++;
            }
        }

        while (tempQueens) {

            int bit = PopBit(&tempQueens);

            if (color == WHITE)
            {
                mg += QueenMG[bit];
                eg += QueenEG[bit];
            }
            else
            {
                mg += QueenMG[mirror[bit]];
                eg += QueenEG[mirror[bit]];
            }

            attacks = (RookAttacks(pos, bit) | BishopAttacks(pos, bit));
            allAttackBoards[color] |= attacks;

            // Apply a bonus if the queen is under an attack threat
            if ((1ULL << bit) & attackedNoQueen[!color]) {
                eg += QueenChecked[MG];
                mg += QueenChecked[EG];
            }

            // Apply a penalty if the queen is under attack by a pawn
            if ((1ULL << bit) & pawnAttacks[!color]) {
                eg += QueenCheckedByPawn[MG];
                mg += QueenCheckedByPawn[EG];
            }

            // Queen gains a mobility bonus based off of the number
            // of attacked or defended squares within the mobility area
            mobilityCount = bitCount((mobilityArea & attacks));
            mg += QueenMobility[MG][mobilityCount];
            eg += QueenMobility[EG][mobilityCount];

            attacks = attacks & kingAreas[!color];
            if (attacks) {
                attackCounts[color] += 4 * bitCount(attacks);
                attackerCounts[color]++;
            }
        }
    }

    // Evaluate the passed pawns for both colors
    for (color = BLACK; color >= WHITE; color--) {

        // Negate the scores so that the scores are from
        // White's perspective after the loop completes
        mg = -mg; eg = -eg;

        tempPawns = pos->ColorBB[color] & passedPawns;

        while (tempPawns) {

            // Pop off the next Passed Pawn
            bit = PopBit(&tempPawns);

            // Determine the relative  rank
            rank = (color == BLACK) ? (7 - Rank(bit)) : Rank(bit);

            // Determine where we would advance to
            destination = (color == BLACK)
                ? ((1ULL << bit) >> 8)
                : ((1ULL << bit) << 8);

            canAdvance = (destination & notEmpty) == 0ULL;
            safeAdvance = !(destination & allAttackBoards[!color]);

            mg += PassedPawn[canAdvance][safeAdvance][rank][MG];;
            eg += PassedPawn[canAdvance][safeAdvance][rank][EG];
        }
    }

    for (color = BLACK; color >= WHITE; color--) {

        // Negate the scores so that the scores are from
        // White's perspective after the loop completes
        mg = -mg; eg = -eg;

        if (attackerCounts[!color] >= 2) {

            // Dont allow attack count to exceed 99
            if (attackCounts[!color] >= 100)
                attackCounts[!color] = 99;

            // Reduce attack count if there are no enemy queens 
            if (!(pos->ColorBB[!color] & queens))
                attackCounts[!color] *= .25;

            mg -= SafetyTable[attackCounts[!color]];
            eg -= SafetyTable[attackCounts[!color]];
        }
    }

    mg += (pos->side == WHITE) ? Tempo[MG] : -Tempo[MG];
    eg += (pos->side == WHITE) ? Tempo[EG] : -Tempo[EG];

    int evalpiecesMG, evalpiecesEG;
    evalpiecesMG = (bitCount(pos->PieceBB[P]) - bitCount(pos->PieceBB[p])) * pieceValues[0][0];
    evalpiecesMG += (bitCount(pos->PieceBB[R]) - bitCount(pos->PieceBB[r])) * pieceValues[3][0];
    evalpiecesMG += (bitCount(pos->PieceBB[N]) - bitCount(pos->PieceBB[n])) * pieceValues[1][0];
    evalpiecesMG += (bitCount(pos->PieceBB[B]) - bitCount(pos->PieceBB[b])) * pieceValues[2][0];
    evalpiecesMG += (bitCount(pos->PieceBB[Q]) - bitCount(pos->PieceBB[q])) * pieceValues[4][0];

    evalpiecesEG = (bitCount(pos->PieceBB[P]) - bitCount(pos->PieceBB[p])) * pieceValues[0][1];
    evalpiecesEG += (bitCount(pos->PieceBB[R]) - bitCount(pos->PieceBB[r])) * pieceValues[3][1];
    evalpiecesEG += (bitCount(pos->PieceBB[N]) - bitCount(pos->PieceBB[n])) * pieceValues[1][1];
    evalpiecesEG += (bitCount(pos->PieceBB[B]) - bitCount(pos->PieceBB[b])) * pieceValues[2][1];
    evalpiecesEG += (bitCount(pos->PieceBB[Q]) - bitCount(pos->PieceBB[q])) * pieceValues[4][1];

    mg = mg + evalpiecesMG;
    eg = eg + evalpiecesEG;

    curPhase = 24 - (bitCount(knights | bishops))
        - (bitCount(rooks) << 1)
        - (bitCount(queens) << 2);

    curPhase = (curPhase * 256 + 12) / 24;

    eval = ((mg * (256 - curPhase)) + (eg * curPhase)) / 256;

    return pos->side == WHITE ? eval : -eval;
}

// hashkeys.c

U64 GeneratePosKey(const S_BOARD* pos) {

    int sq = 0;
    U64 finalKey = 0;
    int piece = e;

    // pieces
    for (sq = 0; sq < BRD_SQ_NUM; ++sq) {
        piece = pos->pieces[sq];
        if (piece != no_sq && piece != e) {
            finalKey ^= PieceKeys[piece][sq];
        }
    }

    if (pos->side == WHITE) {
        finalKey ^= SideKey;
    }

    if (pos->enPas != no_sq) {
        finalKey ^= PieceKeys[e][pos->enPas];
    }

    finalKey ^= CastleKeys[pos->castlePerm];

    return finalKey;
}

//misc

int GetTimeMs() {
    return GetTickCount();
}

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

void ReadInput(S_SEARCHINFO* info) {
    int             bytes;
    char            input[256] = "", * endc;

    if (InputWaiting()) {
        info->stopped = TRUE;
        do {
            bytes = read(fileno(stdin), input, 256);
        } while (bytes < 0);
        endc = strchr(input, '\n');
        if (endc) *endc = 0;

        if (strlen(input) > 0) {
            if (!strncmp(input, "quit", 4)) {
                info->quit = TRUE;
            }
        }
        return;
    }
}

//perft

long leafNodes;

void Perft(int depth, S_BOARD* pos) {

    if (depth == 0) {
        leafNodes++;
        return;
    }

    S_MOVELIST list[1];
    GenerateAllMoves(pos, list);

    int MoveNum = 0;
    for (MoveNum = 0; MoveNum < list->count; ++MoveNum) {

        //Ignore the illegal moves
        if (!MakeMove(pos, list->moves[MoveNum].move)) {
            continue;
        }
        Perft(depth - 1, pos);
        TakeMove(pos);
    }

    return;
}


void PerftTest(int depth, S_BOARD* pos) {

    PrintBoard(pos);
    printf("\nStarting Test To Depth:%d\n", depth);
    leafNodes = 0;
    int start = GetTimeMs();
    S_MOVELIST list[1];
    GenerateAllMoves(pos, list);

    int move;
    int MoveNum = 0;
    for (MoveNum = 0; MoveNum < list->count; ++MoveNum) {
        move = list->moves[MoveNum].move;
        if (!MakeMove(pos, move)) {
            continue;
        }
        long cumnodes = leafNodes;
        Perft(depth - 1, pos);
        TakeMove(pos);
        long oldnodes = leafNodes - cumnodes;
        printf("move %d : %s : %ld\n", MoveNum + 1, PrMove(move), oldnodes);
    }

    printf("\nTest Complete : %ld nodes visited in %dms\n", leafNodes, GetTimeMs() - start);

    return;
}

//search

static void CheckUp(S_SEARCHINFO* info) {
    // .. check if time up, or interrupt from GUI
    if (info->timeset == TRUE && GetTimeMs() > info->stoptime) {
        info->stopped = TRUE;
    }

    ReadInput(info);
}

static void PickNextMove(int moveNum, S_MOVELIST* list) {

    S_MOVE temp;
    int index = 0;
    int bestScore = 0;
    int bestNum = moveNum;

    for (index = moveNum; index < list->count; ++index) {
        if (list->moves[index].score > bestScore) {
            bestScore = list->moves[index].score;
            bestNum = index;
        }
    }
    temp = list->moves[moveNum];
    list->moves[moveNum] = list->moves[bestNum];
    list->moves[bestNum] = temp;
}

static int IsRepetition(const S_BOARD* pos) {

    int index = 0;

    for (index = pos->hisPly - pos->fiftyMove; index < pos->hisPly - 1; ++index) {
        if (pos->posKey == pos->history[index].posKey) {
            return TRUE;
        }
    }
    return FALSE;
}

static void ClearForSearch(S_BOARD* pos, S_SEARCHINFO* info) {

    int index = 0;
    int index2 = 0;

    for (index = 0; index < 13; ++index) {
        for (index2 = 0; index2 < BRD_SQ_NUM; ++index2) {
            pos->searchHistory[index][index2] = 0;
        }
    }

    for (index = 0; index < 2; ++index) {
        for (index2 = 0; index2 < MAXDEPTH; ++index2) {
            pos->searchKillers[index][index2] = 0;
        }
    }

    pos->HashTable->overWrite = 0;
    pos->HashTable->hit = 0;
    pos->HashTable->cut = 0;
    pos->ply = 0;

    info->stopped = 0;
    info->nodes = 0;
    info->fh = 0;
    info->fhf = 0;
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

static int Quiescence(int alpha, int beta, S_BOARD* pos, S_SEARCHINFO* info) {

    if ((info->nodes & 2047) == 0) {
        CheckUp(info);
    }

    info->nodes++;

    if (IsRepetition(pos) || pos->fiftyMove >= 100) {
        return 0;
    }

    if (pos->ply > MAXDEPTH - 1) {
        return Evalpos(pos);
    }

    // Mate Distance Pruning
    alpha = MAX(alpha, -infinite + pos->ply);
    beta = MIN(beta, infinite - pos->ply);
    if (alpha >= beta) {
        return alpha;
    }

    int Score = Evalpos(pos);

    if (Score >= beta) {
        return beta;
    }

    if (Score > alpha) {
        alpha = Score;
    }

    S_MOVELIST list[1];
    GenerateAllCaps(pos, list);

    int MoveNum = 0;
    int Legal = 0;
    Score = -infinite;

    for (MoveNum = 0; MoveNum < list->count; ++MoveNum) {

        PickNextMove(MoveNum, list);

        if (!MakeMove(pos, list->moves[MoveNum].move)) {
            continue;
        }

        Legal++;
        Score = -Quiescence(-beta, -alpha, pos, info);
        TakeMove(pos);

        if (info->stopped == TRUE) {
            return 0;
        }

        if (Score > alpha) {
            if (Score >= beta) {
                if (Legal == 1) {
                    info->fhf++;
                }
                info->fh++;
                return beta;
            }
            alpha = Score;
        }
    }

    return alpha;
}

int BigPiecesExist(S_BOARD* pos, int side)
{
    if (side == 0)
    {
        if (((pos->PieceBB[N]) | (pos->PieceBB[B]) | (pos->PieceBB[R]) | (pos->PieceBB[Q])) == 0ULL)
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
        if (((pos->PieceBB[n]) | (pos->PieceBB[b]) | (pos->PieceBB[r]) | (pos->PieceBB[q])) == 0ULL)
        {
            return 0;
        }
        else
        {
            return 1;
        }
    }
}

// Null Move Pruning Values
static const int RR = 2;
static const int minDepth = 3;

// Razoring Values
static const int RazorDepth = 3;
static const int RazorMargin[4] = { 0, 200, 400, 600 };

// Reverse Futility Values
static const int RevFutilityDepth = 4;
static const int RevFutilityMargin[5] = { 0, 250, 500, 750, 1000 };

/// LMR Values
static const int LateMoveDepth = 3;
static const int FullSearchMoves = 4;

static int AlphaBeta(int alpha, int beta, int depth, S_BOARD* pos, S_SEARCHINFO* info, int doNull, int DoLMR) {

    int InCheck = UnderCheck(pos, pos->side);

    if (InCheck == TRUE) {
        depth++;
    }
    
    if (depth == 0) {
        return Quiescence(alpha, beta, pos, info);
    }

    if ((info->nodes & 2047) == 0) {
        CheckUp(info);
    }

    info->nodes++;

    if ((IsRepetition(pos) || pos->fiftyMove >= 100) && pos->ply) {
        return 0;
    }

    if (pos->ply > MAXDEPTH - 1) {
        return Evalpos(pos);
    }

    // Mate Distance Pruning (finds shorter mates)
    alpha = MAX(alpha, -infinite + pos->ply);
    beta = MIN(beta, infinite - pos->ply);
    if (alpha >= beta) {
        return alpha;
    }

    int Score = -infinite;

    int PvMove = NOMOVE;

    if (ProbeHashEntry(pos, &PvMove, &Score, alpha, beta, depth) == 1) {
        pos->HashTable->cut++;
        return Score;
    }

    const int positionEval = Evalpos(pos);

    // Razoring
    if (depth <= RazorDepth && !PvMove && !InCheck && positionEval + RazorMargin[depth] <= alpha) {
        // drop into qSearch if move most likely won't beat alpha
        Score = Quiescence(alpha - RazorMargin[depth], beta - RazorMargin[depth], pos, info);
        if (Score + RazorMargin[depth] <= alpha) {
            return Score;
        }
    }

    // Reverse Futility Pruning (prunes near beta)
    if (depth <= RevFutilityDepth && !PvMove && !InCheck && abs(beta) < ISMATE && positionEval - RevFutilityMargin[depth] >= beta) {
        return positionEval - RevFutilityMargin[depth];
    }

    // Null Move Pruning
    if (depth >= minDepth && doNull && !InCheck && pos->ply && (BigPiecesExist(pos, pos->side)) && positionEval >= beta) {
        MakeNullMove(pos);
        Score = -AlphaBeta(-beta, -beta + 1, depth - 1 - RR, pos, info, 0, 0);
        TakeNullMove(pos);
        if (info->stopped == 1) {
            return 0;
        }

        if (Score >= beta && abs(Score) < ISMATE) {
            info->nullCut++;
            return beta;
        }
    }

    S_MOVELIST list[1];
    GenerateAllMoves(pos, list);

    int MoveNum = 0;
    int Legal = 0;
    int OldAlpha = alpha;
    int BestMove = NOMOVE;

    int BestScore = -infinite;

    Score = -infinite;

    if (PvMove != NOMOVE) {
        for (MoveNum = 0; MoveNum < list->count; ++MoveNum) {
            if (list->moves[MoveNum].move == PvMove) {
                list->moves[MoveNum].score = 2000000;
                break;
            }
        }
    }

    int FoundPv = 0;

    for (MoveNum = 0; MoveNum < list->count; ++MoveNum) {

        PickNextMove(MoveNum, list);

        //if move is illegal, go to next one
        if (!MakeMove(pos, list->moves[MoveNum].move)) {
            continue;
        }

        //if move is legal
        Legal++;

        // PVS (speeds up search with good move ordering)
        if (FoundPv == TRUE) {

            // Late Move Reductions at Root (reduces moves if past full move search limit (not reducing captures, checks, or promotions))
            if (depth >= LateMoveDepth && !(list->moves[MoveNum].move & MFLAGCAP) && !(list->moves[MoveNum].move & MFLAGPROM) && !UnderCheck(pos, pos->side) && DoLMR && Legal > FullSearchMoves && !(list->moves[MoveNum].score == 800000 || list->moves[MoveNum].score == 900000)) {

                // get initial reduction depth
                int reduce = LMRTable[MIN(depth, 63)][MIN(Legal, 63)];
                // search with the reduced depth
                Score = -AlphaBeta(-alpha - 1, -alpha, depth - 1 - reduce, pos, info, 1, 0);

            }
            else {
                // If LMR conditions not met (not at root, or tactical move), do a null window search (because we are using PVS)
                Score = -AlphaBeta(-alpha - 1, -alpha, depth - 1, pos, info, 1, 1);

            }
            if (Score > alpha&& Score < beta) {
                // If the LMR or the null window fails, do a full search
                Score = -AlphaBeta(-beta, -alpha, depth - 1, pos, info, 1, 0);

            }
        }
        else {
            // If no PV found, do a full search
            Score = -AlphaBeta(-beta, -alpha, depth - 1, pos, info, 1, 0);

        }

        TakeMove(pos);

        if (info->stopped == TRUE) {
            return 0;
        }

        if (Score > BestScore) {
            BestScore = Score;
            BestMove = list->moves[MoveNum].move;
            if (Score > alpha) {
                if (Score >= beta) {
                    if (Legal == 1) {
                        info->fhf++;
                    }
                    info->fh++;

                    if (!(list->moves[MoveNum].move & MFLAGCAP)) {
                        pos->searchKillers[1][pos->ply] = pos->searchKillers[0][pos->ply];
                        pos->searchKillers[0][pos->ply] = list->moves[MoveNum].move;
                    }

                    StoreHashEntry(pos, BestMove, beta, HFBETA, depth);

                    return beta;
                }
                FoundPv = 1;
                alpha = Score;

                if (!(list->moves[MoveNum].move & MFLAGCAP)) {
                    pos->searchHistory[pos->pieces[FROMSQ(BestMove)]][TOSQ(BestMove)] += depth;
                }
            }
        }
    }

    if (Legal == 0) {
        if (InCheck) {
            return -infinite + pos->ply;
        }
        else {
            return 0;
        }
    }

    if (alpha != OldAlpha) {
        StoreHashEntry(pos, BestMove, BestScore, HFEXACT, depth);
    }
    else {
        StoreHashEntry(pos, BestMove, alpha, HFALPHA, depth);
    }

    return alpha;
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

void SearchPosition(S_BOARD* pos, S_SEARCHINFO* info) {

    int bestMove = NOMOVE;
    int bestScore = -infinite;
    int currentDepth = 0;
    int pvMoves = 0;
    int pvNum = 0;

    ClearForSearch(pos, info);

    for (currentDepth = 1; currentDepth <= info->depth; ++currentDepth) {
                            // alpha	 beta
        bestScore = AlphaBeta(-infinite, infinite, currentDepth, pos, info, 1, 1);

        if (info->stopped == TRUE) {
            break;
        }

        pvMoves = GetPvLine(currentDepth, pos);
        bestMove = pos->PvArray[0];

        if (absoluteValue(bestScore) > ISMATE) {
            bestScore = (bestScore > 0 ? infinite - bestScore + 1 : -infinite - bestScore) / 2;
            printf("info score mate %d depth %d nodes %ld time %d ",
                bestScore, currentDepth, info->nodes, GetTimeMs() - info->starttime);
        }
        else{
            printf("info score cp %d depth %d nodes %ld time %d ",
                bestScore, currentDepth, info->nodes, GetTimeMs() - info->starttime);
        }
        
        
        pvMoves = GetPvLine(currentDepth, pos);
        printf("pv");
        for (pvNum = 0; pvNum < pvMoves; ++pvNum) {
             printf(" %s", PrMove(pos->PvArray[pvNum]));
            }
            printf("\n");
    }

    printf("bestmove %s\n", PrMove(bestMove));

}

//uci

#define INPUTBUFFER 400 * 6

// go depth 6 wtime 180000 btime 100000 binc 1000 winc 1000 movetime 1000 movestogo 40
void ParseGo(char* line, S_SEARCHINFO* info, S_BOARD* pos) {

    int depth = -1, movestogo = 30, movetime = -1;
    int time = -1, inc = 0;
    char* ptr = NULL;
    info->timeset = FALSE;

    if ((ptr = strstr(line, "infinite"))) {
        ;
    }

    if ((ptr = strstr(line, "binc")) && pos->side == BLACK) {
        inc = atoi(ptr + 5);
    }

    if ((ptr = strstr(line, "winc")) && pos->side == WHITE) {
        inc = atoi(ptr + 5);
    }

    if ((ptr = strstr(line, "wtime")) && pos->side == WHITE) {
        time = atoi(ptr + 6);
    }

    if ((ptr = strstr(line, "btime")) && pos->side == BLACK) {
        time = atoi(ptr + 6);
    }

    if ((ptr = strstr(line, "movestogo"))) {
        movestogo = atoi(ptr + 10);
    }

    if ((ptr = strstr(line, "movetime"))) {
        movetime = atoi(ptr + 9);
    }

    if ((ptr = strstr(line, "depth"))) {
        depth = atoi(ptr + 6);
    }

    if (movetime != -1) {
        time = movetime;
        movestogo = 1;
    }

    info->starttime = GetTimeMs();
    info->depth = depth;

    if (time != -1) {
        info->timeset = TRUE;
        time /= movestogo;
        time -= 50;
        info->stoptime = info->starttime + time + inc;
    }

    if (depth == -1) {
        info->depth = MAXDEPTH;
    }

    printf("time:%d start:%d stop:%d depth:%d timeset:%d\n",
        time, info->starttime, info->stoptime, info->depth, info->timeset);
    SearchPosition(pos, info);
}

void ParsePosition(char* lineIn, S_BOARD* pos) {

    lineIn += 9;
    char* ptrChar = lineIn;

    if (strncmp(lineIn, "startpos", 8) == 0) {
        ParseFen(START_FEN, pos);
    }
    else {
        ptrChar = strstr(lineIn, "fen");
        if (ptrChar == NULL) {
            ParseFen(START_FEN, pos);
        }
        else {
            ptrChar += 4;
            ParseFen(ptrChar, pos);
        }
    }

    ptrChar = strstr(lineIn, "moves");
    int move;

    if (ptrChar != NULL) {
        ptrChar += 6;
        while (*ptrChar) {
            move = ParseMove(ptrChar, pos);
            if (move == NOMOVE) break;
            MakeMove(pos, move);
            pos->ply = 0;
            while (*ptrChar && *ptrChar != ' ') ptrChar++;
            ptrChar++;
        }
    }
    PrintBoard(pos);
}

void Uci_Loop(S_BOARD* pos, S_SEARCHINFO* info) {

    setbuf(stdin, NULL);
    setbuf(stdout, NULL);

    char line[INPUTBUFFER];
    printf("id name %s\n", NAME);
    printf("id author Jay Warendorff\n");
    printf("uciok\n");

    while (TRUE) {
        memset(&line[0], 0, sizeof(line));
        fflush(stdout);
        if (!fgets(line, INPUTBUFFER, stdin))
            continue;

        if (line[0] == '\n')
            continue;

        if (!strncmp(line, "isready", 7)) {
            printf("readyok\n");
            continue;
        }
        else if (!strncmp(line, "position", 8)) {
            ParsePosition(line, pos);
        }
        else if (!strncmp(line, "ucinewgame", 10)) {
            ParsePosition("position startpos\n", pos);
        }
        else if (!strncmp(line, "go", 2)) {
            ParseGo(line, info, pos);
        }
        else if (!strncmp(line, "quit", 4)) {
            info->quit = TRUE;
            break;
        }
        else if (!strncmp(line, "uci", 3)) {
            printf("id name %s\n", NAME);
            printf("id author Jay Warendorff\n");
            printf("uciok\n");
        }
        if (info->quit) break;
    }
}

int main() {

    // debug mode variable
    int debug = 0;

    // if debugging - debug 1
    if (debug)
    {
        AllInit();

        S_BOARD pos[1];
        S_SEARCHINFO info[1];

        //Perft results depth 3 - 8902, depth 4 - 197,281, depth 5 - 4,865,609, depth 6 - 119,060,324

        ParseFen(START_FEN, pos);
        printf("eval %d\n", Evalpos(pos));
        //depth 5 - 193690690
        //ParseFen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -", pos);
        //PerftTest(5, pos);

        //ParseFen("r3kb1r/1bpq1ppp/ppn1p3/8/3PP3/P1BB1N2/1P3PPP/R2QK2R w KQkq - 2 11", pos);
        //PerftTest(1, pos);
        
        while (1);
    }
    else
    {
        AllInit();

        S_BOARD pos[1];
        S_SEARCHINFO info[1];
        info->quit = FALSE;
        pos->HashTable->pTable = NULL;
        InitHashTable(pos->HashTable, 128);
        setbuf(stdin, NULL);
        setbuf(stdout, NULL);

        char line[256];
        while (TRUE) {
            memset(&line[0], 0, sizeof(line));

            fflush(stdout);
            if (!fgets(line, 256, stdin))
                continue;
            if (line[0] == '\n')
                continue;
            if (!strncmp(line, "uci", 3)) {
                Uci_Loop(pos, info);
                if (info->quit == TRUE) break;
                continue;
            }
            else if (!strncmp(line, "quit", 4)) {
                break;
            }
        }

        free(pos->HashTable->pTable);
    }
    return 0;
}

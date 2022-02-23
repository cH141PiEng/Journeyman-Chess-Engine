#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include <windows.h>
//#include <unistd.h> //needed for compiling with gcc, but does not work on VS 2019

//Journeyman 1.0
//Uses _BitScanForward64, __popcnt64
//Modified version of the Video Instruction Chess Engine video #82
//Instead of a 10x12 board, uses a 64 array for the board
//Increased history table ~128 MB
//Elo in 2 min + 1 sec blitz around 1600

typedef unsigned long long U64;

#define NAME "Journeyman 1.0"
#define BRD_SQ_NUM 64

#define MAXGAMEMOVES 2048//Max # of half moves expected in a game
//Needed for the history of the game
#define MAXPOSITIONMOVES 256
#define MAXDEPTH 64

#define START_FEN  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

//Numbers for not a piece and the pieces:
enum { e = 0, P, N, B, R, Q, K, p, n, b, r, q, k };

enum { FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H, FILE_NONE };
enum { RANK_1, RANK_2, RANK_3, RANK_4, RANK_5, RANK_6, RANK_7, RANK_8, RANK_NONE };

enum { WHITE, BLACK, BOTH };

// The squares of the board
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

//the pvtable is represented using two structures

//A structure used as hashing element it has a posKey and a best move to be hashed for that posKey
//The table structure which has the array of above structure


//The entries which will be present in the PVTABLE...it is a pair of the posKey and the 
//move which will be stored in the principal variation table 
typedef struct {
    U64 posKey;
    int move;
} S_PVENTRY;

//The actual PVTABLE structure which stores the entries array
typedef struct {
    S_PVENTRY* pTable;
    int numEntries;
} S_PVTABLE;

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
    U64 pawns[3];//Pawn bitboards - white, black, both

    int KingSq[2];//King square for white and black

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

    int pceNum[13];//number of pieces for each piece type
    int bigPce[2];//number of all pieces except pawns
    int majPce[2];//number of rooks and queens for white and black
    int minPce[2];//number of knights and bishops for white and black 
    int material[2];//material value for white and black

    S_UNDO history[MAXGAMEMOVES];//Stores all of the previous game states with things which are stated in the undo 
    //structure and also it helps to check if a move is repeated or not. Can do so by using the 
    //hisPly as the index and going back to all the states and see if the posKey is repeated or not

    // piece list
    int pList[13][10];

    S_PVTABLE PvTable[1];//Keep one PvTable in the main structure for the current board position...in the form of 
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

#define IsBQ(p) (PieceBishopQueen[(p)])
#define IsRQ(p) (PieceRookQueen[(p)])
#define IsKn(p) (PieceKnight[(p)])
#define IsKi(p) (PieceKing[(p)])

#define MIRROR64(sq) (Mirror64[(sq)])

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
int PieceMaj[13];
int PieceMin[13];
int PieceVal[13];
int PieceCol[13];
int PiecePawn[13];

int FilesBrd[BRD_SQ_NUM];
int RanksBrd[BRD_SQ_NUM];

int Mirror64[64];

U64 FileBBMask[8];
U64 RankBBMask[8];

U64 BlackPassedMask[64];
U64 WhitePassedMask[64];
U64 IsolatedMask[64];

// data.c

char PceChar[] = ".PNBRQKpnbrqk";
char SideChar[] = "wb-";
char RankChar[] = "12345678";
char FileChar[] = "abcdefgh";

int PieceBig[13] = { FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE };
int PieceMaj[13] = { FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE };
int PieceMin[13] = { FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE };
int PieceVal[13] = { 0, 100, 325, 325, 550, 1000, 50000, 100, 325, 325, 550, 1000, 50000 };
int PieceCol[13] = { BOTH, WHITE, WHITE, WHITE, WHITE, WHITE, WHITE,
    BLACK, BLACK, BLACK, BLACK, BLACK, BLACK };

int PiecePawn[13] = { FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE };
int PieceKnight[13] = { FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE };
int PieceKing[13] = { FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE };
int PieceRookQueen[13] = { FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE };
int PieceBishopQueen[13] = { FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE };
int PieceSlides[13] = { FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE };

int Mirror64[64] = {
56	,	57	,	58	,	59	,	60	,	61	,	62	,	63	,
48	,	49	,	50	,	51	,	52	,	53	,	54	,	55	,
40	,	41	,	42	,	43	,	44	,	45	,	46	,	47	,
32	,	33	,	34	,	35	,	36	,	37	,	38	,	39	,
24	,	25	,	26	,	27	,	28	,	29	,	30	,	31	,
16	,	17	,	18	,	19	,	20	,	21	,	22	,	23	,
8	,	9	,	10	,	11	,	12	,	13	,	14	,	15	,
0	,	1	,	2	,	3	,	4	,	5	,	6	,	7
};

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

void InitEvalMasks() {

    int sq, tsq, r, f;

    for (sq = 0; sq < 8; ++sq) {
        FileBBMask[sq] = 0ULL;
        RankBBMask[sq] = 0ULL;
    }

    for (r = RANK_8; r >= RANK_1; r--) {
        for (f = FILE_A; f <= FILE_H; f++) {
            sq = r * 8 + f;
            FileBBMask[f] |= (1ULL << sq);
            RankBBMask[r] |= (1ULL << sq);
        }
    }

    for (sq = 0; sq < 64; ++sq) {
        IsolatedMask[sq] = 0ULL;
        WhitePassedMask[sq] = 0ULL;
        BlackPassedMask[sq] = 0ULL;
    }

    for (sq = 0; sq < 64; ++sq) {
        tsq = sq + 8;

        while (tsq < 64) {
            WhitePassedMask[sq] |= (1ULL << tsq);
            tsq += 8;
        }

        tsq = sq - 8;
        while (tsq >= 0) {
            BlackPassedMask[sq] |= (1ULL << tsq);
            tsq -= 8;
        }

        if (FilesBrd[sq] > FILE_A) {
            IsolatedMask[sq] |= FileBBMask[FilesBrd[sq] - 1];

            tsq = sq + 7;
            while (tsq < 64) {
                WhitePassedMask[sq] |= (1ULL << tsq);
                tsq += 8;
            }

            tsq = sq - 9;
            while (tsq >= 0) {
                BlackPassedMask[sq] |= (1ULL << tsq);
                tsq -= 8;
            }
        }

        if (FilesBrd[sq] < FILE_H) {
            IsolatedMask[sq] |= FileBBMask[FilesBrd[sq] + 1];

            tsq = sq + 9;
            while (tsq < 64) {
                WhitePassedMask[sq] |= (1ULL << tsq);
                tsq += 8;
            }

            tsq = sq - 7;
            while (tsq >= 0) {
                BlackPassedMask[sq] |= (1ULL << tsq);
                tsq -= 8;
            }
        }
    }
}

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

void AllInit() {
    InitBitMasks();
    InitHashKeys();
    InitFilesRanksBrd();
    InitEvalMasks();
    InitMvvLva();
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
    int index = 0;
    int t_pceNum = -1;

    HASH_PCE(pce, sq);

    pos->pieces[sq] = e;
    pos->material[col] -= PieceVal[pce];

    if (PieceBig[pce]) {
        pos->bigPce[col]--;
        if (PieceMaj[pce]) {
            pos->majPce[col]--;
        }
        else {
            pos->minPce[col]--;
        }
    }
    else {
        CLRBIT(pos->pawns[col], sq);
        CLRBIT(pos->pawns[BOTH], sq);
    }

    for (index = 0; index < pos->pceNum[pce]; ++index) {
        if (pos->pList[pce][index] == sq) {
            t_pceNum = index;
            break;
        }
    }

    pos->pceNum[pce]--;

    pos->pList[pce][t_pceNum] = pos->pList[pce][pos->pceNum[pce]];

}

static void AddPiece(const int sq, S_BOARD* pos, const int pce) {

    int col = PieceCol[pce];

    HASH_PCE(pce, sq);

    pos->pieces[sq] = pce;

    if (PieceBig[pce]) {
        pos->bigPce[col]++;
        if (PieceMaj[pce]) {
            pos->majPce[col]++;
        }
        else {
            pos->minPce[col]++;
        }
    }
    else {
        SETBIT(pos->pawns[col], sq);
        SETBIT(pos->pawns[BOTH], sq);
    }

    pos->material[col] += PieceVal[pce];
    pos->pList[pce][pos->pceNum[pce]++] = sq;

}

static void MovePiece(const int from, const int to, S_BOARD* pos) {

    int index = 0;
    int pce = pos->pieces[from];
    int col = PieceCol[pce];

    HASH_PCE(pce, from);
    pos->pieces[from] = e;

    HASH_PCE(pce, to);
    pos->pieces[to] = pce;

    if (!PieceBig[pce]) {
        CLRBIT(pos->pawns[col], from);
        CLRBIT(pos->pawns[BOTH], from);
        SETBIT(pos->pawns[col], to);
        SETBIT(pos->pawns[BOTH], to);
    }

    for (index = 0; index < pos->pceNum[pce]; ++index) {
        if (pos->pList[pce][index] == from) {
            pos->pList[pce][index] = to;
            break;
        }
    }
}

void TakeMove(S_BOARD* pos);
int SqAttacked(const int sq, const int side, const S_BOARD* pos);

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

    if (PiecePawn[pos->pieces[from]]) {
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

    if (PieceKing[pos->pieces[to]]) {
        pos->KingSq[pos->side] = to;
    }

    pos->side ^= 1;
    HASH_SIDE;

    if (SqAttacked(pos->KingSq[side], pos->side, pos)) {
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

    if (PieceKing[pos->pieces[from]]) {
        pos->KingSq[pos->side] = from;
    }

    int captured = CAPTURED(move);
    if (captured != e) {
        AddPiece(to, pos, captured);
    }

    if (PROMOTED(move) != e) {
        ClearPiece(from, pos);
        AddPiece(from, pos, (PieceCol[PROMOTED(move)] == WHITE ? P : p));
    }
}

// attack

const int KnDir[8] = { -6, -17,	-15, -10, 6, 17, 15, 10 };
const int RkDir[4] = { -1, -8,	1, 8 };
const int BiDir[4] = { -7, -9, 7, 9 };
const int KiDir[8] = { -1, -8,	1, 8, -7, -9, 7, 9 };

// is square attacked by side? - needed in particular to check if king is in check or squares
// in between king and rook to castle with are attacked by any piece of opposite color.
int SqAttacked(const int sq, const int side, const S_BOARD* pos) {

    int cs, rs, ct, rt, pce, index, t_sq, dir;

    cs = COL(sq);
    rs = ROW(sq);

    // pawns
    if (side == WHITE) {
        t_sq = sq - 9;
        ct = COL(t_sq);
        if (pos->pieces[sq - 9] == P && (0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1)) {
            return TRUE;
        }
        t_sq = sq - 7;
        ct = COL(t_sq);
        if (pos->pieces[t_sq] == P && (0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1)) {
            return TRUE;
        }
    }
    else {
        t_sq = sq + 9;
        ct = COL(t_sq);
        if (pos->pieces[t_sq] == p && (0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1)) {
            return TRUE;
        }
        t_sq = sq + 7;
        ct = COL(t_sq);
        if (pos->pieces[t_sq] == p && (0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1)) {
            return TRUE;
        }
    }

    // knights
    for (index = 0; index < 8; ++index) {
        t_sq = sq + KnDir[index];
        pce = pos->pieces[t_sq];
        ct = COL(t_sq);
        rt = ROW(t_sq);
        if (((0 <= t_sq) && (t_sq <= 63) && (-2 <= (cs - ct)) && ((cs - ct) <= 2) && (-2 <= (rs - rt)) && ((rs - rt) <= 2)) && IsKn(pce) && PieceCol[pce] == side) {
            return TRUE;
        }
    }

    // rooks, queens
    for (index = 0; index < 4; ++index) {
        dir = RkDir[index];
        t_sq = sq + dir;
        cs = COL(sq);
        rs = ROW(sq);
        ct = COL(t_sq);
        rt = ROW(t_sq);

        if ((0 <= t_sq) && (t_sq <= 63))
        {
            pce = pos->pieces[t_sq];
        }
        else
        {
            pce = e;
        }
        while ((0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1) && (-1 <= (rs - rt)) && ((rs - rt) <= 1)) {
            if (pce != e) {
                if (IsRQ(pce) && PieceCol[pce] == side) {
                    return TRUE;
                }
                break;
            }
            cs = COL(t_sq);
            rs = ROW(t_sq);
            t_sq += dir;
            if ((0 <= t_sq) && (t_sq <= 63))
            {
                pce = pos->pieces[t_sq];
            }
            else
            {
                pce = e;
            }
            ct = COL(t_sq);
            rt = ROW(t_sq);
        }
    }

    // bishops, queens
    for (index = 0; index < 4; ++index) {
        dir = BiDir[index];
        t_sq = sq + dir;
        cs = COL(sq);
        rs = ROW(sq);
        ct = COL(t_sq);
        rt = ROW(t_sq);

        if ((0 <= t_sq) && (t_sq <= 63))
        {
            pce = pos->pieces[t_sq];
        }
        else
        {
            pce = e;
        }
        while ((0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1) && (-1 <= (rs - rt)) && ((rs - rt) <= 1)) {
            if (pce != e) {
                if (IsBQ(pce) && PieceCol[pce] == side) {
                    return TRUE;
                }
                break;
            }
            cs = COL(t_sq);
            rs = ROW(t_sq);
            t_sq += dir; //if (sq == 4 && dir == 7) { printf("t_sq %d\n", t_sq); }
            if ((0 <= t_sq) && (t_sq <= 63))
            {
                pce = pos->pieces[t_sq];
            }
            else
            {
                pce = e;
            }
            ct = COL(t_sq);
            rt = ROW(t_sq);
        }
    }

    cs = COL(sq);
    rs = ROW(sq);

    // kings
    for (index = 0; index < 8; ++index) {
        t_sq = sq + KiDir[index];
        pce = pos->pieces[t_sq];
        ct = COL(t_sq);
        rt = ROW(t_sq);
        if (((0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1) && (-1 <= (rs - rt)) && ((rs - rt) <= 1)) && IsKi(pce) && PieceCol[pce] == side) {
            return TRUE;
        }
    }

    return FALSE;

}

// movegen

#define MOVE(f,t,ca,pro,fl) ( (f) | ((t) << 7) | ( (ca) << 14 ) | ( (pro) << 20 ) | (fl))

//These arrays enable generation of moves for sliding pieces of both sides as they define the starting 
//index of white and black sliding pieces and when we reach a 0 we have completed one side's sliding pieces 
const int LoopSlidePce[8] = {
 B, R, Q, 0, b, r, q, 0
};

const int LoopNonSlidePce[6] = {
 N, K, 0, n, k, 0
};

const int LoopSlideIndex[2] = { 0, 4 };
const int LoopNonSlideIndex[2] = { 0, 3 };

//These store the directions for each piece which need to be added to the current square...to get all the squares 
//where a type of piece indexed by rows can move; 

const int PceDir[13][8] = {
    { 0, 0, 0, 0, 0, 0, 0 },
    { 0, 0, 0, 0, 0, 0, 0 },
    { -6, -17, -15, -10, 6, 17, 15, 10 },
    { -7, -9, 7, 9, 0, 0, 0, 0 },
    { -1, -8, 1, 8, 0, 0, 0, 0 },
    { -1, -8, 1, 8, -7, -9, 7, 9 },
    { -1, -8, 1, 8, -7, -9, 7, 9 },
    { 0, 0, 0, 0, 0, 0, 0 },
    { -6, -17, -15, -10, 6, 17, 15, 10 },
    { -7, -9, 7, 9, 0, 0, 0, 0 },
    { -1, -8, 1, 8, 0, 0, 0, 0 },
    { -1, -8, 1, 8, -7, -9, 7, 9 },
    { -1, -8, 1, 8, -7, -9, 7, 9 }
};

//The number of directions for each piece type - for queen 8, bishop and rook 4 
const int NumDir[13] = {
 0, 0, 8, 4, 4, 8, 8, 0, 8, 4, 4, 8, 8
};

void GenerateAllMoves(const S_BOARD* pos, S_MOVELIST* list);

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

static void AddQuietMove(const S_BOARD* pos, int move, S_MOVELIST* list) {

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

static void AddCaptureMove(const S_BOARD* pos, int move, S_MOVELIST* list) {

    list->moves[list->count].move = move;
    list->moves[list->count].score = MvvLvaScores[CAPTURED(move)][pos->pieces[FROMSQ(move)]] + 1000000;
    list->count++;
}

static void AddEnPassantMove(const S_BOARD* pos, int move, S_MOVELIST* list) {

    list->moves[list->count].move = move;
    list->moves[list->count].score = 105 + 1000000;
    list->count++;
}

static void AddWhitePawnCapMove(const S_BOARD* pos, const int from, const int to, const int cap, S_MOVELIST* list) {

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

static void AddWhitePawnMove(const S_BOARD* pos, const int from, const int to, S_MOVELIST* list) {

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

static void AddBlackPawnCapMove(const S_BOARD* pos, const int from, const int to, const int cap, S_MOVELIST* list) {

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

static void AddBlackPawnMove(const S_BOARD* pos, const int from, const int to, S_MOVELIST* list) {

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

void GenerateAllMoves(const S_BOARD* pos, S_MOVELIST* list) {

    list->count = 0;

    int pce = e;
    int side = pos->side;
    int sq = 0; int t_sq = 0;
    int pceNum = 0;
    int dir = 0;
    int index = 0;
    int pceIndex = 0;

    int cs, ct, rs, rt;

    if (side == WHITE) {

        for (pceNum = 0; pceNum < pos->pceNum[P]; ++pceNum) {
            sq = pos->pList[P][pceNum];

            if ((sq + 8 <= 63) && pos->pieces[sq + 8] == e) {
                AddWhitePawnMove(pos, sq, sq + 8, list);
                if (RanksBrd[sq] == RANK_2 && pos->pieces[sq + 16] == e) {
                    AddQuietMove(pos, MOVE(sq, (sq + 16), e, e, MFLAGPS), list);
                }
            }

            t_sq = sq + 7;
            cs = COL(sq);
            ct = COL(t_sq);
            rs = ROW(sq);
            rt = ROW(t_sq);
            if ((0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1) && (-1 <= (rs - rt)) && ((rs - rt) <= 1) && (PieceCol[pos->pieces[sq + 7]] == BLACK)) {
                AddWhitePawnCapMove(pos, sq, sq + 7, pos->pieces[sq + 7], list);
            }
            t_sq = sq + 9;
            ct = COL(t_sq);
            rt = ROW(t_sq);
            if ((0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1) && (-1 <= (rs - rt)) && ((rs - rt) <= 1) && (PieceCol[pos->pieces[sq + 9]] == BLACK)) {
                AddWhitePawnCapMove(pos, sq, sq + 9, pos->pieces[sq + 9], list);
            }

            if (pos->enPas != no_sq) {
                ct = COL(sq + 7);
                if ((sq + 7 == pos->enPas) && (-1 <= (cs - ct)) && ((cs - ct) <= 1)) {
                    AddEnPassantMove(pos, MOVE(sq, sq + 7, e, e, MFLAGEP), list);
                }
                ct = COL(sq + 9);
                if ((sq + 9 == pos->enPas) && (-1 <= (cs - ct)) && ((cs - ct) <= 1)) {
                    AddEnPassantMove(pos, MOVE(sq, sq + 9, e, e, MFLAGEP), list);
                }
            }
        }

        if (pos->castlePerm & WKCA) {
            if (pos->pieces[f1] == e && pos->pieces[g1] == e) {
                if (!SqAttacked(e1, BLACK, pos) && !SqAttacked(f1, BLACK, pos)) {
                    AddQuietMove(pos, MOVE(e1, g1, e, e, MFLAGCA), list);
                }
            }
        }

        if (pos->castlePerm & WQCA) {
            if (pos->pieces[d1] == e && pos->pieces[c1] == e && pos->pieces[b1] == e) {
                if (!SqAttacked(e1, BLACK, pos) && !SqAttacked(d1, BLACK, pos)) {
                    AddQuietMove(pos, MOVE(e1, c1, e, e, MFLAGCA), list);
                }
            }
        }

    }
    else {

        for (pceNum = 0; pceNum < pos->pceNum[p]; ++pceNum) {
            sq = pos->pList[p][pceNum];

            if ((0 <= sq - 8) && pos->pieces[sq - 8] == e) {
                AddBlackPawnMove(pos, sq, sq - 8, list);
                if (RanksBrd[sq] == RANK_7 && pos->pieces[sq - 16] == e) {
                    AddQuietMove(pos, MOVE(sq, (sq - 16), e, e, MFLAGPS), list);
                }
            }

            t_sq = sq - 7;
            cs = COL(sq);
            ct = COL(t_sq);
            rs = ROW(sq);
            rt = ROW(t_sq);
            if ((0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1) && (-1 <= (rs - rt)) && ((rs - rt) <= 1) && (PieceCol[pos->pieces[sq - 7]] == WHITE)) {
                AddBlackPawnCapMove(pos, sq, sq - 7, pos->pieces[sq - 7], list);
            }
            t_sq = sq - 9;
            ct = COL(t_sq);
            rt = ROW(t_sq);
            if ((0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1) && (-1 <= (rs - rt)) && ((rs - rt) <= 1) && (PieceCol[pos->pieces[sq - 9]] == WHITE)) {
                AddBlackPawnCapMove(pos, sq, sq - 9, pos->pieces[sq - 9], list);
            }
            if (pos->enPas != no_sq) {
                ct = COL(sq - 7);
                if ((sq - 7 == pos->enPas) && (-1 <= (cs - ct)) && ((cs - ct) <= 1)) {
                    AddEnPassantMove(pos, MOVE(sq, sq - 7, e, e, MFLAGEP), list);
                }
                ct = COL(sq - 9);
                if ((sq - 9 == pos->enPas) && (-1 <= (cs - ct)) && ((cs - ct) <= 1)) {
                    AddEnPassantMove(pos, MOVE(sq, sq - 9, e, e, MFLAGEP), list);
                }
            }
        }

        // castling
        if (pos->castlePerm & BKCA) {
            if (pos->pieces[f8] == e && pos->pieces[g8] == e) {
                if (!SqAttacked(e8, WHITE, pos) && !SqAttacked(f8, WHITE, pos)) {
                    AddQuietMove(pos, MOVE(e8, g8, e, e, MFLAGCA), list);
                }
            }
        }

        if (pos->castlePerm & BQCA) {
            if (pos->pieces[d8] == e && pos->pieces[c8] == e && pos->pieces[b8] == e) {
                if (!SqAttacked(e8, WHITE, pos) && !SqAttacked(d8, WHITE, pos)) {
                    AddQuietMove(pos, MOVE(e8, c8, e, e, MFLAGCA), list);
                }
            }
        }
    }

    //The break statement ends the loop immediately when it is encountered. 
    //The continue statement skips the current iteration of the loop and continues with the next iteration. 

    /* Loop for slider pieces */
    pceIndex = LoopSlideIndex[side];
    pce = LoopSlidePce[pceIndex++];
    while (pce != 0) {		

        for (pceNum = 0; pceNum < pos->pceNum[pce]; ++pceNum) {
            sq = pos->pList[pce][pceNum];

            for (index = 0; index < NumDir[pce]; ++index) {
                dir = PceDir[pce][index];
                t_sq = sq + dir;

                cs = COL(sq);
                ct = COL(t_sq);
                rs = ROW(sq);
                rt = ROW(t_sq);

                while ((0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1) && (-1 <= (rs - rt)) && ((rs - rt) <= 1)) {
                    // BLACK ^ 1 == WHITE       WHITE ^ 1 == BLACK
                    if (pos->pieces[t_sq] != e) {
                        if (PieceCol[pos->pieces[t_sq]] == (side ^ 1)) {
                            AddCaptureMove(pos, MOVE(sq, t_sq, pos->pieces[t_sq], e, 0), list);
                        }
                        break;
                    }
                    AddQuietMove(pos, MOVE(sq, t_sq, e, e, 0), list);
                    cs = COL(t_sq);
                    rs = ROW(t_sq);
                    t_sq += dir;
                    ct = COL(t_sq);
                    rt = ROW(t_sq);
                }
            }
        }

        pce = LoopSlidePce[pceIndex++];
    }

    /* Loop for non slider */
    pceIndex = LoopNonSlideIndex[side];
    pce = LoopNonSlidePce[pceIndex++];

    while (pce != 0) {

        for (pceNum = 0; pceNum < pos->pceNum[pce]; ++pceNum) {
            sq = pos->pList[pce][pceNum];

            for (index = 0; index < NumDir[pce]; ++index) {
                dir = PceDir[pce][index];
                t_sq = sq + dir;

                cs = COL(sq);
                ct = COL(t_sq);
                rs = ROW(sq);
                rt = ROW(t_sq);

                if ((pce == N || pce == n) && !((0 <= t_sq) && (t_sq <= 63) && (-2 <= (cs - ct)) && ((cs - ct) <= 2) && (-2 <= (rs - rt)) && ((rs - rt) <= 2)))
                {
                    continue;
                }

                if ((pce == K || pce == k) && !((0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1) && (-1 <= (rs - rt)) && ((rs - rt) <= 1)))
                {
                    continue;
                }

                // BLACK ^ 1 == WHITE       WHITE ^ 1 == BLACK
                if (pos->pieces[t_sq] != e) {
                    if (PieceCol[pos->pieces[t_sq]] == (side ^ 1)) {
                        AddCaptureMove(pos, MOVE(sq, t_sq, pos->pieces[t_sq], e, 0), list);
                    }
                    continue;
                }
                AddQuietMove(pos, MOVE(sq, t_sq, e, e, 0), list);
            }
        }

        pce = LoopNonSlidePce[pceIndex++];
    }
}

//Mostly the same function as GenerateAllMoves, but it only generates the capture moves which are required in the Quiescence search function

void GenerateAllCaps(const S_BOARD* pos, S_MOVELIST* list) {

    list->count = 0;

    int pce = e;
    int side = pos->side;
    int sq = 0; int t_sq = 0;
    int pceNum = 0;
    int dir = 0;
    int index = 0;
    int pceIndex = 0;

    int cs, ct, rs, rt;

    if (side == WHITE) {

        for (pceNum = 0; pceNum < pos->pceNum[P]; ++pceNum) {
            sq = pos->pList[P][pceNum];

            t_sq = sq + 7;
            cs = COL(sq);
            ct = COL(t_sq);
            rs = ROW(sq);
            rt = ROW(t_sq);
            if ((0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1) && (-1 <= (rs - rt)) && ((rs - rt) <= 1) && (PieceCol[pos->pieces[sq + 7]] == BLACK)) {
                AddWhitePawnCapMove(pos, sq, sq + 7, pos->pieces[sq + 7], list);
            }
            t_sq = sq + 9;
            ct = COL(t_sq);
            rt = ROW(t_sq);
            if ((0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1) && (-1 <= (rs - rt)) && ((rs - rt) <= 1) && PieceCol[pos->pieces[sq + 9]] == BLACK) {
                AddWhitePawnCapMove(pos, sq, sq + 9, pos->pieces[sq + 9], list);
            }

            if (pos->enPas != no_sq) {
                ct = COL(sq + 7);
                if ((sq + 7 == pos->enPas) && (-1 <= (cs - ct)) && ((cs - ct) <= 1)) {
                    AddEnPassantMove(pos, MOVE(sq, sq + 7, e, e, MFLAGEP), list);
                }
                ct = COL(sq + 9);
                if ((sq + 9 == pos->enPas) && (-1 <= (cs - ct)) && ((cs - ct) <= 1)) {
                    AddEnPassantMove(pos, MOVE(sq, sq + 9, e, e, MFLAGEP), list);
                }
            }
        }

    }
    else {

        for (pceNum = 0; pceNum < pos->pceNum[p]; ++pceNum) {
            sq = pos->pList[p][pceNum];

            t_sq = sq - 7;
            cs = COL(sq);
            ct = COL(t_sq);
            rs = ROW(sq);
            rt = ROW(t_sq);
            if ((0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1) && (-1 <= (rs - rt)) && ((rs - rt) <= 1) && (PieceCol[pos->pieces[sq - 7]] == WHITE)) {
                AddBlackPawnCapMove(pos, sq, sq - 7, pos->pieces[sq - 7], list);
            }
            t_sq = sq - 9;
            ct = COL(t_sq);
            rt = ROW(t_sq);
            if ((0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1) && (-1 <= (rs - rt)) && ((rs - rt) <= 1) && (PieceCol[pos->pieces[sq - 9]] == WHITE)) {
                AddBlackPawnCapMove(pos, sq, sq - 9, pos->pieces[sq - 9], list);
            }
            if (pos->enPas != no_sq) {
                ct = COL(sq - 7);
                if ((sq - 7 == pos->enPas) && (-1 <= (cs - ct)) && ((cs - ct) <= 1)) {
                    AddEnPassantMove(pos, MOVE(sq, sq - 7, e, e, MFLAGEP), list);
                }
                ct = COL(sq - 9);
                if ((sq - 9 == pos->enPas) && (-1 <= (cs - ct)) && ((cs - ct) <= 1)) {
                    AddEnPassantMove(pos, MOVE(sq, sq - 9, e, e, MFLAGEP), list);
                }
            }
        }
    }

    /* Loop for slider pieces */
    pceIndex = LoopSlideIndex[side];
    pce = LoopSlidePce[pceIndex++];
    while (pce != 0) {

        for (pceNum = 0; pceNum < pos->pceNum[pce]; ++pceNum) {
            sq = pos->pList[pce][pceNum];

            for (index = 0; index < NumDir[pce]; ++index) {
                dir = PceDir[pce][index];
                t_sq = sq + dir;

                cs = COL(sq);
                ct = COL(t_sq);
                rs = ROW(sq);
                rt = ROW(t_sq);

                while ((0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1) && (-1 <= (rs - rt)) && ((rs - rt) <= 1)) {
                    // BLACK ^ 1 == WHITE       WHITE ^ 1 == BLACK
                    if (pos->pieces[t_sq] != e) {
                        if (PieceCol[pos->pieces[t_sq]] == (side ^ 1)) {
                            AddCaptureMove(pos, MOVE(sq, t_sq, pos->pieces[t_sq], e, 0), list);
                        }
                        break;
                    }
                    cs = COL(t_sq);
                    rs = ROW(t_sq);
                    t_sq += dir;
                    ct = COL(t_sq);
                    rt = ROW(t_sq);
                }
            }
        }

        pce = LoopSlidePce[pceIndex++];
    }

    /* Loop for non slider */
    pceIndex = LoopNonSlideIndex[side];
    pce = LoopNonSlidePce[pceIndex++];

    while (pce != 0) {

        for (pceNum = 0; pceNum < pos->pceNum[pce]; ++pceNum) {
            sq = pos->pList[pce][pceNum];

            for (index = 0; index < NumDir[pce]; ++index) {
                dir = PceDir[pce][index];
                t_sq = sq + dir;

                cs = COL(sq);
                ct = COL(t_sq);
                rs = ROW(sq);
                rt = ROW(t_sq);

                if ((pce == N || pce == n) && !((0 <= t_sq) && (t_sq <= 63) && (-2 <= (cs - ct)) && ((cs - ct) <= 2) && (-2 <= (rs - rt)) && ((rs - rt) <= 2)))
                {
                    continue;
                }

                if ((pce == K || pce == k) && !((0 <= t_sq) && (t_sq <= 63) && (-1 <= (cs - ct)) && ((cs - ct) <= 1) && (-1 <= (rs - rt)) && ((rs - rt) <= 1)))
                {
                    continue;
                }

                // BLACK ^ 1 == WHITE       WHITE ^ 1 == BLACK
                if (pos->pieces[t_sq] != e) {
                    if (PieceCol[pos->pieces[t_sq]] == (side ^ 1)) {
                        AddCaptureMove(pos, MOVE(sq, t_sq, pos->pieces[t_sq], e, 0), list);
                    }
                    continue;
                }
            }
        }

        pce = LoopNonSlidePce[pceIndex++];
    }
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

// board

void UpdateListsMaterial(S_BOARD* pos) {

    int piece, sq, index, colour;

    for (index = 0; index < BRD_SQ_NUM; ++index) {
        sq = index;
        piece = pos->pieces[index];
        if (piece != e) {
            colour = PieceCol[piece];

            if (PieceBig[piece] == TRUE) pos->bigPce[colour]++;
            if (PieceMin[piece] == TRUE) pos->minPce[colour]++;
            if (PieceMaj[piece] == TRUE) pos->majPce[colour]++;

            pos->material[colour] += PieceVal[piece];

            pos->pList[piece][pos->pceNum[piece]] = sq;
            pos->pceNum[piece]++;

            if (piece == K) pos->KingSq[WHITE] = sq;
            if (piece == k) pos->KingSq[BLACK] = sq;

            if (piece == P) {
                SETBIT(pos->pawns[WHITE], sq);
                SETBIT(pos->pawns[BOTH], sq);
            }
            else if (piece == p) {
                SETBIT(pos->pawns[BLACK], sq);
                SETBIT(pos->pawns[BOTH], sq);
            }
        }
    }
}

void ResetBoard(S_BOARD* pos);
U64 GeneratePosKey(const S_BOARD* pos);

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

void ResetBoard(S_BOARD* pos) {

    int index = 0;

    for (index = 0; index < 64; ++index) {
        pos->pieces[index] = e;
    }

    for (index = 0; index < 2; ++index) {
        pos->bigPce[index] = 0;
        pos->majPce[index] = 0;
        pos->minPce[index] = 0;
        pos->material[index] = 0;
    }

    for (index = 0; index < 3; ++index) {
        pos->pawns[index] = 0ULL;
    }

    for (index = 0; index < 13; ++index) {
        pos->pceNum[index] = 0;
    }

    pos->KingSq[WHITE] = pos->KingSq[BLACK] = no_sq;

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

void MirrorBoard(S_BOARD* pos) {

    int tempPiecesArray[64];
    int tempSide = pos->side ^ 1;
    int SwapPiece[13] = { e, p, n, b, r, q, k, P, N, B, R, Q, K };
    int tempCastlePerm = 0;
    int tempEnPas = no_sq;

    int sq;
    int tp;

    if (pos->castlePerm & WKCA) tempCastlePerm |= BKCA;
    if (pos->castlePerm & WQCA) tempCastlePerm |= BQCA;

    if (pos->castlePerm & BKCA) tempCastlePerm |= WKCA;
    if (pos->castlePerm & BQCA) tempCastlePerm |= WQCA;

    if (pos->enPas != no_sq) {
        tempEnPas = Mirror64[pos->enPas];
    }

    for (sq = 0; sq < 64; sq++) {
        tempPiecesArray[sq] = pos->pieces[Mirror64[sq]];
    }

    ResetBoard(pos);

    for (sq = 0; sq < 64; sq++) {
        tp = SwapPiece[tempPiecesArray[sq]];
        pos->pieces[sq] = tp;
    }

    pos->side = tempSide;
    pos->castlePerm = tempCastlePerm;
    pos->enPas = tempEnPas;

    pos->posKey = GeneratePosKey(pos);

    UpdateListsMaterial(pos);
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

int ProbePvTable(const S_BOARD* pos);

//Get the best pvline stored in the PvArray for a given depth but we may also get a result greater than the depth
int GetPvLine(const int depth, S_BOARD* pos) {

    int move = ProbePvTable(pos);
    int count = 0;

    while (move != NOMOVE && count < depth) {

        if (MoveExists(pos, move)) {
            MakeMove(pos, move);
            pos->PvArray[count++] = move;
        }
        else {
            break;
        }
        move = ProbePvTable(pos);
    }

    while (pos->ply > 0) {
        TakeMove(pos);
    }

    return count;

}

const int PvSize = 0x100000 * 128;

//This function is used to reset the Pvtable by setting everything to 0 and the clear the pvtable for use before the next search is applied
void ClearPvTable(S_PVTABLE* table) {

    S_PVENTRY* pvEntry;

    for (pvEntry = table->pTable; pvEntry < table->pTable + table->numEntries; pvEntry++) {
        pvEntry->posKey = 0ULL;
        pvEntry->move = NOMOVE;
    }
}

//This function is for initialising the count
void InitPvTable(S_PVTABLE* table) {

    table->numEntries = PvSize / sizeof(S_PVENTRY);
    table->numEntries -= 2;
    if (table->pTable != NULL) free(table->pTable);
    table->pTable = (S_PVENTRY*)malloc(table->numEntries * sizeof(S_PVENTRY));
    ClearPvTable(table);
    printf("PvTable init complete with %d entries\n", table->numEntries);

}

//To store a move in the pvtable using hashing function as poskey%count
void StorePvMove(const S_BOARD* pos, const int move) {

    int index = pos->posKey % pos->PvTable->numEntries;

    pos->PvTable->pTable[index].move = move;
    pos->PvTable->pTable[index].posKey = pos->posKey;
}

int ProbePvTable(const S_BOARD* pos) {

    int index = pos->posKey % pos->PvTable->numEntries;

    if (pos->PvTable->pTable[index].posKey == pos->posKey) {
        return pos->PvTable->pTable[index].move;
    }

    return NOMOVE;
}

//evaluate

const int PawnIsolated = -10;
const int PawnPassed[8] = { 0, 5, 10, 20, 35, 60, 100, 200 };
const int RookOpenFile = 10;
const int RookSemiOpenFile = 5;
const int QueenOpenFile = 5;
const int QueenSemiOpenFile = 3;
const int BishopPair = 30;

const int PawnTable[64] = {
0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
10	,	10	,	0	,	-10	,	-10	,	0	,	10	,	10	,
5	,	0	,	0	,	5	,	5	,	0	,	0	,	5	,
0	,	0	,	10	,	20	,	20	,	10	,	0	,	0	,
5	,	5	,	5	,	10	,	10	,	5	,	5	,	5	,
10	,	10	,	10	,	20	,	20	,	10	,	10	,	10	,
20	,	20	,	20	,	30	,	30	,	20	,	20	,	20	,
0	,	0	,	0	,	0	,	0	,	0	,	0	,	0
};

const int KnightTable[64] = {
0	,	-10	,	0	,	0	,	0	,	0	,	-10	,	0	,
0	,	0	,	0	,	5	,	5	,	0	,	0	,	0	,
0	,	0	,	10	,	10	,	10	,	10	,	0	,	0	,
0	,	0	,	10	,	20	,	20	,	10	,	5	,	0	,
5	,	10	,	15	,	20	,	20	,	15	,	10	,	5	,
5	,	10	,	10	,	20	,	20	,	10	,	10	,	5	,
0	,	0	,	5	,	10	,	10	,	5	,	0	,	0	,
0	,	0	,	0	,	0	,	0	,	0	,	0	,	0
};

const int BishopTable[64] = {
0	,	0	,	-10	,	0	,	0	,	-10	,	0	,	0	,
0	,	0	,	0	,	10	,	10	,	0	,	0	,	0	,
0	,	0	,	10	,	15	,	15	,	10	,	0	,	0	,
0	,	10	,	15	,	20	,	20	,	15	,	10	,	0	,
0	,	10	,	15	,	20	,	20	,	15	,	10	,	0	,
0	,	0	,	10	,	15	,	15	,	10	,	0	,	0	,
0	,	0	,	0	,	10	,	10	,	0	,	0	,	0	,
0	,	0	,	0	,	0	,	0	,	0	,	0	,	0
};

const int RookTable[64] = {
0	,	0	,	5	,	10	,	10	,	5	,	0	,	0	,
0	,	0	,	5	,	10	,	10	,	5	,	0	,	0	,
0	,	0	,	5	,	10	,	10	,	5	,	0	,	0	,
0	,	0	,	5	,	10	,	10	,	5	,	0	,	0	,
0	,	0	,	5	,	10	,	10	,	5	,	0	,	0	,
0	,	0	,	5	,	10	,	10	,	5	,	0	,	0	,
25	,	25	,	25	,	25	,	25	,	25	,	25	,	25	,
0	,	0	,	5	,	10	,	10	,	5	,	0	,	0
};

const int KingE[64] = {
    -50	,	-10	,	0	,	0	,	0	,	0	,	-10	,	-50	,
    -10,	0	,	10	,	10	,	10	,	10	,	0	,	-10	,
    0	,	10	,	20	,	20	,	20	,	20	,	10	,	0	,
    0	,	10	,	20	,	40	,	40	,	20	,	10	,	0	,
    0	,	10	,	20	,	40	,	40	,	20	,	10	,	0	,
    0	,	10	,	20	,	20	,	20	,	20	,	10	,	0	,
    -10,	0	,	10	,	10	,	10	,	10	,	0	,	-10	,
    -50	,	-10	,	0	,	0	,	0	,	0	,	-10	,	-50
};

const int KingO[64] = {
    0	,	5	,	5	,	-10	,	-10	,	0	,	10	,	5	,
    -30	,	-30	,	-30	,	-30	,	-30	,	-30	,	-30	,	-30	,
    -50	,	-50	,	-50	,	-50	,	-50	,	-50	,	-50	,	-50	,
    -70	,	-70	,	-70	,	-70	,	-70	,	-70	,	-70	,	-70	,
    -70	,	-70	,	-70	,	-70	,	-70	,	-70	,	-70	,	-70	,
    -70	,	-70	,	-70	,	-70	,	-70	,	-70	,	-70	,	-70	,
    -70	,	-70	,	-70	,	-70	,	-70	,	-70	,	-70	,	-70	,
    -70	,	-70	,	-70	,	-70	,	-70	,	-70	,	-70	,	-70
};

int MaterialDraw(const S_BOARD* pos) {
    if (!pos->pceNum[R] && !pos->pceNum[r] && !pos->pceNum[Q] && !pos->pceNum[q]) {
        if (!pos->pceNum[b] && !pos->pceNum[B]) {
            if (pos->pceNum[N] < 3 && pos->pceNum[n] < 3) { return TRUE; }
        }
        else if (!pos->pceNum[N] && !pos->pceNum[n]) {
            if (abs(pos->pceNum[B] - pos->pceNum[b]) < 2) { return TRUE; }
        }
        else if ((pos->pceNum[N] < 3 && !pos->pceNum[B]) || (pos->pceNum[B] == 1 && !pos->pceNum[N])) {
            if ((pos->pceNum[n] < 3 && !pos->pceNum[b]) || (pos->pceNum[b] == 1 && !pos->pceNum[n])) { return TRUE; }
        }
    }
    else if (!pos->pceNum[Q] && !pos->pceNum[q]) {
        if (pos->pceNum[R] == 1 && pos->pceNum[r] == 1) {
            if ((pos->pceNum[N] + pos->pceNum[B]) < 2 && (pos->pceNum[n] + pos->pceNum[b]) < 2) { return TRUE; }
        }
        else if (pos->pceNum[R] == 1 && !pos->pceNum[r]) {
            if ((pos->pceNum[N] + pos->pceNum[B] == 0) && (((pos->pceNum[n] + pos->pceNum[b]) == 1) || ((pos->pceNum[n] + pos->pceNum[b]) == 2))) { return TRUE; }
        }
        else if (pos->pceNum[r] == 1 && !pos->pceNum[R]) {
            if ((pos->pceNum[n] + pos->pceNum[b] == 0) && (((pos->pceNum[N] + pos->pceNum[B]) == 1) || ((pos->pceNum[N] + pos->pceNum[B]) == 2))) { return TRUE; }
        }
    }
    return FALSE;
}

#define ENDGAME_MAT (1 * PieceVal[R] + 2 * PieceVal[N] + 2 * PieceVal[P] + PieceVal[K])

int EvalPosition(const S_BOARD* pos) {

    int pce;
    int pceNum;
    int sq;
    int score = pos->material[WHITE] - pos->material[BLACK];

    if (!pos->pceNum[P] && !pos->pceNum[p] && MaterialDraw(pos) == TRUE) {
        return 0;
    }

    pce = P;
    for (pceNum = 0; pceNum < pos->pceNum[pce]; ++pceNum) {
        sq = pos->pList[pce][pceNum];
        score += PawnTable[sq];


        if ((IsolatedMask[sq] & pos->pawns[WHITE]) == 0) {
            score += PawnIsolated;
        }

        if ((WhitePassedMask[sq] & pos->pawns[BLACK]) == 0) {
            score += PawnPassed[RanksBrd[sq]];
        }

    }

    pce = p;
    for (pceNum = 0; pceNum < pos->pceNum[pce]; ++pceNum) {
        sq = pos->pList[pce][pceNum];
        score -= PawnTable[MIRROR64(sq)];

        if ((IsolatedMask[sq] & pos->pawns[BLACK]) == 0) {
            score -= PawnIsolated;
        }

        if ((BlackPassedMask[sq] & pos->pawns[WHITE]) == 0) {
            score -= PawnPassed[7 - RanksBrd[sq]];
        }
    }

    pce = N;
    for (pceNum = 0; pceNum < pos->pceNum[pce]; ++pceNum) {
        sq = pos->pList[pce][pceNum];
        score += KnightTable[sq];
    }

    pce = n;
    for (pceNum = 0; pceNum < pos->pceNum[pce]; ++pceNum) {
        sq = pos->pList[pce][pceNum];
        score -= KnightTable[MIRROR64(sq)];
    }

    pce = B;
    for (pceNum = 0; pceNum < pos->pceNum[pce]; ++pceNum) {
        sq = pos->pList[pce][pceNum];
        score += BishopTable[sq];
    }

    pce = b;
    for (pceNum = 0; pceNum < pos->pceNum[pce]; ++pceNum) {
        sq = pos->pList[pce][pceNum];
        score -= BishopTable[MIRROR64(sq)];
    }

    pce = R;
    for (pceNum = 0; pceNum < pos->pceNum[pce]; ++pceNum) {
        sq = pos->pList[pce][pceNum];
        score += RookTable[sq];
        if (!(pos->pawns[BOTH] & FileBBMask[FilesBrd[sq]])) {
            score += RookOpenFile;
        }
        else if (!(pos->pawns[WHITE] & FileBBMask[FilesBrd[sq]])) {
            score += RookSemiOpenFile;
        }
    }

    pce = r;
    for (pceNum = 0; pceNum < pos->pceNum[pce]; ++pceNum) {
        sq = pos->pList[pce][pceNum];
        score -= RookTable[MIRROR64(sq)];
        if (!(pos->pawns[BOTH] & FileBBMask[FilesBrd[sq]])) {
            score -= RookOpenFile;
        }
        else if (!(pos->pawns[BLACK] & FileBBMask[FilesBrd[sq]])) {
            score -= RookSemiOpenFile;
        }
    }

    pce = Q;
    for (pceNum = 0; pceNum < pos->pceNum[pce]; ++pceNum) {
        sq = pos->pList[pce][pceNum];
        if (!(pos->pawns[BOTH] & FileBBMask[FilesBrd[sq]])) {
            score += QueenOpenFile;
        }
        else if (!(pos->pawns[WHITE] & FileBBMask[FilesBrd[sq]])) {
            score += QueenSemiOpenFile;
        }
    }

    pce = q;
    for (pceNum = 0; pceNum < pos->pceNum[pce]; ++pceNum) {
        sq = pos->pList[pce][pceNum];
        if (!(pos->pawns[BOTH] & FileBBMask[FilesBrd[sq]])) {
            score -= QueenOpenFile;
        }
        else if (!(pos->pawns[BLACK] & FileBBMask[FilesBrd[sq]])) {
            score -= QueenSemiOpenFile;
        }
    }
    
    pce = K;
    sq = pos->pList[pce][0];

    if ((pos->material[BLACK] <= ENDGAME_MAT)) {
        score += KingE[sq];
    }
    else {
        score += KingO[sq];
    }

    pce = k;
    sq = pos->pList[pce][0];

    if ((pos->material[WHITE] <= ENDGAME_MAT)) {
        score -= KingE[MIRROR64(sq)];
    }
    else {
        score -= KingO[MIRROR64(sq)];
    }

    if (pos->pceNum[B] >= 2) score += BishopPair;
    if (pos->pceNum[b] >= 2) score -= BishopPair;

    if (pos->side == WHITE) {
        return score;
    }
    else {
        return -score;
    }
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

// validate

int SideValid(const int side) {
    return (side == WHITE || side == BLACK) ? 1 : 0;
}

int FileRankValid(const int fr) {
    return (fr >= 0 && fr <= 7) ? 1 : 0;
}

int PieceValidEmpty(const int pce) {
    return (pce >= e && pce <= k) ? 1 : 0;
}

int PieceValid(const int pce) {
    return (pce >= P && pce <= k) ? 1 : 0;
}


void MirrorEvalTest(S_BOARD* pos) {
    FILE* file;
    file = fopen("mirror.epd", "r");
    char lineIn[1024];
    int ev1 = 0; int ev2 = 0;
    int positions = 0;
    if (file == NULL) {
        printf("File Not Found\n");
        return;
    }
    else {
        while (fgets(lineIn, 1024, file) != NULL) {
            ParseFen(lineIn, pos);
            positions++;
            ev1 = EvalPosition(pos);
            MirrorBoard(pos);
            ev2 = EvalPosition(pos);

            if (ev1 != ev2) {
                printf("\n\n\n");
                ParseFen(lineIn, pos);
                PrintBoard(pos);
                MirrorBoard(pos);
                PrintBoard(pos);
                printf("\n\nMirror Fail:\n%s\n", lineIn);
                getchar();
                return;
            }

            if ((positions % 1000) == 0) {
                printf("position %d\n", positions);
            }

            memset(&lineIn[0], 0, sizeof(lineIn));
        }
    }
}

//search

#define infinite 30000
#define ISMATE (infinite - MAXDEPTH)

int rootDepth;

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

    ClearPvTable(pos->PvTable);
    pos->ply = 0;

    info->stopped = 0;
    info->nodes = 0;
    info->fh = 0;
    info->fhf = 0;
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
        return EvalPosition(pos);
    }

    int Score = EvalPosition(pos);

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
    int OldAlpha = alpha;
    int BestMove = NOMOVE;
    Score = -infinite;
    int PvMove = ProbePvTable(pos);

    if (PvMove != NOMOVE) {
        for (MoveNum = 0; MoveNum < list->count; ++MoveNum) {
            if (list->moves[MoveNum].move == PvMove) {
                list->moves[MoveNum].score = 2000000;
                break;
            }
        }
    }

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
            BestMove = list->moves[MoveNum].move;
        }
    }

    if (alpha != OldAlpha) {
        StorePvMove(pos, BestMove);
    }

    return alpha;
}

static int AlphaBeta(int alpha, int beta, int depth, S_BOARD* pos, S_SEARCHINFO* info) {

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
        return EvalPosition(pos);
    }

    int InCheck = SqAttacked(pos->KingSq[pos->side], pos->side ^ 1, pos);

    if (InCheck == TRUE) {
        depth++;
    }

    int Score = -infinite;

    S_MOVELIST list[1];
    GenerateAllMoves(pos, list);

    int MoveNum = 0;
    int Legal = 0;
    int OldAlpha = alpha;
    int BestMove = NOMOVE;
    int PvMove = ProbePvTable(pos);
    Score = -infinite;

    if (PvMove != NOMOVE) {
        for (MoveNum = 0; MoveNum < list->count; ++MoveNum) {
            if (list->moves[MoveNum].move == PvMove) {
                list->moves[MoveNum].score = 2000000;
                break;
            }
        }
    }

    for (MoveNum = 0; MoveNum < list->count; ++MoveNum) {

        PickNextMove(MoveNum, list);

        //if move is illegal, go to next one
        if (!MakeMove(pos, list->moves[MoveNum].move)) {
            continue;
        }

        //if move is legal
        Legal++;
        Score = -AlphaBeta(-beta, -alpha, depth - 1, pos, info);
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

                if (!(list->moves[MoveNum].move & MFLAGCAP)) {
                    pos->searchKillers[1][pos->ply] = pos->searchKillers[0][pos->ply];
                    pos->searchKillers[0][pos->ply] = list->moves[MoveNum].move;
                }

                return beta;
            }
            alpha = Score;
            BestMove = list->moves[MoveNum].move;
            if (!(list->moves[MoveNum].move & MFLAGCAP)) {
                pos->searchHistory[pos->pieces[FROMSQ(BestMove)]][TOSQ(BestMove)] += depth;
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
        StorePvMove(pos, BestMove);
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

    // iterative deepening
    for (currentDepth = 1; currentDepth <= info->depth; ++currentDepth) {
        // alpha	 beta
        rootDepth = currentDepth;
        bestScore = AlphaBeta(-infinite, infinite, currentDepth, pos, info);

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
        info->quit = FALSE;
        pos->PvTable->pTable = NULL;
        InitPvTable(pos->PvTable);

        //Perft results depth 3 - 8902, depth 4 - 197,281, depth 5 - 4,865,609, depth 6 - 119,060,324

        //En passant sq should be 16 not 40
        //ParseFen("rnbqkbnr/1ppppppp/8/p7/7P/8/PPPPPPP1/RNBQKBNR w KQkq a6 0 2", pos);
        //ParseFen("rnbqk1nr/pppp1ppp/4p3/8/8/b1P5/PPQPPPPP/RNBK1BNR b KQkq - 0 3", pos);
        // 119060076
        //ParseFen(START_FEN, pos);
        //193690690 -depth 5:
        //ParseFen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -", pos);
        //178633661 depth 7:
        //ParseFen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -", pos);
        //706045033 depth 6:
        //ParseFen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", pos);
        //89941194 depth 5:
        //ParseFen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", pos);
        //164,075,551 depth 5:
        ParseFen("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", pos);
        PerftTest(6, pos);
        while (1);
    }
    else
    {
        AllInit();

        S_BOARD pos[1];
        S_SEARCHINFO info[1];
        info->quit = FALSE;
        pos->PvTable->pTable = NULL;
        InitPvTable(pos->PvTable);
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

        free(pos->PvTable->pTable);
    }
    return 0;
}

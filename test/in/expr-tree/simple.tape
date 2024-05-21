[34mCT_BINOP_ADD[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:1][m a + b; [1m##[m  -- tok: ;
[34mCT_BINOP_ADD[m
|  [33m1[m
|  [34mCT_BINOP_MUL[m
|  |  [33m2[m
|  |  [33m3[m
[1m[test/in/expr-tree/simple:2][m 1+2 * 3; [1m##[m  -- tok: ;
[34mCT_BINOP_MUL[m
|  [34mCT_BINOP_ADD[m
|  |  [33m1[m
|  |  [33m2[m
|  [33m3[m
[1m[test/in/expr-tree/simple:3][m (1 + 2)*3; [1m##[m  -- tok: ;
[33m0[m
[1m[test/in/expr-tree/simple:4][m 0; [1m##[m  -- tok: ;
[34mCT_BINOP_CALL[m
|  [0mstrlen[m
|  [32m(1)[m
|  |  [36m"coucou"[m
[1m[test/in/expr-tree/simple:5][m strlen("coucou"); [1m##[m  -- tok: ;
[34mCT_BINOP_CALL[m
|  [34mCT_UNOP_PMEMBER[m
|  |  [0ma[m
|  |  ->b
|  [32m(1)[m
|  |  [34mCT_UNOP_PLUS[m
|  |  |  [33m0[m
[1m[test/in/expr-tree/simple:6][m a->b(+0); [1m##[m  -- tok: ;
[34mCT_BINOP_ADD[m
|  [34mCT_BINOP_ADD[m
|  |  [34mCT_BINOP_ADD[m
|  |  |  [0ma[m
|  |  |  [0mb[m
|  |  [0mc[m
|  [0md[m
[1m[test/in/expr-tree/simple:7][m a + b + c + d; [1m##[m  -- tok: ;
[34mCT_BINOP_ADD[m
|  [0ma[m
|  [34mCT_BINOP_DIV[m
|  |  [34mCT_BINOP_MUL[m
|  |  |  [0mb[m
|  |  |  [0mc[m
|  |  [0md[m
[1m[test/in/expr-tree/simple:8][m a + b * c / d; [1m##[m  -- tok: ;
[34mCT_BINOP_ASGN[m
|  [0ma[m
|  [34mCT_BINOP_ASGN[m
|  |  [0mb[m
|  |  [0mc[m
[1m[test/in/expr-tree/simple:9][m a = b = c; [1m##[m  -- tok: ;
[34mCT_BINOP_LOR[m
|  [34mCT_BINOP_EQ[m
|  |  [33m'_'[m
|  |  [0mc[m
|  [34mCT_BINOP_LAND[m
|  |  [34mCT_BINOP_LE[m
|  |  |  [33m'a'[m
|  |  |  [34mCT_BINOP_BOR[m
|  |  |  |  [0mc[m
|  |  |  |  [33m32[m
|  |  [34mCT_BINOP_GE[m
|  |  |  [33m'z'[m
|  |  |  [34mCT_BINOP_BOR[m
|  |  |  |  [0mc[m
|  |  |  |  [33m32[m
[1m[test/in/expr-tree/simple:11][m '_' == c || ('a' <= (c|32) && 'z' >= (c|32)); [1m##[m  -- tok: ;
[34mCT_BINOP_ASGN_BSHL[m
|  [0mn[m
|  [34mCT_BINOP_LT[m
|  |  [34mCT_BINOP_BSHR[m
|  |  |  [0mn[m
|  |  |  [33m1[m
|  |  [33m2[m
[1m[test/in/expr-tree/simple:12][m n<<= n>>1<2; [1m##[m  -- tok: ;
[34mCT_BINOP_ASGN_BSHR[m
|  [0mn[m
|  [34mCT_BINOP_GT[m
|  |  [34mCT_BINOP_BSHL[m
|  |  |  [0mn[m
|  |  |  [33m1[m
|  |  [33m2[m
[1m[test/in/expr-tree/simple:13][m n>>= n<<1>2; [1m##[m  -- tok: ;
[34mCT_BINOP_ASGN[m
|  [0mESC[m
|  [34mCT_BINOP_BXOR[m
|  |  [33m'['[m
|  |  [33m64[m
[1m[test/in/expr-tree/simple:14][m ESC = '['^64; [1m##[m  -- tok: ;
[34mCT_BINOP_TERNCOND[m
|  [34mCT_UNOP_LNOT[m
|  |  [34mCT_BINOP_BAND[m
|  |  |  [34mCT_BINOP_NE[m
|  |  |  |  [33m0[m
|  |  |  |  [34mCT_UNOP_PRE_DEC[m
|  |  |  |  |  [0mz[m
|  |  |  [33m1[m
|  [34mCT_BINOP_TERNBRANCH[m
|  |  [36m"no"[m
|  |  [36m"yes"[m
[1m[test/in/expr-tree/simple:17][m !(0 != --z&1) ? "no" : "yes"; [1m##[m  -- tok: ;
[34mCT_BINOP_ASGN_ADD[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:19][m a+= b ; [1m##[m  -- tok: ;
[34mCT_BINOP_ASGN_SUB[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:20][m a-= b ; [1m##[m  -- tok: ;
[34mCT_BINOP_ASGN_MUL[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:21][m a*= b ; [1m##[m  -- tok: ;
[34mCT_BINOP_ASGN_DIV[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:22][m a/= b ; [1m##[m  -- tok: ;
[34mCT_BINOP_ASGN_REM[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:23][m a%= b ; [1m##[m  -- tok: ;
[34mCT_BINOP_ASGN_BAND[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:24][m a&= b ; [1m##[m  -- tok: ;
[34mCT_BINOP_ASGN_BOR[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:25][m a|= b ; [1m##[m  -- tok: ;
[34mCT_BINOP_ASGN_BXOR[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:26][m a^= b ; [1m##[m  -- tok: ;
[34mCT_BINOP_COMMA[m
|  [34mCT_BINOP_ADD[m
|  |  [33m31[m
|  |  [33m17[m
|  [34mCT_BINOP_BOR[m
|  |  [0maaaaa[m
|  |  [0mx[m
[1m[test/in/expr-tree/simple:28][m 31 + 17, aaaaa|x; [1m##[m  -- tok: ;
[34mCT_UNOP_MINUS[m
|  [0ma[m
[1m[test/in/expr-tree/simple:30][m -(a); [1m##[m  -- tok: ;

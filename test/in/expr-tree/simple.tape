[34mBINOP_ADD[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:1][m a + b; [1m##[m  -- tok: ;
[34mBINOP_ADD[m
|  [33m1[m
|  [34mBINOP_MUL[m
|  |  [33m2[m
|  |  [33m3[m
[1m[test/in/expr-tree/simple:2][m 1+2 * 3; [1m##[m  -- tok: ;
[34mBINOP_MUL[m
|  [34mBINOP_ADD[m
|  |  [33m1[m
|  |  [33m2[m
|  [33m3[m
[1m[test/in/expr-tree/simple:3][m (1 + 2)*3; [1m##[m  -- tok: ;
[33m0[m
[1m[test/in/expr-tree/simple:4][m 0; [1m##[m  -- tok: ;
[34mBINOP_CALL[m
|  [0mstrlen[m
|  [32m(1)[m
|  |  [36m"coucou"[m
[1m[test/in/expr-tree/simple:5][m strlen("coucou"); [1m##[m  -- tok: ;
[34mBINOP_CALL[m
|  [34mUNOP_PMEMBER[m
|  |  [0ma[m
|  |  ->b
|  [32m(1)[m
|  |  [34mUNOP_PLUS[m
|  |  |  [33m0[m
[1m[test/in/expr-tree/simple:6][m a->b(+0); [1m##[m  -- tok: ;
[34mBINOP_ADD[m
|  [34mBINOP_ADD[m
|  |  [34mBINOP_ADD[m
|  |  |  [0ma[m
|  |  |  [0mb[m
|  |  [0mc[m
|  [0md[m
[1m[test/in/expr-tree/simple:7][m a + b + c + d; [1m##[m  -- tok: ;
[34mBINOP_ADD[m
|  [0ma[m
|  [34mBINOP_DIV[m
|  |  [34mBINOP_MUL[m
|  |  |  [0mb[m
|  |  |  [0mc[m
|  |  [0md[m
[1m[test/in/expr-tree/simple:8][m a + b * c / d; [1m##[m  -- tok: ;
[34mBINOP_ASGN[m
|  [0ma[m
|  [34mBINOP_ASGN[m
|  |  [0mb[m
|  |  [0mc[m
[1m[test/in/expr-tree/simple:9][m a = b = c; [1m##[m  -- tok: ;
[34mBINOP_LOR[m
|  [34mBINOP_EQ[m
|  |  [33m'_'[m
|  |  [0mc[m
|  [34mBINOP_LAND[m
|  |  [34mBINOP_LE[m
|  |  |  [33m'a'[m
|  |  |  [34mBINOP_BOR[m
|  |  |  |  [0mc[m
|  |  |  |  [33m32[m
|  |  [34mBINOP_GE[m
|  |  |  [33m'z'[m
|  |  |  [34mBINOP_BOR[m
|  |  |  |  [0mc[m
|  |  |  |  [33m32[m
[1m[test/in/expr-tree/simple:11][m '_' == c || ('a' <= (c|32) && 'z' >= (c|32)); [1m##[m  -- tok: ;
[34mBINOP_ASGN_BSHL[m
|  [0mn[m
|  [34mBINOP_LT[m
|  |  [34mBINOP_BSHR[m
|  |  |  [0mn[m
|  |  |  [33m1[m
|  |  [33m2[m
[1m[test/in/expr-tree/simple:12][m n<<= n>>1<2; [1m##[m  -- tok: ;
[34mBINOP_ASGN_BSHR[m
|  [0mn[m
|  [34mBINOP_GT[m
|  |  [34mBINOP_BSHL[m
|  |  |  [0mn[m
|  |  |  [33m1[m
|  |  [33m2[m
[1m[test/in/expr-tree/simple:13][m n>>= n<<1>2; [1m##[m  -- tok: ;
[34mBINOP_ASGN[m
|  [0mESC[m
|  [34mBINOP_BXOR[m
|  |  [33m'['[m
|  |  [33m64[m
[1m[test/in/expr-tree/simple:14][m ESC = '['^64; [1m##[m  -- tok: ;
[34mBINOP_TERNCOND[m
|  [34mUNOP_LNOT[m
|  |  [34mBINOP_BAND[m
|  |  |  [34mBINOP_NE[m
|  |  |  |  [33m0[m
|  |  |  |  [34mUNOP_PRE_DEC[m
|  |  |  |  |  [0mz[m
|  |  |  [33m1[m
|  [34mBINOP_TERNBRANCH[m
|  |  [36m"no"[m
|  |  [36m"yes"[m
[1m[test/in/expr-tree/simple:17][m !(0 != --z&1) ? "no" : "yes"; [1m##[m  -- tok: ;
[34mBINOP_ASGN_ADD[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:19][m a+= b ; [1m##[m  -- tok: ;
[34mBINOP_ASGN_SUB[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:20][m a-= b ; [1m##[m  -- tok: ;
[34mBINOP_ASGN_MUL[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:21][m a*= b ; [1m##[m  -- tok: ;
[34mBINOP_ASGN_DIV[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:22][m a/= b ; [1m##[m  -- tok: ;
[34mBINOP_ASGN_REM[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:23][m a%= b ; [1m##[m  -- tok: ;
[34mBINOP_ASGN_BAND[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:24][m a&= b ; [1m##[m  -- tok: ;
[34mBINOP_ASGN_BOR[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:25][m a|= b ; [1m##[m  -- tok: ;
[34mBINOP_ASGN_BXOR[m
|  [0ma[m
|  [0mb[m
[1m[test/in/expr-tree/simple:26][m a^= b ; [1m##[m  -- tok: ;
[34mUNOP_CAST[m
|  [32mint[m
|  [33m'a'[m
[1m[test/in/expr-tree/simple:28][m (int)'a'; [1m##[m  -- tok: ;
[34mUNOP_CAST[m
|  [34munsigned[m [34mlong[m [32mint[m
|  [34mUNOP_ADDR[m
|  |  [0mn[m
[1m[test/in/expr-tree/simple:29][m (unsigned long)&n; [1m##[m  -- tok: ;
[34mUNOP_CAST[m
|  [34mptr[m[[32mvoid[m]
|  [34mUNOP_DEREF[m
|  |  [0mp[m
[1m[test/in/expr-tree/simple:30][m (void*)*p; [1m##[m  -- tok: ;
[34mUNOP_CAST[m
|  [34mptr[m[[34mfun[m() -> [32mvoid[m]
|  [0mon[m
[1m[test/in/expr-tree/simple:31][m (void(*)())on; [1m##[m  -- tok: ;

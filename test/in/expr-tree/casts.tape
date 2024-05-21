[34mCT_UNOP_CAST[m
|  [32mint[m
|  [33m'a'[m
[1m[test/in/expr-tree/casts:1][m (int)'a'; [1m##[m  -- tok: ;
[34mCT_UNOP_CAST[m
|  [34munsigned[m [34mlong[m [32mint[m
|  [34mCT_UNOP_ADDR[m
|  |  [0mn[m
[1m[test/in/expr-tree/casts:2][m (unsigned long)&n; [1m##[m  -- tok: ;
[34mCT_UNOP_CAST[m
|  [34mptr[m[[32mvoid[m]
|  [34mCT_UNOP_DEREF[m
|  |  [0mp[m
[1m[test/in/expr-tree/casts:3][m (void*)*p; [1m##[m  -- tok: ;
[34mCT_UNOP_CAST[m
|  [34mptr[m[[34mfun[m() -> [32mvoid[m]
|  [0mon[m
[1m[test/in/expr-tree/casts:4][m (void(*)())on; [1m##[m  -- tok: ;
[34mCT_UNOP_CAST[m
|  [34munsigned[m [32mint[m
|  [33m1[m
[1m[test/in/expr-tree/casts:6][m (unsigned)1; [1m##[m  -- tok: ;
[34mCT_UNOP_CAST[m
|  [32mfloat[m
|  [33m.1[m
[1m[test/in/expr-tree/casts:7][m (float).1; [1m##[m  -- tok: ;
[34mCT_UNOP_CAST[m
|  [34mptr[m[[34mstruct[m buf]
|  [0mNULL[m
[1m[test/in/expr-tree/casts:8][m (struct buf*)NULL; [1m##[m  -- tok: ;
[34mCT_UNOP_CAST[m
|  [34mconst[m [32msize_t[m
|  [0ma[m
[1m[test/in/expr-tree/casts:10][m (size_t const)a; [1m##[m  -- tok: ;
[34mCT_UNOP_CAST[m
|  [32msize_t[m
|  [34mCT_UNOP_MINUS[m
|  |  [33m1[m
[1m[test/in/expr-tree/casts:12][m (size_t)-1; [1m##[m  -- tok: ;
[34mCT_UNOP_CAST[m
|  [32msize_t[m
|  [34mCT_BINOP_ADD[m
|  |  [0ma[m
|  |  [34mCT_BINOP_MUL[m
|  |  |  [0mb[m
|  |  |  [0mc[m
[1m[test/in/expr-tree/casts:13][m (size_t)(a+b*c); [1m##[m  -- tok: ;
[34mCT_UNOP_CAST[m
|  [32mbool[m
|  [34mCT_UNOP_LNOT[m
|  |  [0mp[m
[1m[test/in/expr-tree/casts:14][m (bool)!p; [1m##[m  -- tok: ;
[34mCT_UNOP_CAST[m
|  [34mptr[m[[32msize_t[m]
|  [0ma[m
[1m[test/in/expr-tree/casts:16][m (size_t*)a; [1m##[m  -- tok: ;
[34mCT_UNOP_CAST[m
|  [34mptr[m[[34mconst[m [34mptr[m[[34mptr[m[[32msize_t[m]]]
|  [0ma[m
[1m[test/in/expr-tree/casts:17][m (size_t** const*)a; [1m##[m  -- tok: ;
[34mCT_UNOP_CAST[m
|  [34mvolatile[m [34mptr[m[[32msize_t[m]
|  [0ma[m
[1m[test/in/expr-tree/casts:20][m (size_t* volatile)a; [1m##[m  -- tok: ;

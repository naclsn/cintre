[34mUNOP_CAST[m
|  [34munsigned[m [32mint[m
|  [33m1[m
[1m[test/in/expr-tree/casts:1][m (unsigned)1; [1m##[m  -- tok: ;
[34mUNOP_CAST[m
|  [32mfloat[m
|  [33m.1[m
[1m[test/in/expr-tree/casts:2][m (float).1; [1m##[m  -- tok: ;
[34mUNOP_CAST[m
|  [34mptr[m[[34mstruct[m buf]
|  [0mNULL[m
[1m[test/in/expr-tree/casts:3][m (struct buf*)NULL; [1m##[m  -- tok: ;
[34mUNOP_CAST[m
|  [34mconst[m [32msize_t[m
|  [0ma[m
[1m[test/in/expr-tree/casts:5][m (size_t const)a; [1m##[m  -- tok: ;
[34mUNOP_CAST[m
|  [32msize_t[m
|  [34mUNOP_MINUS[m
|  |  [33m1[m
[1m[test/in/expr-tree/casts:7][m (size_t)-1; [1m##[m  -- tok: ;
[34mUNOP_CAST[m
|  [32msize_t[m
|  [34mBINOP_ADD[m
|  |  [0ma[m
|  |  [34mBINOP_MUL[m
|  |  |  [0mb[m
|  |  |  [0mc[m
[1m[test/in/expr-tree/casts:8][m (size_t)(a+b*c); [1m##[m  -- tok: ;
[34mUNOP_CAST[m
|  [32mbool[m
|  [34mUNOP_LNOT[m
|  |  [0mp[m
[1m[test/in/expr-tree/casts:9][m (bool)!p; [1m##[m  -- tok: ;
[34mUNOP_CAST[m
|  [34mptr[m[[32msize_t[m]
|  [0ma[m
[1m[test/in/expr-tree/casts:11][m (size_t*)a; [1m##[m  -- tok: ;
[34mUNOP_CAST[m
|  [34mptr[m[[34mconst[m [34mptr[m[[34mptr[m[[32msize_t[m]]]
|  [0ma[m
[1m[test/in/expr-tree/casts:12][m (size_t** const*)a; [1m##[m  -- tok: ;
[34mUNOP_CAST[m
|  [34mvolatile[m [34mptr[m[[32msize_t[m]
|  [0ma[m
[1m[test/in/expr-tree/casts:15][m (size_t* volatile)a; [1m##[m  -- tok: ;

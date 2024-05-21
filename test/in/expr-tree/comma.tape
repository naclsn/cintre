[34mCT_BINOP_COMMA[m
|  [33m1[m
|  [33m2[m
[1m[test/in/expr-tree/comma:1][m 1, 2; // allowed 'top level' (expr stmt) [1m##[m  -- tok: ;
[34mCT_BINOP_CALL[m
|  [0ma[m
|  [32m(2)[m
|  |  [33m3[m
|  |  [33m4[m
[1m[test/in/expr-tree/comma:2][m a(3, 4); // disallowed in fn args [1m##[m  -- tok: ;
[34mCT_BINOP_CALL[m
|  [0ma[m
|  [32m(1)[m
|  |  [34mCT_BINOP_COMMA[m
|  |  |  [33m5[m
|  |  |  [33m6[m
[1m[test/in/expr-tree/comma:3][m a((5, 6)); // allowed in just par expr [1m##[m  -- tok: ;
[34mCT_BINOP_CALL[m
|  [0ma[m
|  [32m(1)[m
|  |  [34mCT_BINOP_SUBSCR[m
|  |  |  [0mb[m
|  |  |  [34mCT_BINOP_COMMA[m
|  |  |  |  [33m7[m
|  |  |  |  [33m8[m
[1m[test/in/expr-tree/comma:4][m a(b[7, 8]); // allowed in subscript off [1m##[m  -- tok: ;
[34mCT_BINOP_COMMA[m
|  [34mCT_BINOP_TERNCOND[m
|  |  [0ma[m
|  |  [34mCT_BINOP_TERNBRANCH[m
|  |  |  [0mb[m
|  |  |  [0mc[m
|  [0md[m
[1m[test/in/expr-tree/comma:5][m a ? b : c, d; // disallowed in conditional alt branch [1m##[m  -- tok: ;
[34mCT_BINOP_COMMA[m
|  [0mz[m
|  [34mCT_BINOP_COMMA[m
|  |  [34mCT_BINOP_TERNCOND[m
|  |  |  [0ma[m
|  |  |  [34mCT_BINOP_TERNBRANCH[m
|  |  |  |  [34mCT_BINOP_COMMA[m
|  |  |  |  |  [0mb[m
|  |  |  |  |  [34mCT_BINOP_COMMA[m
|  |  |  |  |  |  [0my[m
|  |  |  |  |  |  [0mw[m
|  |  |  |  [0mc[m
|  |  [0md[m
[1m[test/in/expr-tree/comma:6][m z, a ? b, y, w : c, d; // (A, B, C) with B the ?: [1m##[m  -- tok: ;
[34mCT_BINOP_COMMA[m
|  [33m1[m
|  [34mCT_BINOP_COMMA[m
|  |  [33m2[m
|  |  [34mCT_BINOP_COMMA[m
|  |  |  [33m3[m
|  |  |  [0msoleil[m
[1m[test/in/expr-tree/comma:7][m 1, 2, 3, soleil; // associativity ended up being right-to-left which is technically wrong but doesn matter enough for it to be corrected [1m##[m  -- tok: ;

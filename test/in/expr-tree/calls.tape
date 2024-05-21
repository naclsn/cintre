[34mCT_BINOP_CALL[m
|  [0ma[m
|  [32m(2)[m
|  |  [33m1[m
|  |  [33m2[m
[1m[test/in/expr-tree/calls:1][m a(1, 2); [1m##[m  -- tok: ;
[34mCT_BINOP_CALL[m
|  [0ma[m
|  [32m(3)[m
|  |  [33m1[m
|  |  [33m2[m
|  |  [33m3[m
[1m[test/in/expr-tree/calls:2][m a(1, 2, 3); [1m##[m  -- tok: ;
[34mCT_BINOP_CALL[m
|  [0ma[m
|  [32m(0)[m
[1m[test/in/expr-tree/calls:3][m a(); [1m##[m  -- tok: ;
[34mCT_BINOP_CALL[m
|  [0ma[m
|  [32m(1)[m
|  |  [34mCT_BINOP_CALL[m
|  |  |  [0mb[m
|  |  |  [32m(2)[m
|  |  |  |  [33m1[m
|  |  |  |  [33m2[m
[1m[test/in/expr-tree/calls:4][m a(b(1, 2)); [1m##[m  -- tok: ;
[34mCT_BINOP_CALL[m
|  [0ma[m
|  [32m(2)[m
|  |  [33m0[m
|  |  [34mCT_BINOP_CALL[m
|  |  |  [0mb[m
|  |  |  [32m(2)[m
|  |  |  |  [33m1[m
|  |  |  |  [33m2[m
[1m[test/in/expr-tree/calls:5][m a(0, b(1, 2)); [1m##[m  -- tok: ;
[34mCT_BINOP_CALL[m
|  [0ma[m
|  [32m(2)[m
|  |  [34mCT_BINOP_CALL[m
|  |  |  [0mb[m
|  |  |  [32m(2)[m
|  |  |  |  [33m1[m
|  |  |  |  [33m2[m
|  |  [34mCT_BINOP_CALL[m
|  |  |  [0mc[m
|  |  |  [32m(2)[m
|  |  |  |  [33m3[m
|  |  |  |  [33m4[m
[1m[test/in/expr-tree/calls:6][m a(b(1, 2), c(3, 4)); [1m##[m  -- tok: ;
[34mCT_BINOP_CALL[m
|  [0ma[m
|  [32m(2)[m
|  |  [34mCT_BINOP_CALL[m
|  |  |  [0mc[m
|  |  |  [32m(2)[m
|  |  |  |  [33m3[m
|  |  |  |  [33m4[m
|  |  [33m0[m
[1m[test/in/expr-tree/calls:7][m a(c(3, 4), 0); [1m##[m  -- tok: ;
[34mCT_BINOP_COMMA[m
|  [34mCT_BINOP_CALL[m
|  |  [0ma[m
|  |  [32m(1)[m
|  |  |  [33m1[m
|  [34mCT_UNOP_MINUS[m
|  |  [33m1[m
[1m[test/in/expr-tree/calls:8][m a(1), -1; [1m##[m  -- tok: ;
[34mCT_BINOP_CALL[m
|  [34mCT_BINOP_CALL[m
|  |  [0ma[m
|  |  [32m(0)[m
|  [32m(0)[m
[1m[test/in/expr-tree/calls:9][m (a())(); [1m##[m  -- tok: ;
[34mCT_BINOP_CALL[m
|  [34mCT_BINOP_CALL[m
|  |  [0ma[m
|  |  [32m(0)[m
|  [32m(0)[m
[1m[test/in/expr-tree/calls:10][m a()(); [1m##[m  -- tok: ;
[34mCT_BINOP_CALL[m
|  [34mCT_BINOP_CALL[m
|  |  [34mCT_BINOP_CALL[m
|  |  |  [0ma[m
|  |  |  [32m(1)[m
|  |  |  |  [33m1[m
|  |  [32m(1)[m
|  |  |  [33m2[m
|  [32m(1)[m
|  |  [33m3[m
[1m[test/in/expr-tree/calls:11][m a(1)(2)(3); [1m##[m  -- tok: ;
[34mCT_BINOP_CALL[m
|  [0ma[m
|  [32m(2)[m
|  |  [34mCT_BINOP_COMMA[m
|  |  |  [33m1[m
|  |  |  [33m2[m
|  |  [33m3[m
[1m[test/in/expr-tree/calls:12][m a((1, 2), 3); [1m##[m  -- tok: ;

[34mBINOP_TERNCOND[m
|  [33m1[m
|  [34mBINOP_TERNBRANCH[m
|  |  [33m2[m
|  |  [33m3[m
[1m[test/in/expr-tree/conds:1][m 1 ? 2 : 3; [1m##[m  -- tok: ;
[34mBINOP_TERNCOND[m
|  [33m1[m
|  [34mBINOP_TERNBRANCH[m
|  |  [33m2[m
|  |  [34mBINOP_TERNCOND[m
|  |  |  [33m3[m
|  |  |  [34mBINOP_TERNBRANCH[m
|  |  |  |  [33m4[m
|  |  |  |  [33m5[m
[1m[test/in/expr-tree/conds:2][m 1 ? 2 : 3 ? 4 : 5; [1m##[m  -- tok: ;
[34mBINOP_TERNCOND[m
|  [0ma[m
|  [34mBINOP_TERNBRANCH[m
|  |  [34mBINOP_COMMA[m
|  |  |  [0mb[m
|  |  |  [0mc[m
|  |  [34mBINOP_COMMA[m
|  |  |  [0md[m
|  |  |  [0me[m
[1m[test/in/expr-tree/conds:3][m a ? (b, c) : (d, e); [1m##[m  -- tok: ;
[34mBINOP_TERNCOND[m
|  [0ma[m
|  [34mBINOP_TERNBRANCH[m
|  |  [34mBINOP_COMMA[m
|  |  |  [0mb[m
|  |  |  [0mc[m
|  |  [0md[m
[1m[test/in/expr-tree/conds:4][m a ? b, c : d; [1m##[m  -- tok: ;
[34mBINOP_COMMA[m
|  [34mBINOP_TERNCOND[m
|  |  [0ma[m
|  |  [34mBINOP_TERNBRANCH[m
|  |  |  [34mBINOP_COMMA[m
|  |  |  |  [0mb[m
|  |  |  |  [0mc[m
|  |  |  [0md[m
|  [0me[m
[1m[test/in/expr-tree/conds:5][m a ? b, c : d, e; [1m##[m  -- tok: ;
[34mBINOP_TERNCOND[m
|  [33m1[m
|  [34mBINOP_TERNBRANCH[m
|  |  [36m"one"[m
|  |  [34mBINOP_TERNCOND[m
|  |  |  [33m2[m
|  |  |  [34mBINOP_TERNBRANCH[m
|  |  |  |  [36m"two"[m
|  |  |  |  [34mBINOP_TERNCOND[m
|  |  |  |  |  [33m3[m
|  |  |  |  |  [34mBINOP_TERNBRANCH[m
|  |  |  |  |  |  [36m"tree"[m
|  |  |  |  |  |  [36m"sun"[m
[1m[test/in/expr-tree/conds:6][m 1 ? "one" : 2 ? "two" : 3 ? "tree" : "sun"; [1m##[m  -- tok: ;
[34mBINOP_ASGN[m
|  [0ma[m
|  [34mBINOP_TERNCOND[m
|  |  [33m1[m
|  |  [34mBINOP_TERNBRANCH[m
|  |  |  [33m2[m
|  |  |  [33m3[m
[1m[test/in/expr-tree/conds:7][m a = 1 ? 2 : 3; [1m##[m  -- tok: ;

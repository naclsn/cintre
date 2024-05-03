[34mBINOP_ASGN[m
|  [34mBINOP_SUBSCR[m
|  |  [0mptr[m
|  |  [34mBINOP_ASGN_ADD[m
|  |  |  [0mlen[m
|  |  |  [33m4[m
|  [33m'\0'[m
[1m[test/in/expr-tree/weird:1][m ptr[len+= 4] = '\0'; [1m##[m  -- tok: ;
[34mUNOP_MINUS[m
|  [34mUNOP_BNOT[m
|  |  [34mUNOP_ADDR[m
|  |  |  [34mUNOP_DEREF[m
|  |  |  |  [34mUNOP_PRE_INC[m
|  |  |  |  |  [34mUNOP_POST_DEC[m
|  |  |  |  |  |  [34mBINOP_CALL[m
|  |  |  |  |  |  |  [34mBINOP_SUBSCR[m
|  |  |  |  |  |  |  |  [0mx[m
|  |  |  |  |  |  |  |  [34mBINOP_COMMA[m
|  |  |  |  |  |  |  |  |  [0ma[m
|  |  |  |  |  |  |  |  |  [0mb[m
|  |  |  |  |  |  |  [32m(1)[m
|  |  |  |  |  |  |  |  [34mBINOP_REM[m
|  |  |  |  |  |  |  |  |  [0my[m
|  |  |  |  |  |  |  |  |  [0mz[m
[1m[test/in/expr-tree/weird:2][m -~&*++x[a, b](y%z)--; [1m##[m  -- tok: ;
[34mUNOP_ADDR[m
|  [34mUNOP_MEMBER[m
|  |  [34mBINOP_SUBSCR[m
|  |  |  [0mx[m
|  |  |  [34mUNOP_PMEMBER[m
|  |  |  |  [34mUNOP_POST_INC[m
|  |  |  |  |  [0ma[m
|  |  |  |  ->b
|  |  .c
[1m[test/in/expr-tree/weird:3][m &x[(a++)->b].c; [1m##[m  -- tok: ;

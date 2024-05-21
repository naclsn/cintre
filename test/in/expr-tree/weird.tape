[34mCT_BINOP_ASGN[m
|  [34mCT_BINOP_SUBSCR[m
|  |  [0mptr[m
|  |  [34mCT_BINOP_ASGN_ADD[m
|  |  |  [0mlen[m
|  |  |  [33m4[m
|  [33m'\0'[m
[1m[test/in/expr-tree/weird:1][m ptr[len+= 4] = '\0'; [1m##[m  -- tok: ;
[34mCT_UNOP_MINUS[m
|  [34mCT_UNOP_BNOT[m
|  |  [34mCT_UNOP_ADDR[m
|  |  |  [34mCT_UNOP_DEREF[m
|  |  |  |  [34mCT_UNOP_PRE_INC[m
|  |  |  |  |  [34mCT_UNOP_POST_DEC[m
|  |  |  |  |  |  [34mCT_BINOP_CALL[m
|  |  |  |  |  |  |  [34mCT_BINOP_SUBSCR[m
|  |  |  |  |  |  |  |  [0mx[m
|  |  |  |  |  |  |  |  [34mCT_BINOP_COMMA[m
|  |  |  |  |  |  |  |  |  [0ma[m
|  |  |  |  |  |  |  |  |  [0mb[m
|  |  |  |  |  |  |  [32m(1)[m
|  |  |  |  |  |  |  |  [34mCT_BINOP_REM[m
|  |  |  |  |  |  |  |  |  [0my[m
|  |  |  |  |  |  |  |  |  [0mz[m
[1m[test/in/expr-tree/weird:2][m -~&*++x[a, b](y%z)--; [1m##[m  -- tok: ;
[34mCT_UNOP_ADDR[m
|  [34mCT_UNOP_MEMBER[m
|  |  [34mCT_BINOP_SUBSCR[m
|  |  |  [0mx[m
|  |  |  [34mCT_UNOP_PMEMBER[m
|  |  |  |  [34mCT_UNOP_POST_INC[m
|  |  |  |  |  [0ma[m
|  |  |  |  ->b
|  |  .c
[1m[test/in/expr-tree/weird:3][m &x[(a++)->b].c; [1m##[m  -- tok: ;

[34mfor[m (;;)
    /*-*/;
[1m[test/in/stmt-graph/for:2][m for (1; 2; 3); [1m##[m  -- tok: for
[34mfor[m (1; 2; 3)
    /*-*/;
[1m[test/in/stmt-graph/for:3][m for (; 2; 3); [1m##[m  -- tok: for
[34mfor[m (; 2; 3)
    /*-*/;
[1m[test/in/stmt-graph/for:4][m for (1; ; 3); [1m##[m  -- tok: for
[34mfor[m (1;; 3)
    /*-*/;
[1m[test/in/stmt-graph/for:5][m for (1; 2; ); [1m##[m  -- tok: for
[34mfor[m (1; 2;)
    /*-*/;
[1m[test/in/stmt-graph/for:6][m for (1; ; ); [1m##[m  -- tok: for
[34mfor[m (1;;)
    /*-*/;
[1m[test/in/stmt-graph/for:7][m for (; 2; ); [1m##[m  -- tok: for
[34mfor[m (; 2;)
    /*-*/;
[1m[test/in/stmt-graph/for:8][m for (; ; 3); [1m##[m  -- tok: for
[34mfor[m (;; 3)
    /*-*/;
[1m[test/in/stmt-graph/for:9][m for (int k = 0; k < 42; ++k) puts("HI :3"); [1m##[m  -- tok: for
[34mfor[m (k: [32mint[m = 0; k < 42; ++k)
    puts("HI :3");
[1m[test/in/stmt-graph/for:9][m for (int k = 0; k < 42; ++k) puts("HI :3"); [1m##[m  -- tok: 

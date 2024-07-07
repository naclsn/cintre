[34mfor[m (;;)
    /*-*/;
[1m[test/in/stmt-graph/for:2][m for (1; 2; 3); [1m##[m  -- tok: for
[34mfor[m ((1); (2); (3))
    /*-*/;
[1m[test/in/stmt-graph/for:3][m for (int k = 0; k < 42; ++k) puts("HI :3"); [1m##[m  -- tok: for
[34mfor[m (k: [32mint[m = (0); ((k) < (42)); (++(k)))
    ((puts)(("HI :3")));
[1m[test/in/stmt-graph/for:3][m for (int k = 0; k < 42; ++k) puts("HI :3"); [1m##[m  -- tok: 

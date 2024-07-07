[34mwhile[m ((1))
    /*-*/;
[1m[test/in/stmt-graph/whiles:3][m do sure: ; [1m##[m  -- tok: do
[34mdo[m
[36msure[m:
    /*-*/;
[34mwhile[m ((0));
[1m[test/in/stmt-graph/whiles:6][m do ok: while (0); [1m##[m  -- tok: do
[34mdo[m
[36mok[m:
    [34mwhile[m ((0))
        /*-*/;
[34mwhile[m ((1));
[1m[test/in/stmt-graph/whiles:9][m while (0) whynot: do; [1m##[m  -- tok: while
[34mwhile[m ((0))
[36mwhynot[m:
    [34mdo[m
        /*-*/;
    [34mwhile[m ((1));
[1m[test/in/stmt-graph/whiles:10][m while (1); [1m##[m  -- tok: 

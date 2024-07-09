[34mif[m (1 == k)
    /*-*/;
[1m[test/in/stmt-graph/ifelse:2][m if (2 == k); else; [1m##[m  -- tok: if
[34mif[m (2 == k)
    /*-*/;
[34melse[m
    /*-*/;
[1m[test/in/stmt-graph/ifelse:3][m if (3 == k); else if (3 == k); [1m##[m  -- tok: if
[34mif[m (3 == k)
    /*-*/;
[34melse[m
    [34mif[m (3 == k)
        /*-*/;
[1m[test/in/stmt-graph/ifelse:5][m if ("one") one: ; else [1m##[m  -- tok: if
[34mif[m ("one")
[36mone[m:
    /*-*/;
[34melse[m
    [34mif[m ("two")
[36mtwo[m:
        /*-*/;
    [34melse[m
        [34mif[m ("three")
[36mthree[m:
            /*-*/;
        [34melse[m
[36mmore[m:
            /*-*/;
[1m[test/in/stmt-graph/ifelse:10][m maybe_one: if ("one") ; else [1m##[m  -- tok: maybe_one
[36mmaybe_one[m:
[34mif[m ("one")
    /*-*/;
[34melse[m
[36mmaybe_two[m:
    [34mif[m ("two")
        /*-*/;
    [34melse[m
[36mmaybe_three[m:
        [34mif[m ("three")
            /*-*/;
        [34melse[m
[36mmore[m:
            /*-*/;
[1m[test/in/stmt-graph/ifelse:15][m if (1) [1m##[m  -- tok: if
[34mif[m (1)
    [34mif[m (2)
        /*-*/;
    [34melse[m
        /*-*/;
[34melse[m
    /*-*/;
[1m[test/in/stmt-graph/ifelse:18][m     else ; [1m##[m  -- tok: 

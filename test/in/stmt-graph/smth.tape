/*-*/;
[1m[test/in/stmt-graph/smth:3][m {} [1m##[m  -- tok: {
{
}
[1m[test/in/stmt-graph/smth:4][m {;} [1m##[m  -- tok: {
{
    /*-*/;
}
[1m[test/in/stmt-graph/smth:5][m {coucou:;{}}; [1m##[m  -- tok: {
{
    /*-*/;
    {
    }
}
[1m[test/in/stmt-graph/smth:5][m {coucou:;{}}; [1m##[m  -- tok: ;
/*-*/;
[1m[test/in/stmt-graph/smth:6][m if(0) {;} else if(1); [1m##[m  -- tok: if
[34mif[m ((0))
    {
        /*-*/;
    }
[34melse[m
    [34mif[m ((1))
        /*-*/;
[1m[test/in/stmt-graph/smth:6][m if(0) {;} else if(1); [1m##[m  -- tok: 

/*-*/;
[1m[test/in/stmt-graph/other:2][m {} [1m##[m  -- tok: {
{
}
[1m[test/in/stmt-graph/other:3][m 0; [1m##[m  -- tok: 0
0;
[1m[test/in/stmt-graph/other:4][m 1, 2; [1m##[m  -- tok: 1
1, 2;
[1m[test/in/stmt-graph/other:5][m {;{};} [1m##[m  -- tok: {
{
    /*-*/;
    {
    }
    /*-*/;
}
[1m[test/in/stmt-graph/other:6][m char a; [1m##[m  -- tok: char
a: [32mchar[m;
[1m[test/in/stmt-graph/other:7][m int b = 0; [1m##[m  -- tok: int
b: [32mint[m = 0;
[1m[test/in/stmt-graph/other:8][m float x, y; [1m##[m  -- tok: float
x: [32mfloat[m;
y: [32mfloat[m;
[1m[test/in/stmt-graph/other:9][m char a = 'b', c = 'd', e = b+d; [1m##[m  -- tok: char
a: [32mchar[m = 'b';
c: [32mchar[m = 'd';
e: [32mchar[m = b+d;
[1m[test/in/stmt-graph/other:9][m char a = 'b', c = 'd', e = b+d; [1m##[m  -- tok: 

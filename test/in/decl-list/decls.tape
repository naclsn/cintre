a: [34mconst[m [32mint[m
[1m[test/in/decl-list/decls:2][m int const a; [1m##[m  -- tok: ;
ld: [34mcomplex[m [34mlong[m [32mdouble[m
[1m[test/in/decl-list/decls:3][m complex long double ld; [1m##[m  -- tok: ;
ar: [34marr[m[(32), [32mint[m]
[1m[test/in/decl-list/decls:4][m int ar[32], sfdsbbfsbdfb; [1m##[m  -- tok: ,
sfdsbbfsbdfb: [32mint[m
[1m[test/in/decl-list/decls:4][m int ar[32], sfdsbbfsbdfb; [1m##[m  -- tok: ;
ppp: [34mptr[m[[34mptr[m[[32mchar[m]]
[1m[test/in/decl-list/decls:5][m char** ppp; [1m##[m  -- tok: ;
y: [32mchar[m
[1m[test/in/decl-list/decls:6][m char (y); [1m##[m  -- tok: ;
yy: [34mconst[m [34mptr[m[[32mchar[m]
[1m[test/in/decl-list/decls:7][m char (*const yy); [1m##[m  -- tok: ;
yyy: [34mptr[m[[34mfun[m([36mvoid[m) -> [32mchar[m]
[1m[test/in/decl-list/decls:8][m char (*yyy)(void); [1m##[m  -- tok: ;
yyyy: [34mptr[m[[34marr[m[(2), [32mchar[m]]
[1m[test/in/decl-list/decls:9][m char (*yyyy)[2]; [1m##[m  -- tok: ;
yyyyy: [34marr[m[(5), [34mptr[m[[32mchar[m]]
[1m[test/in/decl-list/decls:10][m char (*yyyyy[5]); [1m##[m  -- tok: ;
v: [34msigned[m [32mint[m
[1m[test/in/decl-list/decls:11][m signed v; [1m##[m  -- tok: ;
[36mtypedef[m b: [32mfloat[m
[1m[test/in/decl-list/decls:12][m typedef float b; [1m##[m  -- tok: ;
c: [34mlong[m [34mlong[m [32mint[m
[1m[test/in/decl-list/decls:13][m long long c, * const d, e; [1m##[m  -- tok: ,
d: [34mconst[m [34mptr[m[[34mlong[m [34mlong[m [32mint[m]
[1m[test/in/decl-list/decls:13][m long long c, * const d, e; [1m##[m  -- tok: ,
e: [34mlong[m [34mlong[m [32mint[m
[1m[test/in/decl-list/decls:13][m long long c, * const d, e; [1m##[m  -- tok: ;
f: [34mfun[m() -> [32mvoid[m
[1m[test/in/decl-list/decls:14][m void f(); [1m##[m  -- tok: ;
g: [34mfun[m([36mvoid[m) -> [32mvoid[m
[1m[test/in/decl-list/decls:15][m void g(void); [1m##[m  -- tok: ;
[36mstatic[m aa: [34mfun[m() -> [34mptr[m[[32mint[m]
[1m[test/in/decl-list/decls:16][m static int * aa ( ) , bb ; [1m##[m  -- tok: ,
[36mstatic[m bb: [32mint[m
[1m[test/in/decl-list/decls:16][m static int * aa ( ) , bb ; [1m##[m  -- tok: ;
laksdjf: [34mfun[m(c: [32mchar[m, i: [32mint[m, f: [32mfloat[m, d: [32mdouble[m) -> [34munsigned[m [32mint[m
[1m[test/in/decl-list/decls:17][m unsigned laksdjf(char c, int i, float f, double d); [1m##[m  -- tok: ;
x: [34mstruct[m S
[1m[test/in/decl-list/decls:18][m struct S x; [1m##[m  -- tok: ;
z: [34msigned[m [32mint[m
[1m[test/in/decl-list/decls:19][m signed z; [1m##[m  -- tok: ;
fst: [34menum[m E1 {ONE, TWO= (3)}
[1m[test/in/decl-list/decls:20][m enum E1 {ONE, TWO =3 } fst, lsdkfjsldf; [1m##[m  -- tok: ,
lsdkfjsldf: [34menum[m E1 {ONE, TWO= (3)}
[1m[test/in/decl-list/decls:20][m enum E1 {ONE, TWO =3 } fst, lsdkfjsldf; [1m##[m  -- tok: ;
sec: [34menum[m {ONN, TWW}
[1m[test/in/decl-list/decls:21][m enum {ONN, TWW, } sec; [1m##[m  -- tok: ;
trd: [34menum[m E3 {}
[1m[test/in/decl-list/decls:22][m enum E3 trd; [1m##[m  -- tok: ;
[36mstatic[m crap: [34mfun[m(: [32mint[m, : [34mlong[m [32mint[m, : [34mshort[m [32mint[m) -> [32mint[m
[1m[test/in/decl-list/decls:23][m static int crap(int, long, short); [1m##[m  -- tok: ;
ddd: [34mptr[m[[34mptr[m[[34mfun[m() -> [32mvoid[m]]
[1m[test/in/decl-list/decls:24][m void (**ddd)(); [1m##[m  -- tok: ;
b: [32msome_t[m
[1m[test/in/decl-list/decls:25][m some_t b; [1m##[m  -- tok: ;
v: [34menum[m E {A, B, C}
[1m[test/in/decl-list/decls:26][m enum E { A, B, C } v; [1m##[m  -- tok: ;
w: [34mstruct[m S {a: [32mint[m, b: [32mint[m, c: [32mint[m}
[1m[test/in/decl-list/decls:27][m struct S { int a, b, c; } w; [1m##[m  -- tok: ;
b: [32mint[m
[1m[test/in/decl-list/decls:28][m int b; [1m##[m  -- tok: ;
fff: [34mstruct[m {}
[1m[test/in/decl-list/decls:29][m struct { } fff; [1m##[m  -- tok: ;
outer: [34mstruct[m {middle: [34mstruct[m {inner: [32mint[m}}
[1m[test/in/decl-list/decls:30][m struct { struct { int inner; } middle; } outer; [1m##[m  -- tok: ;
a: [32mint[m
[1m[test/in/decl-list/decls:33][m int a; [1m##[m  -- tok: ;
[36mstatic[m [34minline[m b: [34mfun[m(: [34mconst[m [34mrestrict[m [34mvolatile[m [34marr[m[[36mstatic[m (1), [34mconst[m [34mrestrict[m [34mvolatile[m [34mptr[m[[34mconst[m [34munsigned[m [34mlong[m [34mlong[m [32mint[m]]) -> [34mconst[m [34mrestrict[m [34mvolatile[m [34mptr[m[[34mconst[m [34mvolatile[m [34munsigned[m [34mlong[m [34mlong[m [32mint[m]
[1m[test/in/decl-list/decls:34][m static const volatile inline unsigned long long int* const restrict volatile b(const unsigned long long int* const restrict volatile[static const restrict volatile 1]); [1m##[m  -- tok: ;
c: [32mint[m
[1m[test/in/decl-list/decls:35][m int c, d(void), e[1][2][3]; [1m##[m  -- tok: ,
d: [34mfun[m([36mvoid[m) -> [32mint[m
[1m[test/in/decl-list/decls:35][m int c, d(void), e[1][2][3]; [1m##[m  -- tok: ,
e: [34marr[m[(3), [34marr[m[(2), [34marr[m[(1), [32mint[m]]]
[1m[test/in/decl-list/decls:35][m int c, d(void), e[1][2][3]; [1m##[m  -- tok: ;
f: [32mint[m
[1m[test/in/decl-list/decls:36][m int (f); [1m##[m  -- tok: ;
g: [34mfun[m() -> [32mvoid[m
[1m[test/in/decl-list/decls:37][m void (g()) {} [1m##[m  -- tok: {
h: [34mfun[m(: [32mint[m) -> [34mptr[m[[34mfun[m([36mvoid[m) -> [32mint[m]
[1m[test/in/decl-list/decls:38][m int (* h(int) )(void); [1m##[m  -- tok: ;
: [34mstruct[m A {x: [32mint[m, y: [32mint[m :(1), z: [32mint[m :(3), t: [32mint[m}
[1m[test/in/decl-list/decls:39][m struct A { int x, y :1; int z :3, t; }; [1m##[m  -- tok: ;
[36mtypedef[m B: [34munion[m {}
[1m[test/in/decl-list/decls:40][m typedef union {} B, *pB; [1m##[m  -- tok: ,
[36mtypedef[m pB: [34mptr[m[[34munion[m {}]
[1m[test/in/decl-list/decls:40][m typedef union {} B, *pB; [1m##[m  -- tok: ;
i: [34menum[m {X, Y= (0), Z}
[1m[test/in/decl-list/decls:41][m enum { X, Y= 0, Z, } i; [1m##[m  -- tok: ;
begin: [32mint[m
[1m[test/in/decl-list/decls:44][m int begin; [1m##[m  -- tok: ;
[36mstatic[m aa: [34mstruct[m {a: [32mint[m, b: [32mint[m, c: [34mptr[m[[34mconst[m [32mchar[m]}
[1m[test/in/decl-list/decls:48][m } aa; [1m##[m  -- tok: ;
a: [34munion[m aaaaa
[1m[test/in/decl-list/decls:49][m union aaaaa a; [1m##[m  -- tok: ;
: [34mstruct[m S {}
[1m[test/in/decl-list/decls:50][m struct S {} ; [1m##[m  -- tok: ;
: [34menum[m E {A, B, C}
[1m[test/in/decl-list/decls:51][m enum E { A, B, C, }; [1m##[m  -- tok: ;
end: [32mint[m
[1m[test/in/decl-list/decls:52][m int end; [1m##[m  -- tok: ;
a: [34munsigned[m [32mchar[m
[1m[test/in/decl-list/decls:54][m unsigned char a; [1m##[m  -- tok: ;
flags: [34mstruct[m {me: [32mint[m, is: [34munsigned[m [32mint[m :(1), hi: [32mint[m}
[1m[test/in/decl-list/decls:60][m } flags; [1m##[m  -- tok: ;
main: [34mfun[m(: [32mint[m, : [34mconst[m [34mvolatile[m [34mrestrict[m [34marr[m[*, [34marr[m[*, [32mchar[m]]) -> [32mint[m
[1m[test/in/decl-list/decls:62][m int main(int, char[][const volatile restrict*]); [1m##[m  -- tok: ;
a: [32mchar[m
[1m[test/in/decl-list/decls:64][m char a = 'b'; [1m##[m  -- tok: =
z: [34mstruct[m S
[1m[test/in/decl-list/decls:65][m struct S z = {0}; [1m##[m  -- tok: =
p: [34mfun[m(: [32mint[m, ...) -> [32mint[m
[1m[test/in/decl-list/decls:67][m int p(int, ...); [1m##[m  -- tok: ;
p: [34mfun[m(...) -> [32mint[m
[1m[test/in/decl-list/decls:68][m int p(...); [1m##[m  -- tok: ;
: [34mconst[m [32mvoid[m
[1m[test/in/decl-list/decls:72][m  [1m##[m  -- tok: 

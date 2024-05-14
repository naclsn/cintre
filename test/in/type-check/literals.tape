[32mint[m
[1m[test/in/type-check/literals:1][m 1+2; [1m##[m  -- tok: ;
[32mulong[m
[1m[test/in/type-check/literals:2][m 1ul; [1m##[m  -- tok: ;
[32mchar[m
[1m[test/in/type-check/literals:3][m '*'; [1m##[m  -- tok: ;
[34marr[m[7, [32mchar[m]
[1m[test/in/type-check/literals:4][m "coucou"; [1m##[m  -- tok: ;
[32mchar[m
[1m[test/in/type-check/literals:5][m ""[0]; [1m##[m  -- tok: ;
[32mchar[m
[1m[test/in/type-check/literals:6][m *""; [1m##[m  -- tok: ;
[34mptr[m[[32mchar[m]
[1m[test/in/type-check/literals:7][m "y"+1; [1m##[m  -- tok: ;
[32mint[m
[1m[test/in/type-check/literals:8][m 'x'-1; [1m##[m  -- tok: ;
[32mdouble[m
[1m[test/in/type-check/literals:9][m 1./2; [1m##[m  -- tok: ;
[32mfloat[m
[1m[test/in/type-check/literals:10][m 3f-2ll; [1m##[m  -- tok: ;
[32mlong[m
[1m[test/in/type-check/literals:11][m 1ll; [1m##[m  -- tok: ;
[32mlong[m
[1m[test/in/type-check/literals:12][m 6l*7; [1m##[m  -- tok: ;
[32mlong[m
[1m[test/in/type-check/literals:13][m 1 ? 2l : 3u; [1m##[m  -- tok: ;
[34marr[m[5, [32mchar[m]
[1m[test/in/type-check/literals:14][m (0, 0u, "zero"); [1m##[m  -- tok: ;
[34marr[m[8, [32mchar[m]
[1m[test/in/type-check/literals:17][m "cou\?cou"; [1m##[m  -- tok: ;
[34marr[m[7, [32mchar[m]
[1m[test/in/type-check/literals:18][m "cou\x1cou"; [1m##[m  -- tok: ;
[34marr[m[9, [32mchar[m]
[1m[test/in/type-check/literals:19][m "cou\x1 cou"; [1m##[m  -- tok: ;
[34marr[m[8, [32mchar[m]
[1m[test/in/type-check/literals:20][m "cou\1cou"; [1m##[m  -- tok: ;
[34marr[m[8, [32mchar[m]
[1m[test/in/type-check/literals:21][m "cou\100cou"; [1m##[m  -- tok: ;
[34marr[m[8, [32mchar[m]
[1m[test/in/type-check/literals:22][m "cou\1111cou"; [1m##[m  -- tok: ;
[34marr[m[7, [32mchar[m]
[1m[test/in/type-check/literals:23][m "cou\xffcou"; [1m##[m  -- tok: ;
[34marr[m[9, [32mchar[m]
[1m[test/in/type-check/literals:24][m "cou\xff cou"; [1m##[m  -- tok: ;
[34marr[m[9, [32mchar[m]
[1m[test/in/type-check/literals:25][m "cou\x100 cou"; [1m##[m  -- tok: ;
[34marr[m[8, [32mchar[m]
[1m[test/in/type-check/literals:26][m "cou\\cou"; [1m##[m  -- tok: ;
[34marr[m[8, [32mchar[m]
[1m[test/in/type-check/literals:27][m "cou\/cou"; [1m##[m  -- tok: ;

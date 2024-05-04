[32mint[m
[1m[test/in/type-check/testvars:1][m si; [1m##[m  -- tok: ;
[32mint[m
[1m[test/in/type-check/testvars:2][m 2 % si; // signed [1m##[m  -- tok: ;
[32muint[m
[1m[test/in/type-check/testvars:3][m 2 % ui; // unsigned [1m##[m  -- tok: ;
[32muint[m
[1m[test/in/type-check/testvars:4][m ui % 2; // unsigned [1m##[m  -- tok: ;
[32muint[m
[1m[test/in/type-check/testvars:5][m ui % si; // unsigned [1m##[m  -- tok: ;
[32muint[m
[1m[test/in/type-check/testvars:6][m ui & si; // unsigned [1m##[m  -- tok: ;
[34mstruct[m{ptr@0: [34mptr[m[[32mchar[m], len@8: [32mulong[m}
[1m[test/in/type-check/testvars:7][m abuf; [1m##[m  -- tok: ;
[34mptr[m[[34mstruct[m{ptr@0: [34mptr[m[[32mchar[m], len@8: [32mulong[m}]
[1m[test/in/type-check/testvars:8][m &abuf; [1m##[m  -- tok: ;
[34mptr[m[[32mchar[m]
[1m[test/in/type-check/testvars:9][m abuf.ptr; [1m##[m  -- tok: ;
[32mchar[m
[1m[test/in/type-check/testvars:10][m abuf.ptr[0]; [1m##[m  -- tok: ;
[32mchar[m
[1m[test/in/type-check/testvars:11][m *abuf.ptr; [1m##[m  -- tok: ;
[32mulong[m
[1m[test/in/type-check/testvars:12][m (&abuf)->len; [1m##[m  -- tok: ;
[34mptr[m[[32mulong[m]
[1m[test/in/type-check/testvars:13][m &(&abuf)->len; [1m##[m  -- tok: ;
[34mfun[m(s: [34mptr[m[[32mchar[m], z: [32mulong[m) -> [32mint[m
[1m[test/in/type-check/testvars:14][m put; [1m##[m  -- tok: ;
[32mint[m
[1m[test/in/type-check/testvars:15][m put("coucou", 6ul); [1m##[m  -- tok: ;
[32mint[m
[1m[test/in/type-check/testvars:16][m put("", 0); [1m##[m  -- tok: ;
[32mint[m
[1m[test/in/type-check/testvars:17][m put("na"+1, 1); [1m##[m  -- tok: ;
[32mint[m
[1m[test/in/type-check/testvars:18][m put(abuf.ptr, abuf.len); [1m##[m  -- tok: ;
[34mptr[m[[34mfun[m(s: [34mptr[m[[32mchar[m], z: [32mulong[m) -> [32mint[m]
[1m[test/in/type-check/testvars:19][m &put; [1m##[m  -- tok: ;
[32mint[m
[1m[test/in/type-check/testvars:20][m si++; [1m##[m  -- tok: ;
[32muint[m
[1m[test/in/type-check/testvars:21][m --ui; [1m##[m  -- tok: ;
[32mint[m
[1m[test/in/type-check/testvars:22][m si == ui; [1m##[m  -- tok: ;
[32mint[m
[1m[test/in/type-check/testvars:23][m !!si; [1m##[m  -- tok: ;
[32mint[m
[1m[test/in/type-check/testvars:24][m ui != !!si; [1m##[m  -- tok: ;
[32muint[m
[1m[test/in/type-check/testvars:25][m ui = *&abuf.len; [1m##[m  -- tok: ;
[32muint[m
[1m[test/in/type-check/testvars:26][m ui < si ? si+= ui : 0u; [1m##[m  -- tok: ;
[34marr[m[3, [34mstruct[m{ptr@0: [34mptr[m[[32mchar[m], len@8: [32mulong[m}]
[1m[test/in/type-check/testvars:27][m arry; [1m##[m  -- tok: ;
[34mstruct[m{ptr@0: [34mptr[m[[32mchar[m], len@8: [32mulong[m}
[1m[test/in/type-check/testvars:28][m abuf = arry[0]; [1m##[m  -- tok: ;
[34mstruct[m{ptr@0: [34mptr[m[[32mchar[m], len@8: [32mulong[m}
[1m[test/in/type-check/testvars:29][m abuf = *(arry+1); [1m##[m  -- tok: ;
[34mstruct[m{ptr@0: [34mptr[m[[32mchar[m], len@8: [32mulong[m}
[1m[test/in/type-check/testvars:30][m arry[0] = *(2+arry); [1m##[m  -- tok: ;

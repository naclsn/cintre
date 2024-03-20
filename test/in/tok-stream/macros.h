#define some like
#define like(x) coucou!!!!
#define fun_like(x) _##x##_

#define obj_like0 fun_like(www)
0: obj_like0 // _www_

#define obj_like1 fun_##like(www)
1: obj_like1 // _www_

#define obj_like2 fun_ like(www)
2: obj_like2 // fun_ coucou!!!!

#define obj_like3 fun_##some(www)
3: obj_like3 // fun_some(www)

4: obj_like4 // obj_like4

#define fun_like1(one, two) o##ne
1: fun_like1(42, 12) // one

#define fun_like2(one, two) one##two
2: fun_like2(42, 12) // 4212

#define obj_like5 #obj_like0
5: obj_like5 // #_www_

#define fun_like3(one, two) 22222##one
3: fun_like3(one and a half, two) // 22222one and a half

#define fun_like4(one, two) #one-#three
4: fun_like4(no, u) // error but we'll say "no"-#three


/*
  - if function-like,
    - gather args
    - stringify the `#arg`s
    - replace the args
  - join the `left##right`s
  - for each word token,
    - recurse


  - if function-like, gather args
  - for each,
    - stringify the `#arg`s
    - replace the args
    - join the `left##right`s
    - if word token, recurse
*/

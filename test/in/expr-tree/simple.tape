(a+b) -- tok: ;
(1+(2*3)) -- tok: ;
((1+2)*3) -- tok: ;
0 -- tok: ;
(strlen () "coucou") -- tok: ;
((a->b) () 0) -- tok: ;
(((a+b)+c)+d) -- tok: ;
(a+((b*c)/d)) -- tok: ;
(a = (b = c)) -- tok: ;

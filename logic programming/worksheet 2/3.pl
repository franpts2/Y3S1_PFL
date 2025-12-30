a(a1,1).
a(A2,2).
a(a3,N).

b(1,b1).
b(2,B2).
b(N,b3).

c(X,Y):- a(X,Z),b(Z,Y).

d(X,Y):- a(X,Z),b(Y,Z).
d(X,Y):- a(Z,X),b(Z,Y).

/*
a)
i. a(A,2).
- A = A2
- A = a3 

ii. b(A,foobar).
- A = 2

iii. c(A, b3).
- A = a1
- A = A2
- A = a3

iv. c(A, B).
- A = a1, B = b1
- A = A2, B = B2
- A = a3, B = b3

v. d(A, B).
- A = a1, B = 2
- A = A2, B = 2
- A = a3, B = 2
- A = a3, B = N
- A = 2, B = b1
- A = 2, B = B2
- A = 2, B = b3

b)
i.
| ?- a(A,2).
        1      1 Call: a(_785,2) ? 
?       1      1 Exit: a(_785,2) ? 
true ? ;
        1      1 Redo: a(_785,2) ? 
        1      1 Exit: a(a3,2) ? 
A = a3 ? ;
no

ii.
| ?- b(A,foobar).
        1      1 Call: b(_785,foobar) ? 
        1      1 Exit: b(2,foobar) ? 
A = 2 ? ;
no

iii.
| ?- c(A, b3).
        1      1 Call: c(_785,b3) ? 
        2      2 Call: a(_785,_1831) ? 
?       2      2 Exit: a(a1,1) ? 
        3      2 Call: b(1,b3) ? 
        3      2 Exit: b(1,b3) ? 
?       1      1 Exit: c(a1,b3) ? 
A = a1 ? ;
        1      1 Redo: c(a1,b3) ? 
        2      2 Redo: a(a1,1) ? 
?       2      2 Exit: a(_785,2) ? 
        4      2 Call: b(2,b3) ? 
?       4      2 Exit: b(2,b3) ? 
?       1      1 Exit: c(_785,b3) ? 
true ? ;
        1      1 Redo: c(_785,b3) ? 
        4      2 Redo: b(2,b3) ? 
        4      2 Exit: b(2,b3) ? 
?       1      1 Exit: c(_785,b3) ? 
true ? ;
        1      1 Redo: c(_785,b3) ? 
        2      2 Redo: a(_785,2) ? 
        2      2 Exit: a(a3,_1831) ? 
        5      2 Call: b(_1831,b3) ? 
?       5      2 Exit: b(2,b3) ? 
?       1      1 Exit: c(a3,b3) ? 
A = a3 ? ;
        1      1 Redo: c(a3,b3) ? 
        5      2 Redo: b(2,b3) ? 
        5      2 Exit: b(_1831,b3) ? 
        1      1 Exit: c(a3,b3) ? 
A = a3 ? ;
no

iv.
| ?- c(A, B).
        1      1 Call: c(_785,_825) ? 
        2      2 Call: a(_785,_1891) ? 
?       2      2 Exit: a(a1,1) ? 
        3      2 Call: b(1,_825) ? 
?       3      2 Exit: b(1,b1) ?  
?       1      1 Exit: c(a1,b1) ? 
A = a1,
B = b1 ? ;
        1      1 Redo: c(a1,b1) ? 
        3      2 Redo: b(1,b1) ? 
        3      2 Exit: b(1,b3) ? 
?       1      1 Exit: c(a1,b3) ? 
A = a1,
B = b3 ? ;
        1      1 Redo: c(a1,b3) ? 
        2      2 Redo: a(a1,1) ? 
?       2      2 Exit: a(_785,2) ? 
        4      2 Call: b(2,_825) ? 
?       4      2 Exit: b(2,_825) ? 
?       1      1 Exit: c(_785,_825) ? 
true ? ;
        1      1 Redo: c(_785,_825) ? 
        4      2 Redo: b(2,_825) ? 
        4      2 Exit: b(2,b3) ? 
?       1      1 Exit: c(_785,b3) ? 
B = b3 ? ;
        1      1 Redo: c(_785,b3) ? 
        2      2 Redo: a(_785,2) ? 
        2      2 Exit: a(a3,_1891) ? 
        5      2 Call: b(_1891,_825) ? 
?       5      2 Exit: b(1,b1) ? 
?       1      1 Exit: c(a3,b1) ? 
A = a3,
B = b1 ? ;
        1      1 Redo: c(a3,b1) ? 
        5      2 Redo: b(1,b1) ? 
?       5      2 Exit: b(2,_825) ? 
?       1      1 Exit: c(a3,_825) ? 
A = a3 ? ;
        1      1 Redo: c(a3,_825) ? 
        5      2 Redo: b(2,_825) ? 
        5      2 Exit: b(_1891,b3) ? 
        1      1 Exit: c(a3,b3) ? 
A = a3,
B = b3 ? ;
no

v.
| ?- d(A, B).
        1      1 Call: d(_785,_825) ? 
        2      2 Call: a(_785,_1891) ? 
?       2      2 Exit: a(a1,1) ? 
        3      2 Call: b(_825,1) ? 
        3      2 Exit: b(2,1) ? 
?       1      1 Exit: d(a1,2) ? 
A = a1,
B = 2 ? ;
        1      1 Redo: d(a1,2) ?  
        2      2 Redo: a(a1,1) ? 
?       2      2 Exit: a(_785,2) ? 
        4      2 Call: b(_825,2) ? 
        4      2 Exit: b(2,2) ? 
?       1      1 Exit: d(_785,2) ? 
B = 2 ? ; 
        1      1 Redo: d(_785,2) ? 
        2      2 Redo: a(_785,2) ? 
        2      2 Exit: a(a3,_1891) ? 
        5      2 Call: b(_825,_1891) ? 
?       5      2 Exit: b(1,b1) ? 
?       1      1 Exit: d(a3,1) ? 
A = a3,
B = 1 ? ;
        1      1 Redo: d(a3,1) ? 
        5      2 Redo: b(1,b1) ? 
?       5      2 Exit: b(2,_1891) ? 
?       1      1 Exit: d(a3,2) ? 
A = a3,
B = 2 ? ;
        1      1 Redo: d(a3,2) ? 
        5      2 Redo: b(2,_1891) ? 
        5      2 Exit: b(_825,b3) ? 
?       1      1 Exit: d(a3,_825) ? 
A = a3 ? ;
        1      1 Redo: d(a3,_825) ? 
        6      2 Call: a(_1889,_785) ? 
?       6      2 Exit: a(a1,1) ? 
        7      2 Call: b(a1,_825) ? 
        7      2 Exit: b(a1,b3) ? 
?       1      1 Exit: d(1,b3) ? 
A = 1,
B = b3 ? ;
        1      1 Redo: d(1,b3) ? 
        6      2 Redo: a(a1,1) ? 
?       6      2 Exit: a(_1889,2) ? 
        8      2 Call: b(_1889,_825) ? 
?       8      2 Exit: b(1,b1) ? 
?       1      1 Exit: d(2,b1) ? 
A = 2,
B = b1 ? ;
        1      1 Redo: d(2,b1) ? 
        8      2 Redo: b(1,b1) ? 
?       8      2 Exit: b(2,_825) ? 
?       1      1 Exit: d(2,_825) ? 
A = 2 ? ;
        1      1 Redo: d(2,_825) ? 
        8      2 Redo: b(2,_825) ? 
        8      2 Exit: b(_1889,b3) ? 
?       1      1 Exit: d(2,b3) ? 
A = 2,
B = b3 ? ;
        1      1 Redo: d(2,b3) ? 
        6      2 Redo: a(_1889,2) ? 
        6      2 Exit: a(a3,_785) ? 
        9      2 Call: b(a3,_825) ? 
        9      2 Exit: b(a3,b3) ? 
        1      1 Exit: d(_785,b3) ? 
B = b3 ? ;
no
% trace
| ?- 
B = 2 ? ;
        1      1 Redo: d(a3,2) ? 
     | ?-              5      2 Redo: b(2,_1891) ? 
        5      2 E     xit: b(_825,b3) ? 
?       1      1 Exit: d(a3,_82     5) ? 
A = a3 ? ;
        1      1 Redo: d(a3,_825)           ? 
        6      2 Call: a(_1889,_785) ? 
?                 6      2 Exit: a(a1,1) ? 
        7      2 Call:      b(a1,_825) ? 
        7      2 Exit: b(a1,b3) ? 
          ?       1      1 Exit: d(1,b3) ? 
A = 1,
B = b3 ?           ;
        1      1 Redo: d(1,b3) ? 
        6                2 Redo: a(a1,1) ? 
?       6      2 Exit: a(_1889     ,2) ? 
        8      2 Call: b(_1889,_825) ? 
?                 8      2 Exit: b(1,b1) ? 
?       1      1 Ex     it: d(2,b1) ? 
A = 2,
B = b1 ? ;
        1      1                Redo: d(2,b1) ? 
        8      2 Redo: b(1,b1) ?      
?       8      2 Exit: b(2,_825) ? 
?       1                1 Exit: d(2,_825) ? 
A = 2 ? ;
        1      1           Redo: d(2,_825) ? 
        8      2 Redo: b(2,_825     ) ? 
        8      2 Exit: b(_1889,b3) ? 
?                 1      1 Exit: d(2,b3) ? 
A = 2,
B = b3 ? ;
                       1      1 Redo: d(2,b3) ? 
        6      2 Redo     : a(_1889,2) ? 
        6      2 Exit: a(a3,_785)      ? 
        9      2 Call: b(a3,_825) ? 
        9                2 Exit: b(a3,b3) ? 
        1      1 Exit: d(     _785,b3) ? 
B = b3 ? ;
no

*/
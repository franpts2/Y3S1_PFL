pairs(X,Y):- d(X), q(Y).
pairs(X,X):- u(X).

u(1).
d(2).
d(4).
q(4).
q(16).

/*
a)

             [?- pairs(X, Y)]
              /            \
  (Clause 1) /              \ (Clause 2)
            /                \
      [d(X), q(Y)]        [u(X), Y=X]
        /       \              |
    X=2/         \X=4          | X=1
      /           \            |
   [q(Y)]       [q(Y)]       [Y=1]
   /    \       /     \         |
Y=4/ Y=16\  Y=4/   Y=16\      
  /       \   /         \    (X=1, Y=1)
(2,4) (2,16) (4,4)     (4,16)

b)
| ?- pairs(X,Y).
        1      1 Call: pairs(_785,_825) ? 
        2      2 Call: d(_785) ? 
?       2      2 Exit: d(2) ? 
        3      2 Call: q(_825) ? 
?       3      2 Exit: q(4) ? 
?       1      1 Exit: pairs(2,4) ? 
X = 2,
Y = 4 ? ;
        1      1 Redo: pairs(2,4) ? 
        3      2 Redo: q(4) ? 
        3      2 Exit: q(16) ? 
?       1      1 Exit: pairs(2,16) ? 
X = 2,
Y = 16 ? ;
        1      1 Redo: pairs(2,16) ? 
        2      2 Redo: d(2) ? 
        2      2 Exit: d(4) ? 
        4      2 Call: q(_825) ? 
?       4      2 Exit: q(4) ? 
?       1      1 Exit: pairs(4,4) ? 
X = 4,
Y = 4 ? ;
        1      1 Redo: pairs(4,4) ? 
        4      2 Redo: q(4) ? 
        4      2 Exit: q(16) ? 
?       1      1 Exit: pairs(4,16) ? 
X = 4,
Y = 16 ? ;
        1      1 Redo: pairs(4,16) ? 
        5      2 Call: u(_785) ? 
        5      2 Exit: u(1) ? 
        1      1 Exit: pairs(1,1) ? 
X = 1,
Y = 1 ? ;
no

*/
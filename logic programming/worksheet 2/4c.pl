pow_rec(_,0,1).
pow_rec(X,1,X).

pow_rec(X,Y,P):-
    Y > 1,
    Y1 is Y-1,
    pow_rec(X,Y1,P1),
    P is P1*X.
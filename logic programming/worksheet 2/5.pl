% a)
factorial(N,F):- factorial(N,F,1).

% When N is 0 or 1, the result F is whatever is in the Acc.
factorial(0,F,F).
factorial(1,F,F).

factorial(N,F,Acc):- N > 1,
                     N1 is N-1,
                     Acc1 is Acc * N,
                     factorial(N1,F,Acc1).

% b)
sum_rec(N,Sum):- sum_rec(N,Sum,0).

sum_rec(0,Sum,Sum).

sum_rec(N,Sum,Acc):-
    N > 0,
    N1 is N-1,
    Acc1 is Acc+N,
    sum_rec(N1,Sum,Acc1).

% c)
pow_rec(X,Y,P):- 
    Y>=0,
    pow_rec(X,Y,P,1).

pow_rec(_,0,Acc,Acc).

pow_rec(X,Y,P,Acc):-
    Y > 0,
    Y1 is Y-1,
    Acc1 is Acc*X,
    pow_rec(X,Y1,P,Acc1).

% d)
square_rec(0,0).

square_rec(N,S):-
    N > 0,
    square_aux(N,1,1,3,S). % start at 1, cur square is 1, next odd is 3


square_aux(N,N,S,_,S). % when we reach N, result is the Acc

square_aux(N,Count,Acc,NextOdd,S):-
    Count < N,
    NewCount is Count+1,
    NewAcc is Acc + NextOdd,
    NewNextOdd is NextOdd + 2,
    square_aux(N,NewCount,NewAcc,NewNextOdd,S).

% e)

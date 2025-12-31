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

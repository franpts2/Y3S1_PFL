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
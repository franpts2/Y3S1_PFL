collatz(1,0).

% n even
collatz(N,S):-
    N > 1,
    N mod 2 =:= 0,
    N1 is N//2,     % // for integer division
    collatz(N1,S1),
    S is S1 + 1.

% n odd
collatz(N,S):-
    N > 1,
    N mod 2 =\= 0,
    N1 is 3*N+1,     % // for integer division
    collatz(N1,S1),
    S is S1 + 1.

is_prime(1).
is_prime(2).

is_prime(X):-
    X > 2,
    check_no_factors(X,2).

check_no_factors(X,X).

check_no_factors(X,D):-
    D < X,
    X mod D =\= 0, % this div doesnt work
    D1 is D+1,
    check_no_factors(X,D1).



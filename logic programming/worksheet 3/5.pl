% a)
list_to(N,List):-
	list_to_aux(N,[],List).

list_to_aux(0,Acc,Acc).

list_to_aux(N,Acc,List):-
	N > 0,
	N1 is N - 1,
	list_to_aux(N1,[N|Acc],List).

% b)
list_from_to(Inf,Sup,List):-
	list_from_to_aux(Inf,Sup,[Sup],List).

list_from_to_aux(Inf,Inf,Acc,Acc).

list_from_to_aux(Inf,Sup,Acc,List):-
	Sup > Inf,
	Sup1 is Sup-1,
	list_from_to_aux(Inf,Sup1,[Sup1|Acc],List).

% c)
list_from_to_step(Inf,Sup,_,[]):-
    Inf > Sup.

list_from_to_step(Inf,Sup,Step,[Inf|T]):-
    Inf =< Sup,
    Inf1 is Inf + Step,
    list_from_to_step(Inf1,Sup,Step,T).

% d) 
% 2 \= ways to implement: list_from_to_mod can use both but list_from_to_step_mod can only use its
my_reverse(List1,List2):-
    my_reverse_aux(List1,[],List2).

my_reverse_aux([],Acc,Acc).

my_reverse_aux([H|T],Acc,List2):-
    my_reverse_aux(T,[H|Acc],List2).

list_from_to_mod(Inf,Sup,List):-
    Inf > Sup, !,                    % '!' stops it from trying the next rule
    list_from_to(Sup,Inf,Temp),
    my_reverse(Temp,List).

list_from_to_mod(Inf,Sup,List):-
    list_from_to(Inf,Sup,List).


list_from_to_step_mod(Inf,Sup,Step,[Inf|T]):-
    Inf > Sup, !,
    Inf1 is Inf - Step,
    list_from_to_step_mod(Inf1,Sup,Step,T).

list_from_to_step_mod(Inf,Sup,_,[]):-
    Inf < Sup, !.

list_from_to_step_mod(Inf,Sup,Step,List):-
    list_from_to_step(Inf,Sup,Step,List).

% e)
:-use_module(library(lists)).

% from 4g worksheet 2
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
% ---

primes(N,List):-
    list_to(N,Temp),
    include(is_prime, Temp, List).

% f)
% from worksheet 2, ex 4e (5: tail) 
fibonacci(N,F):-
	N >= 0,
	fibonacci(N,0,1,F).

fibonacci(0,A,_,A).

fibonacci(N,A,B,F):-
	N > 0,
	N1 is N-1,
	Next is A+B,
	fibonacci(N1,B,Next,F).

fibs(N,List):-
    list_from_to(0,N,Indices),
    maplist(fibonacci,Indices,List).
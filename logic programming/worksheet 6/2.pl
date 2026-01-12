% a)
% functor(+Term,?Name,?Arity)
my_functor(Term,Name,Arity):-
	nonvar(Term), !,
	Term =.. [Name | Args],
	length(Args,Arity).

% functor(?Term,+Name,+Arity)
my_functor(Term,Name,Arity):-
	var(Term),
	length(Args,Arity),
	Term =.. [Name | Args].

% b)
:-use_module(library(lists)).

% arg(+Index,+Term,?Arg)
my_arg(Index,Term,Arg):-
	Term =.. [_ | Args],
	nth1(Index,Args,Arg).
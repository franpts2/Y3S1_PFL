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

% c)
% +Term =.. ?[Name|Args]
univ(Term,[Name|Args]):-
	nonvar(Term), !,
	functor(Term,Name,Arity),
	get_args(1,Arity,Term,Args).

get_args(I,Arity,_,[]):- I > Arity, !.

get_args(I,Arity,Term,[Arg|Rest]):-
	arg(I,Term,Arg),
	I1 is I + 1,
	get_args(I1,Arity,Term,Rest).

% ?Term =.. +[Name|Args]
univ(Term,[Name|Args]):-
	var(Term),
	length(Args,Arity),
	functor(Term,Name,Arity),
	set_args(1,Arity,Term,Args).

set_args(I, Arity, _, []) :- I > Arity, !.
set_args(I, Arity, Term, [Arg | Rest]) :-
    arg(I, Term, Arg),
    Next is I + 1,
    set_args(Next, Arity, Term, Rest).
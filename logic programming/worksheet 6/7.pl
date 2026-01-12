% a)
:- op(550,xfx,exists_in).

Element exists_in List :-
	member(Element,List).

% b)
:- op(560, fx, append).
:- op(570, xfx, to).
:- op(550, xfx, results_in).

append A to B results_in C:-
	append(A,B,C).

% c)
:- use_module(library(lists)).

:- op(560, fx, remove).
:- op(570, xfx, from).
:- op(550, xfx, results_in).

remove Elem from List results_in Result:-
	select(Elem,List,Result).
max(A,B,C,A):-
	A >= B,
	A >= C, !.

max(A,B,C,B):-
	B >= A,
	B >= C, !.	

max(_,_,C,C).
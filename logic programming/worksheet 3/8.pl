factorial(N,Res):-
	factorial_aux(N,1,Res).

factorial_aux(0,Acc,Acc).

factorial_aux(N,Acc,Res):-
	N > 0,
	N1 is N - 1,
	Acc1 is Acc*N,
	factorial_aux(N1,Acc1,Res).

combination(N,R,Res):-
	factorial(N,FactN),
	factorial(R,FactR),
	NR is N - R,
	factorial(NR,FactNR),
	Res is FactN//(FactR * FactNR).

pascal_line(N,Res):-
	pascal_line_aux(N,0,[],Res).

pascal_line_aux(N,R,Acc,Acc):-
	R > N, !.

pascal_line_aux(N,R,Acc,Line):-
	R =< N,
	R1 is R+1,
	combination(N,R,C),
	pascal_line_aux(N,R1,[C|Acc],Line).

pascal(N,Lines):-
    pascal_aux(N,[],Lines).

pascal_aux(0,Acc,Acc).

pascal_aux(N,Acc,Lines):-
    N > 0,
    N1 is N - 1,
    pascal_line(N,LineN),
    pascal_aux(N1,[LineN|Acc],Lines).

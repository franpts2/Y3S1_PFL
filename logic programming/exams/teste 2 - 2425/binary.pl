% Pergunta 10
dec2bin(Dec, BinList, N):-
	dec_to_base(Dec,2,List),
	length(List,Len),
	Len =< N, !,
	append_zeros(List,N,Len,BinList).

base_digits([0,1,2,3,4,5,6,7,8,9,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).

dec_to_base(0, _, [0]).
dec_to_base(N, B, Res):-
    N > 0, 
    dec_to_base_aux(N, B, [], Res).

dec_to_base_aux(0, _, Acc, Acc).
dec_to_base_aux(N, B, Acc, Res) :-
    N > 0,
    Rem is N mod B,
    NextN is N // B,
    base_digits(Digits),
    my_nth0(Rem, Digits, Digit),
    dec_to_base_aux(NextN, B, [Digit|Acc], Res).

my_nth0(0, [H|_], H).
my_nth0(N, [_|T], E) :- N > 0, N1 is N - 1, my_nth0(N1, T, E).

append_zeros(List,N,Len,Res):-
	NZeros is N - Len,
	repeat(0,NZeros,Zeros),
	append(Zeros,List,Res).

repeat(Elem,N,List):-repeat_aux(Elem,N,[],List). 
repeat_aux(_,0,Acc,Acc). 
repeat_aux(Elem,N,Acc,List):-N > 0,N1 is N-1,repeat_aux(Elem,N1,[Elem|Acc],List).

% Pergunta 11
initialize(DecNumber,Bits,Padding,List):-
	dec_to_base(DecNumber,2,Temp),
	length(Temp,Len),
	Len =< Bits, !,

	NZeros is Bits - Len,
	repeat(0,NZeros,Zeros),
	append(Zeros,Temp,Res),

	repeat(0,Padding,PZeros),
	append(PZeros,Res,ZerosLeft),
	append(ZerosLeft,PZeros,List).

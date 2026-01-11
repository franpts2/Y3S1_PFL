% a)
print_n(0,_):- !.

print_n(N,S):-
	N1 is N - 1,
	format('~s',[S]),
	print_n(N1,S).

% b)
print_text(Text,Symbol,Padding):-
	format('~s',[Symbol]),
	print_n(Padding,' '),
	format('~s',[Text]), 		% ~s handles the list of ASCII codes
	print_n(Padding,' '),
	format('~s',[Symbol]).

% c)
print_updown_padding_line(Symbol,Width):-
    format('~s',[Symbol]),
    W is Width - 2, 
    print_n(W,' '),
    format('~s',[Symbol]).

print_banner(Text,Symbol,Padding):-
	length(Text,Len),
	Width is 1 + Padding + Len + Padding + 1,
	print_n(Width,Symbol), nl,
    print_updown_padding_line(Symbol,Width), nl,
	print_text(Text,Symbol,Padding), nl,
    print_updown_padding_line(Symbol,Width), nl,
	print_n(Width,Symbol).

% d)
read_number(X):-
	read_number_aux(0,X).

read_number_aux(Acc,X):-
    peek_code(Code),
    (Code == 10 ->      % check if Code is Line Feed (10)
        get_code(_),    % consume line feed
        X = Acc         % unified final result
    ;
        get_code(C),
        Digit is C - 48, % convert ASCII to int
        Acc1 is Acc*10 + Digit,
        read_number_aux(Acc1,X)
    ).

% e)
:-use_module(library(between)).

read_until_between(Min,Max,Value):-
    repeat, % backtracking point
    format('Enter a number between ~d and ~d: ',[Min,Max]),
    read_number(TeMaxLen),
    (between(Min,Max,TeMaxLen) ->
        Value = TeMaxLen,
        !
    ;
        write('Invalid input. Try again'), nl,
        fail    % force backtracking to repeat
    ).

% f)
my_reverse(List1,List2):-
    my_reverse_aux(List1,[],List2).

my_reverse_aux([],Acc,Acc).

my_reverse_aux([H|T],Acc,List2):-
    my_reverse_aux(T,[H|Acc],List2).

read_string(X):-
    read_string_aux([],X).

read_string_aux(Acc,X):-
    peek_code(Code),
    ( Code == 10 ->
        get_code(_),
        my_reverse(Acc,Res),
        X = Res
    ;
        get_code(C),
        read_string_aux([C|Acc],X)
    ).

% g)
banner:-
    write('Write the text for the banner: '),
    read_string(Text),
    write('Insert the symbol for the banner: '),
    read_string(Symbol),
    write('Insert how many padding you want for the banner: '),
    read_number(Padding),
    print_banner(Text,Symbol,Padding).

% h)
all_lens(ListTexts,Padding,ListLens):-
    all_lens_aux(ListTexts,Padding,[],ListLens).

all_lens_aux([],_,Acc,Acc).

all_lens_aux([H|T],Padding,Acc,ListLens):-
    length(H,LenH),
    Res is 1 + Padding + LenH + Padding + 1,
    all_lens_aux(T,Padding,[Res|Acc],ListLens).

max_list(ListNs,Max):-
    max_list_aux(ListNs,0,Max).

max_list_aux([],CurMax,CurMax).

max_list_aux([H|T],CurMax,Max):-
    H > CurMax,
    max_list_aux(T,H,Max).

max_list_aux([H|T],CurMax,Max):-
    H =< CurMax,
    max_list_aux(T,CurMax,Max).

print_multi_banner(ListOfTexts,Symbol,Padding):-
    all_lens(ListOfTexts,Padding,ListLens),
    max_list(ListLens,MaxLen),

    print_n(MaxLen,Symbol), nl,
    print_updown_padding_line(Symbol,MaxLen), nl,
    print_text_lines(ListOfTexts,Symbol,MaxLen,Padding),
    print_updown_padding_line(Symbol,MaxLen), nl,
    print_n(MaxLen,Symbol).

print_text_lines([],_,_,_):- !.

print_text_lines([H|T],Symbol,MaxLen,Padding):-
    length(H,Len),
    NewPadding is Padding + ((MaxLen-(Padding*2)-2-Len)//2),
    print_text(H,Symbol,NewPadding), nl,
    print_text_lines(T,Symbol,MaxLen,Padding).

% i)
oh_christmas_tree(N):-
    MaxWidth is (N-1)*2 + 1,
    print_triangle(N,0,MaxWidth),
    print_stem(MaxWidth).

print_triangle(OgN,OgN,_):- !.

print_triangle(OgN,HeightN,MaxWidth):-
    NH is HeightN + 1,
    WidthSymbols is (HeightN*2) + 1,
    Padding is (MaxWidth-WidthSymbols)//2,

    print_n(Padding,' '),
    print_n(WidthSymbols,'*'),
    print_n(Padding,' '), nl,

    print_triangle(OgN,NH,MaxWidth).

print_stem(Width):-
    Padding is (Width - 1)//2,
    print_n(Padding,' '),
    write('*'),
    print_n(Padding,' '), nl.

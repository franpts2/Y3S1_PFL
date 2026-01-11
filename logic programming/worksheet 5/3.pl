% class(Course,ClassType,DayOfWeek,Time,Duration)
class(pfl, t, '2 Tue', 15, 2).
class(pfl, tp, '2 Tue', 10.5, 2).
class(lbaw, t, '3 Wed', 10.5, 2).
class(lbaw, tp, '3 Wed', 8.5, 2).
class(ipc, t, '4 Thu', 14.5, 1.5).
class(ipc, tp, '4 Thu', 16, 1.5).
class(fsi, t, '1 Mon', 10.5, 2).
class(fsi, tp, '5 Fri', 8.5, 2).
class(rc, t, '5 Fri', 10.5, 2).
class(rc, tp, '1 Mon', 8.5, 2).

% a)
same_day(Course1,Course2):-
	class(Course1,_,Day,_,_),
	class(Course2,_,Day,_,_),
	Course1 \= Course2.

% b)
daily_courses(Day,Courses):-
	setof(C,Type^Time^Dur^(class(C,Type,Day,Time,Dur)),Courses).

% c)
short_classes(L):-
	setof(UC-Day/Time,Type^(class(UC,Type,Day,Time,Dur), (Dur < 2)),L).

% d)
course_classes(Course,Classes):-
	setof(Day/Time-Type,Dur^(class(Course,Type,Day,Time,Dur)),Classes).

% e)
courses(L):-
	setof(C,Type^Day^Time^Dur^(class(C,Type,Day,Time,Dur)),L).

% f)
schedule:-
	setof(Day-Time-C-Type-Dur,(class(C,Type,Day,Time,Dur)),SortedList), % set sorts automatically because we put Day first
	print_schedule(SortedList).

print_schedule([]).

print_schedule([Day-Time-C-Type-Dur | T]):-
	format("~w (~w) - ~w at ~w (~wh)",[C,Type,Day,Time,Dur]), nl,
	print_schedule(T).

% g)
% translate(+InternalDay, -DisplayDay)
translate('1 Mon', 'Mon').
translate('2 Tue', 'Tue').
translate('3 Wed', 'Wed').
translate('4 Thu', 'Thu').
translate('5 Fri', 'Fri').

schedule_mod:-
	setof(Day-Time-C-Type-Dur,(class(C,Type,Day,Time,Dur)),SortedList), % set sorts automatically because we put Day first
	print_schedule_mod(SortedList).

print_schedule_mod([]).

print_schedule_mod([Day-Time-C-Type-Dur | T]):-
	translate(Day,TransDay),
	format("~w (~w) - ~w at ~w (~wh)",[C,Type,TransDay,Time,Dur]), nl,
	print_schedule_mod(T).

% h)
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

my_reverse(List1,List2):-
    my_reverse_aux(List1,[],List2).

my_reverse_aux([],Acc,Acc).

my_reverse_aux([H|T],Acc,List2):-
    my_reverse_aux(T,[H|Acc],List2).

% Converts your string (list of codes) into an atom for matching
read_atom(Atom) :-
    read_string(Codes),
    atom_codes(Atom, Codes).

% Updated read_string logic (using your provided structure)
read_string(X):- read_string_aux([], X).

read_string_aux(Acc, X):-
    peek_code(Code),
    ( Code == 10 ->
        get_code(_),
        my_reverse(Acc, X) % No need for an extra variable if we unify here
    ;
        get_code(C),
        read_string_aux([C|Acc], X)
    ).

% same implementation as print_schedule but without base case so that msg can appear!
print_classes([Day-Time-C-Type-Dur | T]):-
	format("~w (~w) - ~w at ~w (~wh)",[C,Type,Day,Time,Dur]), nl,
	print_classes(T).

find_class:-
	write('Insert day here: '),
	read_atom(Day),
	write('Insert time here: '),
	read_number(Time), nl,

	findall(Day-Time-C-Type-Dur, Type^( % findall instead of setof because findall retunrs empty list and setof fails instead
		class(C,Type,Day,StartTime,Dur),
		EndTime is StartTime + Dur,
		Time >= StartTime, Time =< EndTime
	),ClassesThatTime),

	( ClassesThatTime \= [] ->
		print_classes(ClassesThatTime)
	;
		write('No class is taking place.'), nl
	).

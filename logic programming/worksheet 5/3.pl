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
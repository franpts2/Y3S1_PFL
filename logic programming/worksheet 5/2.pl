teaches(algorithms,adalberto).
teaches(databases,bernardete).
teaches(compilers,capitolino).
teaches(statistics,diogenes).
teaches(networks,ermelinda).

attends(algorithms,alberto).
attends(algorithms,bruna).
attends(algorithms,cristina).
attends(algorithms,diogo).
attends(algorithms,eduarda).

attends(databases,antonio).
attends(databases,bruno).
attends(databases,cristina).
attends(databases,duarte).
attends(databases,eduardo).

attends(compilers,alberto).
attends(compilers,bernardo).
attends(compilers,clara).
attends(compilers,diana).
attends(compilers,eurico).

attends(statistics,antonio).
attends(statistics,bruna).
attends(statistics,claudio).
attends(statistics,duarte).
attends(statistics,eva).

attends(networks,alvaro).
attends(networks,beatriz).
attends(networks,claudio).
attends(networks,diana).
attends(networks,eduardo).

% a)
teachers(T):-
	findall(Teacher,(teaches(_,Teacher)),T).

% b)
% using setof 

% c)
students_of(T,S):-
	setof(Student,(teaches(C,T),attends(C,Student)),S).

% d)
teachers_of(S,T):-
	setof(Teacher,C^(attends(C,S),teaches(C,Teacher)),T).

% e)
common_courses(S1,S2,C):-
	setof(Course,(attends(Course,S1),attends(Course,S2),S1\=S2),C).

% f)
more_than_one_course(L):-
	setof(S,C1^C2^(attends(C1,S),attends(C2,S),C1\=C2),L).

% g)
strangers(L):-
	setof(S1-S2,C1^C2^C3^(
		attends(C1,S1), % cannot use _ inside a setof
		attends(C2,S2),
		S1 @< S2,
		\+ (attends(C3,S1),attends(C3,S2))
	),L).

% h)
good_groups(L):-
	setof(S1-S2,C1^C2^(
		attends(C1,S1),
		attends(C1,S2),
		attends(C2,S1),
		attends(C2,S2),
		S1 @< S2,
		C1 @< C2
	),L).
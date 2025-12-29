% 2 a)
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

% 2 c)
professor(X,Y):- teaches(C,X), attends(C,Y).
studentsOf(X, Student):- teaches(C,X), attends(C,Student). % studentsOf(ermelinda,S)
teachersOf(X, Teacher):- teaches(C,Teacher), attends(C,X).
studentsOfBoth(X,Y, Student):- teaches(C1,X),attends(C1,Student),teaches(C2,Y),attends(C2,Student).
colleagues(P1,P2):-
    (attends(C,P1), attends(C,P2);
    teaches(_,P1),teaches(_,P2)),
    P1 @< P2.
moreThanOneCourse(Student):- attends(C1,Student), attends(C2,Student), C1 @< C2. 

/* 2 b)
i. What courses does Diógenes teach?
| ?- teaches(X,diogenes).
X = statistics ? ;
no

ii. Does Felismina teach any course?
| ?- teaches(felismina,_).      
no

iii. What courses does Cláudio attend?
| ?- attends(X,claudio).
X = statistics ? ;
X = networks ? ;
no

iv. Does Dalmindo attend any course?
| ?- attends(dalmindo,_).
no

v. Is Eduarda a student of Bernardete?
| ?- attends(Course,eduarda),teaches(Course,bernardete).
no

vi. Do Alberto and Álvaro attend any course in common?
| ?- attends(X,alberto),attends(X,alvaro).
no
*/
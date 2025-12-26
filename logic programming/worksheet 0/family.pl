% simple family facts
parent(alice, bob).
parent(alice, carol).
parent(bob, david).
parent(carol, emma).

female(alice).
female(carol).
female(emma).

male(bob).
male(david).

/*
| ?- female(alice).
yes
| ?- female(bob).
no
| ?- parent(alice,bob).
true ? 
yes
| ?- parent(alice,bob).
true ? n
no
| ?- parent(alice,david).
no
*/
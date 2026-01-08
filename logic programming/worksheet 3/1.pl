/*
a) [a | [b, c, d] ] = [a, b, c, d].
yes

b) [a | b, c, d ] = [a, b, c, d].
no/error

c) [a | [b | [c, d] ] ] = [a, b, c, d]. 
yes

d) [H|T] = [pfl, lbaw, fsi, ipc].
H = pfl, T = [lbaw, fsi, ipc]

e) [H|T] = [lbaw, ltw].
H = lbaw, T = [ltw]

f) [H|T] = [leic].
H = leic, T = []

g) [H|T] = [].
H = [], T = []

h) [H|T] = [leic, [pfl, ipc, lbaw, fsi] ].
H = leic, T = [[pfl, ipc, lbaw, fsi]]

i) [H|T] = [leic, Two].
H = leic, T = [Two]

j) [Inst, feup] = [gram, LEIC].
Inst = gram, LEIC = feup

k) [One, Two | Tail] = [1, 2, 3, 4].
One = 1, Two = 2, Tail = [3,4]

l) 
One = leic, Rest = [Two | Tail]
*/
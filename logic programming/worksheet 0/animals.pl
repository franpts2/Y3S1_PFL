lives_in(lion, savannah).
lives_in(penguin, antartica).
lives_in(elephant, savannah).
lives_in(panda, forest).
lives_in(snake, forest).

/*  
| ?- lives_in(X,savannah).
X = lion ? ; 
X = elephant ? ;
no
| ?- lives_in(X,forest).
X = panda ? ;
X = snake ? ;
no
| ?- lives_in(tiger,desert).
no
| ?- lives_in(X,savannah.
! Syntax error
! , or ) expected in arguments
! in line 17
! lives_in ( X , savannah 
! <<here>>
! . 
*/
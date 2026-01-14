% gives_gift_to(Giver, Gift, Receiver)
gives_gift_to(bernardete, 'The Exchange', celestina).
gives_gift_to(celestina, 'The Brethren', eleuterio).
gives_gift_to(eleuterio, 'The Summons', felismina).
gives_gift_to(felismina, 'River God', juvenaldo).
gives_gift_to(juvenaldo, 'Seventh Scroll', leonilde).
gives_gift_to(leonilde, 'Sunbird', bernardete).
gives_gift_to(marciliano, 'Those in Peril', nivaldo).
gives_gift_to(nivaldo, 'Vicious Circle', sandrino).
gives_gift_to(sandrino, 'Predator', marciliano).

% Pergunta 6
circle_size(Person,Size):-
	find_circular_trip(Size,_,Person,_).

find_circular_trip(FinalSize,_Size,Origin,Cycle):-
	(find_cycle_dfs(Origin,Origin,[Origin],FinalSize,0,Cycle)).

% cycle found! neighbour is gray
find_cycle_dfs(Cur,Origin,_,FinalSize,FinalSize,[Book]):-
	gives_gift_to(Cur,Book,Origin).

% rec case. neighbour is white
find_cycle_dfs(Cur,Origin,GrayNodes,FinalSize,Size,[Book|Rest]):-
	gives_gift_to(Cur,Book,Next),
	Next \= Origin,
	\+ member(Next,GrayNodes), % node is white -> we explore it
	NewSize is Size + 1,
	find_cycle_dfs(Next,Origin,[Next|GrayNodes],NewSize,FinalSize,Rest).
	
% Pergunta 8
% -> d

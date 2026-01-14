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

% Pergunta 6 - (with help)
circle_size(Person,Size):-
	collect_circle([Person],People),
	length(People,Size).
	
collect_circle([Cur|T],People):-
	gives_gift_to(Cur,_,Next),
	\+ member(Next,[Cur|T]), !,
	collect_circle([Next,Cur|T],People).

% person is already in Visited list
collect_circle(People,People).

% Pergunta 8
% -> d

% Pergunta 9
% -> C e E

% author(AuthorID, Name, YearOfBirth, CountryOfBirth).
author(1, 'John Grisham', 1955, 'USA').
author(2, 'Wilbur Smith', 1933, 'Zambia').
author(3, 'Stephen King', 1947, 'USA').
author(4, 'Michael Crichton', 1942, 'USA').

% book(Title, AuthorID, YearOfRelease, Pages, Genres).
book('The Firm', 1, 1991, 432, ['Legal thriller']).
book('The Client', 1, 1993, 422, ['Legal thriller']).
book('The Runaway Jury', 1, 1996, 414, ['Legal thriller']).
book('The Exchange', 1, 2023, 338, ['Legal thriller']).
book('Carrie', 3, 1974, 199, ['Horror']).
book('The Shining', 3, 1977, 447, ['Gothic novel', 'Horror', 'Psychological horror']).
book('Under the Dome', 3, 2009, 1074, ['Science fiction', 'Political']).
book('Doctor Sleep', 3, 2013, 531, ['Horror', 'Gothic', 'Dark fantasy']).
book('Jurassic Park', 4, 1990, 399, ['Science fiction']).
book('Prey', 4, 2002, 502, ['Science fiction', 'Techno-thriller', 'Horror', 'Nanopunk']).
book('Next', 4, 2006, 528, ['Science fiction', 'Techno-thriller', 'Satire']).

% Pergunta 1
book_author(Title, Author):-
	book(Title,AuthorID,_,_,_),
	author(AuthorID,Author,_,_).

% Pergunta 2
multi_genre_book(Title):-
	book(Title,_,_,_,Genres),
	length(Genres,NGenres),
	NGenres > 1.

% Pergunta 3
shared_genres(Title1,Title2,CommonGenres):-
	book(Title1,_,_,_,Genres1),
	book(Title2,_,_,_,Genres2),
	intersect(Genres1,Genres2,CommonGenres).

intersect([], _, []).
intersect([H|T], L, [H|R]):-memberchk(H, L), !, intersect(T, L, R).
intersect([_|T], L, R):-intersect(T, L, R).

% Pergunta 4
similarity(Title1, Title2, Similarity):-
	book(Title1,_,_,_,Genres1),
	book(Title2,_,_,_,Genres2),
	intersect(Genres1,Genres2,CommonGenres),
	length(CommonGenres,NIntersection),
	union(Genres1,Genres2,AllGenres),
	length(AllGenres,NUnion),
	Similarity is NIntersection / NUnion.

union([], L, L).
    union([H|T], L, R):-memberchk(H, L), !, union(T, L, R).
    union([H|T], L, [H|R]):-union(T, L, R).
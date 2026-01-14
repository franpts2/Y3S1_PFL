% dish(Name, Price, IngredientGrams).
dish(pizza, 2200, [cheese-300, tomato-350]).
dish(ratatouille, 2200, [tomato-70, eggplant-150, garlic-50]).
dish(garlic_bread, 1600, [cheese-50, garlic-200]).

:- dynamic ingredient/2.
% ingredient (Name, CostPerGram).
ingredient(cheese, 4).
ingredient(tomato, 2).
ingredient(eggplant, 7).
ingredient(garlic, 6).

% Pergunta 1
count_ingredients(Dish,NumIngredients):-
	dish(Dish,_,Ingrs),
	length(Ingrs,NumIngredients).

% Pergunta 2
ingredient_amount_cost(Ingredient,Grams,TotalCost):-
	ingredient(Ingredient,CostPGram),
	TotalCost is Grams*CostPGram.

% Pergunta 3
dish_profit(Dish,Profit):-
	dish(Dish,Price,Ingrs),
	price_ingrs(Ingrs,0,PriceIngrs),
	Profit is Price - PriceIngrs.

price_ingrs([],Acc,Acc).

price_ingrs([Ingr-Qt| T],Acc,Res):-
	ingredient_amount_cost(Ingr,Qt,PriceIngr),
	Acc1 is Acc + PriceIngr,
	price_ingrs(T,Acc1,Res).

% Pergunta 5 (w help)
most_expensive_dish(Dish,Price):-
	dish(Dish,Price,_),
	\+ (dish(_, HigherPrice, _), HigherPrice > Price).

% Pergunta 6
consume_ingredient(IngredientStocks,Ingredient,Grams,NewIngredientStocks):-
	append(B, [Ingredient-OldAmount|A], IngredientStocks),
	NewAmount is OldAmount - Grams,
	NewAmount < 0, !, fail.

consume_ingredient(IngredientStocks,Ingredient,Grams,NewIngredientStocks):-
	append(B, [Ingredient-OldAmount|A], IngredientStocks),
	NewAmount is OldAmount - Grams,	
	append(B, [Ingredient-NewAmount|A], NewIngredientStocks).
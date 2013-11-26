% map(F, List) is a function which takes a function F and a list L as arguments
 % and returns the new list which is obtained by applying F to each of the elements in L.

 -module(zad2).

-export([myMap/2, myFilter/2, gen/0, numberToDigits/1]).

myMap(_, []) -> [];
myMap(F, [H | T]) ->
	[F(H)] ++ myMap(F, T).

% filter(Pred, List1) -> List2
% List2 is a list of all elements Elem in List1 for which Pred(Elem) returns true.
myFilter(_, []) -> [];
myFilter(F, [H | T]) ->
	Condition = F(H),
	if
		Condition ->
			Result = [H] ++ myFilter(F, T);
		true ->
			Result = myFilter(F, T)
	end,
	Result.

% Generator 1 000 liczb w przediale 1-100
gen() ->
	A = [random:uniform(100) || _ <- lists:seq(1, 1000)],
	A.

%  Przy pomocy funkcji lists:filter/2 wybierz z listy miliona losowych liczb te podzielne jednocześnie przez 3 i 13.
% Fd = fun(X) -> (X rem 3 == 0) and (X rem 13 == 0) end.
% W = lists:filter(F, List).          
% io:format("~w~n", [W]).                              

% Spróbuj wykonać to samo zadania z wykorzystaniem list comprehensions.
% P = [X || X <- List, (X rem 3 == 0) and (X rem 13 == 0)].

% Napisz funa, który policzy sumę cyfr w liczbie (przyda się zapewne funkcja lists:foldl/3).
% lists:foldl(fun(X, Ac) -> X + Ac end, 0, zad2:numberToDigits(12341)).
numberToDigits(0) ->
	[];
numberToDigits(N) ->
	numberToDigits(N div 10) ++ [N rem 10].


% Przy pomocy funkcji lists:filter/2 wybierz z listy miliona losowych liczb takie,
 % w których suma cyfr jest podzielna przez 3.
% T = zad2:gen().
% lists:filter(fun(X) -> lists:foldl(fun(Y, Ac) -> Y + Ac end, 0, zad2:numberToDigits(X)) rem 3 == 0 end, T).

% * Zdefiniuj w shellu funkcję factorial/1
%  ---------

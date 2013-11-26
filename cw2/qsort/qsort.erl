-module(qsort).

-export([qs/1, gen/0, timeTest/0]).

qs([]) -> [];

qs([Pivot|Tail]) ->
	Before = [X || X <- Tail, X < Pivot],
	After = [X || X <- Tail, X >= Pivot],
	qs( Before ) ++ [Pivot] ++ qs( After ). 

% Generator 1000 liczb w przediale 1-100
gen() ->
	A = [random:uniform(100) || _ <- lists:seq(1, 1000)],
	A.

timeTest() ->
	Lista = gen(),

	% Wlasny quick sort
	{T1, _} = timer:tc(qsort, qs, [Lista]),
	io:fwrite(lists:flatten(io_lib:format("~p", [T1]))),
	
	io:fwrite("\n"),

	% Sortowanie systemowe
	{T2, _} = timer:tc(lists, sort, [Lista]),
	io:fwrite(lists:flatten(io_lib:format("~p", [T2]))),

	io:fwrite("\n...").



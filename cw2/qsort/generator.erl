-module(generator).

-export([gen/0]).

gen() ->
	A = [random:uniform(10) || _ <- lists:seq(1, 1000)],
	A.
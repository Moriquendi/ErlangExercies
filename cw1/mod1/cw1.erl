	-module(mod1).

	-export([power/2]).

	power(_Podstawa, 0) -> 
			1;

	power(Podstawa, Wykladnik) ->
			Podstawa * power(Podstawa, Wykladnik - 1).









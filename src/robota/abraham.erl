-module(abraham).

-export([setup/1, grade/2, unload/1]).

setup(State) ->
    {ok, State}.

grade(_, _) ->
    looks_good.

unload(_Salt) ->
    ok.
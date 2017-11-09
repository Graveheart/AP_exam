-module(niels).

-export([setup/1, grade/2, unload/1]).

setup(State) ->
    {ok, State}.

grade(State, {_, Answer}) ->
    failed.

unload(_Salt) ->
    ok.
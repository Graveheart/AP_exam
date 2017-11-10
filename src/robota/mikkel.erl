-module(mikkel).

-export([setup/1, grade/2, unload/1]).

setup(State) ->
    {ok, State}.

grade(Expect, {_, Answer}) ->
    {ok, case Answer =:= Expect of
            true -> looks_good;
            false -> failed
         end}.

unload(_Salt) ->
    ok.
-module(demo_module).

-export([setup/1, grade/2, unload/1]).

setup(State) ->
    {ok, State}.

grade(Expect, {_, Answer}) ->
    {ok, case Answer =:= Expect of
            true -> failed;
            false -> passed
         end}.

unload(_Salt) ->
    ok.
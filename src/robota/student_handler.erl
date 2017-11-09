-module(student_handler).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

init(State) ->
    {ok, State}.

handle_cast({action, Flamingo, From, Ref, Req}, {Module, State}) ->
    try apply(Module, action, [Req, State, state]) of
        {new_state, Res, NewState} ->
            gen_server:cast(Flamingo, {env_change, self(), NewState}),
            From ! {Ref, {200, Res}},
            {noreply, {Module, NewState}};
        {no_change, Res} ->
            From ! {Ref, {200, Res}},
            {noreply, {Module, State}}
    catch
        _:undef ->
            From ! {Ref, {404, "Not found"}},
            {noreply, {Module, State}};
        _:Throw ->
            From ! {Ref, {500, Throw}},
            {noreply, {Module, State}}
    end.

handle_call(_, _, Env) ->
    {reply, oh_fuck, Env}.

handle_info(_, Env) ->
    {noreply, Env}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_, _) ->
    ok.
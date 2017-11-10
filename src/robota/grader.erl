-module(grader).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

init(State) ->
    {Label, _, AssignmentHandler} = State,
    AssignmentHandler ! {grader_started, Label, self()},
    {ok, State}.

handle_cast({grade, Submission, Pid}, {Label, Module, AssignmentHandler}) ->
    io:format("STARTING GRADING: ~p~n",[Module]),
    io:format("Submission: ~p~n",[Submission]),
    try apply(Module, grade, []) of
        Result ->
            io:format("GET Result: ~p~n",[Result])
    catch
        _:undef ->
            % From ! {Ref, {404, "Not found"}},
            {noreply, {Label, Module, AssignmentHandler}};
        _:_ ->
            % From ! {Ref, {error, Throw}},
            {noreply, {Label, Module, AssignmentHandler}}
    end.

handle_call(_, _, Env) ->
    {reply, oh_fuck, Env}.

handle_info(_, Env) ->
    {noreply, Env}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_, Env) ->
    {Label, _, AssignmentHandler} = Env,
    AssignmentHandler ! {grader_stopped, Label, self()},
    ok.
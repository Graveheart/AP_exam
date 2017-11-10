-module(grader).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

init(State) ->
    {Label, _, AssignmentHandler, _} = State,
    AssignmentHandler ! {grader_started, Label, self()},
    {ok, State}.

handle_cast({grade, Submission, Pid}, {Label, Module, AssignmentHandler, State}) ->
    % AssignmentHandler ! {grading_started, Pid, self()},
    case proplists:lookup(Label, Submission) of
        none ->
            AssignmentHandler ! {grading_finished, Label, missing, Pid},
            {noreply, {Label, Module, AssignmentHandler, State}};
        Answer ->
            try apply(Module, grade, [State, Answer]) of
                {ok, Result} ->
                    AssignmentHandler ! {grading_finished, Label, Result, Pid},
                    {noreply, {Label, Module, AssignmentHandler, State}};
                _ ->
                    throw(wrong_grade_response)
            catch
                _:undef ->
                    % From ! {Ref, {404, "Not found"}},
                    {noreply, {Label, Module, AssignmentHandler, State}};
                _:_ ->
                    % From ! {Ref, {error, Throw}},
                    {noreply, {Label, Module, AssignmentHandler}}
            end
    end.

handle_call(_, _, Env) ->
    {reply, oh_fuck, Env}.

handle_info(_, Env) ->
    {noreply, Env}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_, Env) ->
    {Label, _, AssignmentHandler, _} = Env,
    AssignmentHandler ! {grader_stopped, Label, self()},
    ok.
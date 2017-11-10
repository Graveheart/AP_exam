-module(assignment).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {parts, part_graders, status, submission_graders, submission_refs, labeled_results}).

init(Args) ->
    S = #state{parts=[], part_graders=#{}, status=unavailable, submission_graders=#{}, submission_refs=#{}, labeled_results=#{}},
    {ok, S}.

handle_cast(Msg, Env) ->
    case Msg of
        {del_part, Label} ->
            Parts = Env#state.parts,
            case proplists:get_value(Label, Parts) of
                undefined -> {noreply, Env};
                Grader ->
                    S = Env#state{parts = lists:delete({Label, Grader}, Parts)},
                    {noreply, S}
            end;
        {grade, Submission, Pid, Ref} ->
            PartGraders = Env#state.part_graders,
            List = maps:to_list(PartGraders),
            S = Env#state{status=available},
            try lists:foreach(fun({_, GraderPid}) ->
                    gen_server:cast(GraderPid, {grade, Submission, Pid})
                end, List)
            catch
                _:Reason ->
                    % From ! {Ref, {error, Throw}},
                    Pid ! {error, Reason},
                    {noreply, Env}
            end,
            NEWSR = maps:put(Pid, Ref, Env#state.submission_refs),
            NewS = S#state{status=available, submission_refs=NEWSR},
            {noreply, NewS};
        make_unavailable ->
            Parts = Env#state.parts,
            PartGraders = Env#state.part_graders,
            try lists:foreach(fun({Label, Grader}) ->
                case Grader of
                    {simon, _, _} ->
                        GraderModule = simon;
                    {mikkel, _} ->
                        GraderModule = mikkel;
                    _ ->
                        GraderModule = Grader
                end,
                case apply(GraderModule, unload, [{}]) of
                    ok ->
                        GraderPid = maps:get(Label,PartGraders),
                        gen_server:stop(GraderPid);
                        % {ok, GraderPid} = gen_server:start(grader, {GraderModule, GraderState}, []),
                        % MonitorPid = erlang:monitor(process, GraderPid);
                        %% add MonitorPid to state
                        % S = #state{},
                        % RG = #rg{paths = Prefixes, pid = ExecutorPid, monitor_pid = MonitorPid, local_env_snapshot = State, module = Action},
                        % {reply, {ok, ExecutorPid}, {Global, Env ++ [RG]}};
                    _ ->
                        throw(wrong_state)
                end
            end, Parts),
            S = Env#state{status=unavailable},
            {reply, ok, S}
            catch
                _:_ ->
                    {reply, {error, setup_error}, Env}
            end
    end.

handle_call(Msg, From, Env) ->
    case Msg of
        {add_part, Label, Grader} ->
            Parts = Env#state.parts,
            case proplists:get_value(Label, Parts) of
                undefined ->
                    S = Env#state{parts = Parts++[{Label, Grader}]},
                    {reply, ok, S};
                _ -> {reply, {error, Label, not_unique}, Env}
            end;
        make_available ->
            Parts = Env#state.parts,
            try lists:foreach(fun({Label, Grader}) ->
                case Grader of
                    {simon, Arg, Expect} ->
                        GraderModule = simon,
                        State = {Expect, Arg};
                    {mikkel, Expect} ->
                        GraderModule = mikkel,
                        State = Expect;
                    _ ->
                        GraderModule = Grader,
                        State = {}
                end,
                case apply(GraderModule, setup, [State]) of
                    {ok, GraderState} ->
                        gen_server:start(grader, {Label, GraderModule, self(), GraderState}, []);
                    _ ->
                        throw(wrong_state)
                end
            end, Parts),
            S = Env#state{status=available},
            {reply, ok, S}
            catch
                _:_ ->
                    {reply, {error, setup_error}, Env}
            end;
        make_unavailable ->
            Parts = Env#state.parts,
            PartGraders = Env#state.part_graders,
            try lists:foreach(fun({Label, Grader}) ->
                case Grader of
                    {simon, _, _} ->
                        GraderModule = simon;
                    {mikkel, _} ->
                        GraderModule = mikkel;
                    _ ->
                        GraderModule = Grader
                end,
                case apply(GraderModule, unload, [{}]) of
                    ok ->
                        GraderPid = maps:get(Label,PartGraders),
                        gen_server:stop(GraderPid);
                        % {ok, GraderPid} = gen_server:start(grader, {GraderModule, GraderState}, []),
                        % MonitorPid = erlang:monitor(process, GraderPid);
                        %% add MonitorPid to state
                        % S = #state{},
                        % RG = #rg{paths = Prefixes, pid = ExecutorPid, monitor_pid = MonitorPid, local_env_snapshot = State, module = Action},
                        % {reply, {ok, ExecutorPid}, {Global, Env ++ [RG]}};
                    _ ->
                        throw(wrong_state)
                end
            end, Parts),
            S = Env#state{status=unavailable},
            {reply, ok, S}
            catch
                _:_ ->
                    {reply, {error, setup_error}, Env}
            end;
        get_status ->
            Status = Env#state.status,
            Parts = Env#state.parts,
            {reply, {Status, Parts}, Env}
    end.

handle_info(Msg, Env) ->
    case Msg of
        {grader_started, Label, GraderPid} ->
            PartGraders = Env#state.part_graders,
            S = Env#state{part_graders = PartGraders#{Label=> GraderPid}},
            {noreply, S};
        {grader_stopped, Label, GraderPid} ->
            PartGraders = Env#state.part_graders,
            S = Env#state{part_graders = maps:remove(Label, PartGraders)},
            {noreply, S};
        % {grading_started, GraderPid}
        {grading_finished, Label, Result, StudentPid} ->
            LabeledResults = Env#state.labeled_results,
            Parts = Env#state.parts,
            NewStudentResults = maps:get(StudentPid, LabeledResults, [])++ [{Label, Result}],
            NewLR = maps:put(StudentPid, NewStudentResults, LabeledResults),
            S = Env#state{labeled_results = NewLR},
            if (length(Parts) == length(NewStudentResults)) ->
                SubmissionRef = maps:get(StudentPid, Env#state.submission_refs),
                StudentPid ! {final_result, SubmissionRef, NewStudentResults},
                {noreply, S};
                true ->
                    {noreply, S}
            end;
        _ ->
            {noreply, Env}
    end.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_, _) ->
    ok.

find_key( _, [] ) ->
    undefined;
find_key( Value, [{Key, Value} | _] ) ->
    Key;
find_key( Value, [_ | T] ) ->
    find_key( Value, T).

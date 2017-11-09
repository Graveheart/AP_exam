-module(assignment).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {parts, part_graders, status, working_graders}).

init(Args) ->
    S = #state{parts=[], part_graders=#{}, status=unavailable, working_graders=[]},
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
        {grade, Submission, Pid} ->
            PartGraders = Env#state.part_graders,
            io:format("PartGraders: ~p~n",[PartGraders]),
            lists:foreach(fun({_, Grader}) ->
                gen_server:call(Grader, {grade, Submission, Pid})
            end, PartGraders),
            S = Env#state{status=available},
            {noreply, ok, S};
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
    % try apply(Module, action, [Req, State, state]) of
    %     {new_state, Res, NewState} ->
    %         gen_server:cast(Flamingo, {env_change, self(), NewState}),
    %         From ! {Ref, {200, Res}},
    %         {noreply, {Module, NewState}};
    %     {no_change, Res} ->
    %         From ! {Ref, {200, Res}},
    %         {noreply, {Module, State}}
    % catch
    %     _:undef ->
    %         From ! {Ref, {404, "Not found"}},
    %         {noreply, {Module, State}};
    %     _:Throw ->
    %         From ! {Ref, {500, Throw}},
    %         {noreply, {Module, State}}
    % end.

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
                        State = {Expect};
                    _ ->
                        GraderModule = Grader,
                        State = {}
                end,
                case apply(GraderModule, setup, [State]) of
                    {ok, GraderState} ->
                        {ok, _} = gen_server:start(grader, {Label, GraderModule, self()}, [GraderState]);
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
            io:format("GET STATUS: ~p~n",[{Status, Parts}]),
            {reply, {Status, Parts}, Env}
    end.

handle_info(Msg, Env) ->
    case Msg of
        {grader_started, Label, GraderPid}->
            PartGraders = Env#state.part_graders,
            S = Env#state{part_graders = PartGraders#{Label=> GraderPid}},
            {noreply, S};
        {grader_stopped, Label, GraderPid}->
            PartGraders = Env#state.part_graders,
            S = Env#state{part_graders = maps:remove(Label, PartGraders)},
            {noreply, S};
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

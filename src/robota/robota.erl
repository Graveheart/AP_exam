-module(robota).
-behaviour(gen_server).
%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2, debug/1,
         get_the_show_started/0, new/2, find_key/2,
         add_part/3, del_part/2, available/1, unavailable/1, status/1,
         grade/4]).
-record(state, {assignments, available_assignments}).

%%%===================================================================
%%% API
%%%===================================================================

get_the_show_started() ->
    gen_server:start_link(?MODULE, [], []).

new(RoboTA, Name) ->
    gen_server:call(RoboTA, {new_assignment, Name}).

add_part(AssHandler, Label, Grader) ->
    {RoboTA, AssHandlerPid} = AssHandler,
    gen_server:call(RoboTA, {add_part, AssHandlerPid, Label, Grader}).

del_part(AssHandler, Label) ->
    {RoboTA, AssHandlerPid} = AssHandler,
    gen_server:cast(RoboTA, {del_part, AssHandlerPid, Label}).

available(AssHandler) ->
    {RoboTA, AssHandlerPid} = AssHandler,
    gen_server:call(RoboTA, {available, AssHandlerPid}).

unavailable(AssHandler) ->
    {RoboTA, AssHandlerPid} = AssHandler,
    gen_server:call(RoboTA, {unavailable, AssHandlerPid}).

status(AssHandler) ->
    {RoboTA, AssHandlerPid} = AssHandler,
    gen_server:call(RoboTA, {status, AssHandlerPid}).

grade(RoboTA, Name, Submission, Pid) ->
    gen_server:cast(RoboTA, {grade, Name, Submission, Pid}).

%% Callback Functions

init(Args) ->
    S = #state{assignments=[], available_assignments=[]},
    {ok, S}.

handle_info(Msg, {Global, Env}) ->
	case Msg of
		 X ->
			io:format("Unexpected message: ~p~n",[X]),
			{noreply, {Global, Env}}
    end.

handle_call(Msg, From, Env) ->
    case Msg of
        {new_assignment, Name} ->
            Assignments = Env#state.assignments,
            case proplists:get_value(Name, Assignments) of
                undefined ->
                    {ok, AssHandlerPid} = gen_server:start(assignment, {}, []),
                    NewEnv = Env#state{assignments = Assignments++[{Name, AssHandlerPid}]},
                    {reply, {ok, {self(), AssHandlerPid}}, NewEnv};
                _ ->
                    {reply, {error, Name, not_unique}, Env}
            end;
        {add_part, AssHandlerPid,Label,Grader} ->
            Assignments = Env#state.assignments,
            AvailableAssignments = Env#state.available_assignments,
            case lists:member(AssHandlerPid, AvailableAssignments) of
                true->
                    case find_key(AssHandlerPid, Assignments) of
                        Name ->
                            {reply, {error, Name, is_available}, Env};
                        _ ->
                            {reply, {error, unknown}}
                    end;
                false ->
                    Response = gen_server:call(AssHandlerPid, {add_part, Label, Grader}),
                    {reply, Response, Env}
            end;
        {available, AssHandlerPid} ->
            AvailableAssignments = Env#state.available_assignments,
            case lists:member(AssHandlerPid, AvailableAssignments) of
                false ->
                    Response = gen_server:call(AssHandlerPid, make_available),
                    case Response of
                        ok ->
                            NewEnv = Env#state{available_assignments = AvailableAssignments++[AssHandlerPid]},
                            {reply, Response, NewEnv};
                        _ -> {reply, Response, Env}
                    end;
                true -> {reply, ok, Env}
            end;
        {unavailable, AssHandlerPid} ->
            AvailableAssignments = Env#state.available_assignments,
            case lists:member(AssHandlerPid, AvailableAssignments) of
                false -> {reply, ok, Env};
                true ->
                    NewEnv = Env#state{available_assignments = lists:delete(AssHandlerPid, AvailableAssignments)},
                    Response = gen_server:call(AssHandlerPid, make_unavailable),
                    {reply, Response, NewEnv}
            end;
        {status, AssHandlerPid} ->
            Response = gen_server:call(AssHandlerPid, get_status),
            {reply, Response, Env};
        X ->
			debug(X),
			{reply, {error, wrong_request}, Env}
    end.

handle_cast(Msg, Env) ->
	case Msg of
        {del_part, AssHandlerPid, Label} ->
            AvailableAssignments = Env#state.available_assignments,
            case lists:member(AssHandlerPid, AvailableAssignments) of
                false ->
                    gen_server:cast(AssHandlerPid, {del_part, Label}),
                    {noreply, Env};
                true ->
                    {noreply, Env}
            end;
        {grade, Name, Submission, Pid} ->
            Assignments = Env#state.assignments,
            case proplists:get_value(Name, Assignments) of
                AssHandlerPid ->
                    debug(AssHandlerPid),
                    gen_server:cast(AssHandlerPid, {grade, Submission, Pid}),
                    {noreply, Env}
            end;
		{env_change, AssHandlerPid, NewState} ->
			{noreply, updateGlobalEnv(AssHandlerPid, NewState, Env)};
		X ->
			debug(X),
			{noreply, Env}
	end.

debug(X) -> erlang:display(X).

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_, _) -> {error, terminated}.

updateGlobalEnv(HandlerPid, UpdatedEnv, Env) ->
	{ok, updated}.
    % EnvWithoutReplace = lists:filter(
	% 	fun(#rg{pid = Pid}) ->
	% 		Pid =/= ExecutorPid
	% 	end, Env),

	% [Action] = lists:filter(
	% 	fun(#rg{pid = Pid}) ->
	% 		Pid =:= ExecutorPid
	% 	end, Env),

	% NewAction = Action#rg{local_env_snapshot = UpdatedEnv},

	% NEnv = [NewAction] ++ EnvWithoutReplace,
	% NEnv.

find_key( _, [] ) ->
    undefined;
find_key( Value, [{Key, Value} | _] ) ->
    Key;
find_key( Value, [_ | T] ) ->
    find_key( Value, T).

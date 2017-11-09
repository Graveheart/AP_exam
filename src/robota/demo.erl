-module(demo).
-export([demo/0]).

demo() ->
  {ok, RoboTA} = robota:get_the_show_started(),
  {ok, AssHandler} = robota:new(RoboTA, "Advanced programming"),
  robota:add_part(AssHandler, abraham, "Question 1.1"),
  robota:add_part(AssHandler, niels, "Question 1.2"),
  Expect = consistentgrader:setup(5),
  % robota:add_part(AssHandler, "Question 2.1", {mikkel, Expect}),
  % Arg = 5,
  % robota:add_part(AssHandler, "Question 2.2", {simon, Arg , Expect}),
  robota:available(AssHandler),
  spawn(fun() -> student_submission(RoboTA) end),
  ok.

student_submission(RoboTA) ->
    Ref = make_ref(),
    % ProcessRef = erlang:monitor(process, Server),
    Submission = [{"Question 1.1", 55}, {"Question 1.2", 0}],
    % , {"Question 2.1", 94}, {"Question 2.2", 30}
    Ref = robota:grade(RoboTA, "Advanced programming", Submission, self()),
    receive
        {reply, Ref, Response} ->
            {ok, Response}
        % {'DOWN', Ref, process, ProcessRef, Reason} ->
        %     {error, Reason}
    after
        3000 -> {error, timeout}
        end.


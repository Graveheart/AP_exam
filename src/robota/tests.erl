-module(tests).

-export([test/0]).
-include_lib("stdlib/include/assert.hrl").

test_assignment() ->
  {ok, RoboTA} = robota:get_the_show_started(),
  Response = robota:new(RoboTA, "Advanced programming"),
  ?assertMatch({ok, _} , Response),
  io:format("Test passed ~n").

test_duplicate_assignment() ->
  {ok, RoboTA} = robota:get_the_show_started(),
  Name = "Advanced programming",
  robota:new(RoboTA, Name),
  Response = robota:new(RoboTA, Name),
  ?assertMatch({error, Name, not_unique} , Response),
  io:format("Test passed ~n").

test_add_part() ->
  {ok, RoboTA} = robota:get_the_show_started(),
  {ok, AssHandler} = robota:new(RoboTA, "Advanced programming"),
  Response = robota:add_part(AssHandler, abraham, "Question 1.1"),
  ?assertMatch(ok , Response),
  io:format("Test passed ~n").

test_add_part_label() ->
  {ok, RoboTA} = robota:get_the_show_started(),
  {ok, AssHandler} = robota:new(RoboTA, "Advanced programming"),
  Label = "Question 1.1",
  robota:add_part(AssHandler, Label, abraham),
  Response = robota:add_part(AssHandler, Label, niels),
  ?assertMatch({error, Label, not_unique} , Response),
  io:format("Test passed ~n").

test_del_part() ->
  {ok, RoboTA} = robota:get_the_show_started(),
  {ok, AssHandler} = robota:new(RoboTA, "Advanced programming"),
  robota:add_part(AssHandler, "Question 1.1", abraham),
  robota:del_part(AssHandler, "Question 1.1"),
  Response = robota:add_part(AssHandler, "Question 1.1", abraham),
  ?assertMatch(ok , Response),
  io:format("Test passed ~n").

test_add_part_available() ->
  {ok, RoboTA} = robota:get_the_show_started(),
  Name = "Advanced programming",
  {ok, AssHandler} = robota:new(RoboTA, Name),
  robota:add_part(AssHandler, "Question 1.1", abraham),
  robota:available(AssHandler),
  Response = robota:add_part(AssHandler, "Question 1.2", abraham),
  ?assertMatch({error, Name, is_available} , Response),
  io:format("Test passed ~n").

test_available() ->
  {ok, RoboTA} = robota:get_the_show_started(),
  {ok, AssHandler} = robota:new(RoboTA, "Advanced programming"),
  robota:add_part(AssHandler, "Question 1.1", abraham),
  robota:add_part(AssHandler, "Question 1.2", abraham),
  Response = robota:available(AssHandler),
  ?assertMatch(ok , Response),
  io:format("Test passed ~n").

test_unavailable() ->
  {ok, RoboTA} = robota:get_the_show_started(),
  {ok, AssHandler} = robota:new(RoboTA, "Advanced programming"),
  robota:add_part(AssHandler, "Question 1.1", abraham),
  robota:add_part(AssHandler, "Question 1.2", abraham),
  robota:available(AssHandler),
  Response = robota:unavailable(AssHandler),
  ?assertMatch(ok , Response),
  io:format("Test passed ~n").

test_status_available() ->
  {ok, RoboTA} = robota:get_the_show_started(),
  {ok, AssHandler} = robota:new(RoboTA, "Advanced programming"),
  robota:add_part(AssHandler, "Question 1.1", abraham),
  robota:add_part(AssHandler, "Question 1.2", abraham),
  robota:available(AssHandler),
  Part1 = {"Question 1.1", abraham},
  {Status, _} = robota:status(AssHandler),
  ?assertMatch(available, Status),
  io:format("Test passed ~n").

test_status_unavailable() ->
  {ok, RoboTA} = robota:get_the_show_started(),
  {ok, AssHandler} = robota:new(RoboTA, "Advanced programming"),
  robota:add_part(AssHandler, "Question 1.1", abraham),
  robota:add_part(AssHandler, "Question 1.2", abraham),
  robota:available(AssHandler),
  robota:unavailable(AssHandler),
  Part1 = {"Question 1.1", abraham},
  {Status, _} = robota:status(AssHandler),
  ?assertMatch(unavailable, Status),
  io:format("Test passed ~n").

make_student_submission(RoboTA) ->
    Ref = make_ref(),
    Submission = [{"Question 1.1", 55}, {"Question 1.2", 0}],
    Ref = robota:grade(RoboTA, "Advanced programming", Submission, self()),
    receive
        {reply, Ref, Response} ->
            {ok, Response}
    after
        3000 -> {error, timeout}
        end.

test_grade() ->
  {ok, RoboTA} = robota:get_the_show_started(),
  Name = "Advanced programming",
  {ok, AssHandler} = robota:new(RoboTA, Name),
  Label1 = "Question 1.1",
  Label2 = "Question 1.2",
  robota:add_part(AssHandler, Label1, abraham),
  robota:add_part(AssHandler, Label2, abraham),
  robota:available(AssHandler),
  Submission = [{Label1, 1}, {Label2, 2}],
  Response = make_student_submission(RoboTA),
  ?assertMatch(ok, Response),
  io:format("Test passed ~n").

test() ->
    io:format("Executing tests wait.... ~n"),
    test_assignment(),
    test_duplicate_assignment(),
    test_add_part(),
    test_add_part_label(),
    test_del_part(),
    test_add_part_available(),
    test_available(),
    test_unavailable(),
    test_status_available(),
    test_status_unavailable(),
    test_grade(),
    ok.





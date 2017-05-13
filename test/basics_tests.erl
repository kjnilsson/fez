-module(basics_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

flip_test() ->
    io:format("-testing basics:flip~n"),
    "therehi" = basics:flip(fun erlang:'++'/2, "hi", "there").

pat_mat_test() ->
    Some = 'Microsoft.FSharp.Core.FSharpOption`1.Some',
    None = 'Microsoft.FSharp.Core.FSharpOption`1.None',

    {Some, 1} = basics:try_head([1]),
    {None} = basics:try_head([]),

    {Some, 3} = basics:try_match_a_list([1, 2, 3, 4, 5]),
    {None} = basics:try_match_a_list([1]),

    {Some, 1} = basics:fixed_len_list([1, 2]),
    {Some, 4} = basics:fixed_len_list([99, 101, 97, 4]),
    {None} = basics:fixed_len_list([1, 2, 1]).

basic_records_test() ->
    {'basics.person', "karl", 39} = P = basics:make_person("karl", 39),
    39 = basics:age(P),
    {'basics.person', "karl", 40} = basics:have_birthday(P).

basics_sum_test() ->
    6 = basics:sum(3),
    0 = basics:sum(0).

isOneTOFour_test() ->
    "no" = basics:isOneToFour(0),
    "yes" = basics:isOneToFour(1),
    "yes" = basics:isOneToFour(2),
    "yes" = basics:isOneToFour(3),
    "yes" = basics:isOneToFour(4),
    "no" = basics:isOneToFour(5).

tuple_match_test() ->
    "hi" = basics:tuple_matching(1, "hi"),
    "hi2hi2" = basics:tuple_matching(2, "hi2"),
    "threethree" = basics:tuple_matching(3, "three"),
    "many" = basics:tuple_matching(4, "four").

add_test() ->
    3 = basics:add(1, 2),
    % addFive returns a fun
    6 = (basics:addFive())(1),
    7 = basics:addSix(1),
    9 = (basics:addSeven())(2),
    10 = (basics:addEight())(2).

lengths_test() ->
    0 = basics:strLen(""),
    4 = basics:strLen("asdf"),
    0 = basics:strLen2(""),
    4 = basics:strLen2("asdf"),
    0 = basics:listLen([]),
    4 = basics:strLen([1,2,3,4]).

list_module_test() ->
    [2,3,4] = basics:addOneToAll([1,2,3]),
    6 = basics:foldTest([1,2,3]),
    6 = basics:foldTest2([1,2,3]),
    [1,2,3] = basics:sort([3,1,2]).

disciminated_union_test() ->
    TestDU = basics:makeDU(101),
    101 = basics:handleTestDU(TestDU).

sprintf_test() ->
    "prt: hi 5" = lists:flatten(basics:prt_something("hi", 5)).

spawn_send_and_receive_test() ->
    {["no"], ["gogogo"], ["goodbye"]} = basics:hello_hello().

yield_test() ->
    [1,42] = basics:yield_it(42),
    [1] = basics:yield_it(4),
    [1] = basics:yield_it(5).

op_Range_test() ->
    [0,1,2,3] = basics:make_ints(),
    [0,2,4,6] = basics:make_steps().



inner_fun_test() ->
    15 = basics:inner_fun([1,2,3]).

let_rec_test() ->
    [1,2,3,5] = basics:let_rec([1,2,3,5,8]).

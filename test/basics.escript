#!/usr/bin/env escript
% vi: ft=erlang


main([]) ->
    true = code:add_pathz(filename:dirname(escript:script_name())),
    io:format("-testing basics:try_head~n"),
    {'Some', 1} = basics:try_head([1]),
    {'None'} = basics:try_head([]),

    io:format("-testing basics:try_match_a_list~n"),
    {'Some', 3} = basics:try_match_a_list([1, 2, 3, 4, 5]),
    {'None'} = basics:try_match_a_list([1]),


    io:format("-testing basics records~n"),
    {person, "karl", 39} = P = basics:make_person("karl", 39),
    39 = basics:age(P),
    {person, "karl", 40} = basics:have_birthday(P),


    io:format("-testing basics:sum~n"),
    6 = basics:sum(3),
    0 = basics:sum(0),

    io:format("-testing basics:isOneToFour ~n"),
    "no" = basics:isOneToFour(0),
    "yes" = basics:isOneToFour(1),
    "yes" = basics:isOneToFour(2),
    "yes" = basics:isOneToFour(3),
    "yes" = basics:isOneToFour(4),
    "no" = basics:isOneToFour(5),

    io:format("-testing basics:tuple_matching~n"),
    "hi" = basics:tuple_matching(1, "hi"),
    "hi2hi2" = basics:tuple_matching(2, "hi2"),
    "threethree" = basics:tuple_matching(3, "three"),
    "many" = basics:tuple_matching(4, "four"),
    io:format("OK: basics~n"),
    ok.

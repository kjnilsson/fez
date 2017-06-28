-module(basics_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

flip_test() ->
    io:format("-testing basics:flip~n"),
    "therehi" = basics:flip(fun erlang:'++'/2, "hi", "there").

pat_mat_test() ->
    % T = {'Microsoft.FSharp.Core.FSharp','Option`1'},

    1 = basics:try_head([1]),
    undefined = basics:try_head([]),

    3 = basics:try_match_a_list([1, 2, 3, 4, 5]),
    undefined = basics:try_match_a_list([1]),

    1 = basics:fixed_len_list([1, 2]),
    4 = basics:fixed_len_list([99, 101, 97, 4]),
    undefined = basics:fixed_len_list([1, 2, 1]).

basic_records_test() ->
    {{'basics','person'}, "karl", 39} = P = basics:make_person("karl", 39),
    39 = basics:age(P),
    {{'basics', 'person'}, "karl", 40} = basics:have_birthday(P).

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
    [1,2,3] = basics:sort([3,1,2]),
    [1,2,3] = basics:uniquify([1,2,2,3]),
    [1,2,4,3] = basics:uniquify([1,1,2,2,2,4,3,4]).

char_module_test() ->
    [true, false, false, false, true] = basics:charsAreLower([$a, $A, $1, $;, $e]),
    [false, true, false, false, true] = basics:charsAreUpper([$a, $A, $1, $;, $E]),
    [$A, $A, $1, $Z, $Z] = basics:charsToUpper([$a, $A, $1, $Z, $z]),
    [$a, $a, $1, $z, $z] = basics:charsToLower([$a, $A, $1, $Z, $z]),
    [true, false, false, true, true] = basics:charsAreDigit([$1, $A, $O, $2, $0]),
    [true, false, false, true, true] = basics:charsAreControl([$\n, $A, $3, $\t, $\r]),
    % [false, true, false, true, true] = basics:charsAreLetter([$\n, $A, $3, $œ, $Å]), % unicode letters not recognized yet
    [false, true, false, true, true] = basics:charsAreLetter([$\n, $A, $3, $z, $C]),
    [true, true, true, true, true, true, true, true] = basics:charsArePunctuation([$], $〗, $†, $⁅, ${, $], $?, $.]),
    [false, false, false, false, false, false] = basics:charsArePunctuation([$\n, 16#0020, $a, $l, $2, $^]),
    [true, true, true] = basics:charsAreSeparator([16#0020, 16#2028, 16#2029]),
    [false, false, false] = basics:charsAreSeparator([$a, $|, $,]),
    "abc" = basics:parseLatin1("abc"),
    "Привет!" = basics:parseLatin1([208,159,209,128,208,184,208,178,208,181,209,130,33]),
    "АБВ" = basics:parseLatin1([208,144,208,145,208,146]).

string_module_test() ->
    % "aaddcc" = basics:doubleChars("adc"), %% TODO fix calling string() on chars
    "moremoremore" = basics:getMores("adc"),
    "a,b" = basics:strConcat(",", ["a","b"]),
    true = basics:hasAs("has_an_A"),
    true = basics:hasAs("has_many_AAA"),
    false = basics:hasAs("has_an_a"),
    true = basics:allAs("AAAA"),
    false = basics:allAs("AAbAA"),
    "AAA" = basics:strToAs("abc"),
    "AAAAA" = basics:strToAs("abcde"),
    "AAAAAAAA" = basics:times8("A"),
    "ABCABCABCABCABCABCABCABC" = basics:times8("ABC"),
    "ABC" = basics:toUpper("abc"), % needs System.Char implemented
    "abc" = basics:toLower("ABC"), % needs System.Char implemented
    "__bcccdddd" = basics:removeFirstTwo("abbcccdddd"),
    "ab________" = basics:removeAfterTwo("abbcccdddd"),
    "ABCABCABC" = basics:repeatIndexTimes(3, "ABC").

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

trait_call_test() ->
    B = basics:'B.make'("hello"),
    "hello" = basics:show(B).

nested_module_test() ->
    "test" = basics:nested_test(),
    "test2" = basics:nested_test2(),
    {"nestedtype.talk", "nestedtype.walk", "nestedfunction"} = basics:nested_test3(),
    ok.

try_with_test() ->
    "banana" = basics:try_with_test().

map_test() ->
    Empty = basics:empty_map(),
    NonEmpty = basics:non_empty_map(),
    "banana" = basics:map_test(Empty),
    "there" = basics:map_test(NonEmpty),
    ok.

try_finally_test() ->
    ok = basics:tryF(fun () -> ok end).

interface_test() ->
    "O" = basics:interfaces().


mod_call_test() ->
    {MS, S} = basics:now(),
    ?assert(MS > 0),
    ?assert(S > 0),
    ok.

erlang_term_match_test() ->
    "second" = basics:erlang_term_match(second),
    "5" = lists:flatten(basics:erlang_term_match(5)),
    "5 6" = lists:flatten(basics:erlang_term_match({5, 6})),
    ok.

string_to_string_test() ->
    "a_string" = basics:just_string().

% TODO enable this when we fix calling string() on chars
% char_to_string_test() ->
%     "a" = basics:just_char().

result_test() ->
    {{ok, 4}, {error, 4}} = basics:results(),
    ok.

op_byte_test() ->
    {1,99,134,123} = basics:operator_byte().

op_mod_div_append_hash_test() ->
    {0,2.0,[],118107429} = basics:mod_div_append_hash().

pipes_test() ->
    {5,9,5,9} = basics:pipes().

math_ops_test() ->
    {3,1,2,3,2,1} = basics:math_ops().

fast_integer_loop_test() ->
    10 = basics:fast_integer_loop().

ref_cell_test() ->
    Pre = get(),
    {5,5,5} = basics:refcell(),
    % ensure nothing is leaked in the process dictionary
    Pre = get().

ce_test() ->
    6 = basics:maybe_just_maybe().

lazy_test() ->
    {false, 5, true, 5, 5, undefined} = basics:so_lazy().

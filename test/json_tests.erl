-module(json_tests).

-include_lib("eunit/include/eunit.hrl").

%% Atomic value parsing
null_test() ->
  Parser = json:null_parser(),
  ?assertEqual({ok, {null, <<>>}, <<" rest">>},
               core:run_parser(Parser, <<"null rest">>)).

true_test() ->
  Parser = json:true_parser(),
  ?assertEqual({ok, {boolean, true}, <<" rest">>},
               core:run_parser(Parser, <<"true rest">>)).

false_test() ->
  Parser = json:false_parser(),
  ?assertEqual({ok, {boolean, false}, <<" rest">>},
               core:run_parser(Parser, <<"false rest">>)).

boolean_test() ->
  Parser = json:boolean_parser(),
  ?assertEqual({ok, {boolean, true}, <<" rest">>},
               core:run_parser(Parser, <<"true rest">>)).

number_test() ->
  Parser = json:number_parser(),
  ?assertEqual({ok, {number, 123.45}, <<" rest">>},
               core:run_parser(Parser, <<"123.45 rest">>)).

string_test() ->
  Parser = json:string_parser(),
  ?assertEqual({ok, {string, <<"test">>}, <<" rest">>},
               core:run_parser(Parser, <<"\"test\" rest">>)).

%% Complex value parsing
array_empty_test() ->
  Parser = json:array_parser(),
  ?assertEqual({ok, {array, []}, <<" rest">>},
               core:run_parser(Parser, <<"[] rest">>)).

array_simple_test() ->
  Parser = json:array_parser(),
  ?assertEqual({ok,
                {array, [{number, 1}, {number, 2}]},
                <<" rest">>},
               core:run_parser(Parser, <<"[1,2] rest">>)).

object_empty_test() ->
  Parser = json:object_parser(),
  ?assertEqual({ok, {object, []}, <<" rest">>},
               core:run_parser(Parser, <<"{} rest">>)).

object_simple_test() ->
  Parser = json:object_parser(),
  Input = <<"{\"key\": 42} rest">>,
  ?assertEqual({ok,
                {object, [{<<"key">>, {number, 42}}]},
                <<" rest">>},
               core:run_parser(Parser, Input)).

%% Complex nested structures
nested_test() ->
  Parser = json:json_parser(),
  Input =
    <<"{
        \"name\": \"test\",
        \"values\": [1, 2, 3],
        \"nested\": {
            \"key\": true,
            \"array\": [null, false]
        }
    }">>,
  {ok, {object, Properties}, _} =
    core:run_parser(Parser, Input),
  ?assertEqual(3, length(Properties)),
  ?assertMatch({_, {string, <<"test">>}},
               lists:keyfind(<<"name">>, 1, Properties)),
  ?assertMatch({_,
                {array, [{number, 1}, {number, 2}, {number, 3}]}},
               lists:keyfind(<<"values">>, 1, Properties)).

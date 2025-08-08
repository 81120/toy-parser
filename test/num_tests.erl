-module(num_tests).

-include_lib("eunit/include/eunit.hrl").

%% Digit parsing
digits_test() ->
  Parser = num:digits_parser(),
  ?assertEqual({ok, <<"123">>, <<" rest">>},
               core:run_parser(Parser, <<"123 rest">>)).

digits_fail_test() ->
  Parser = num:digits_parser(),
  ?assertMatch({error, _, _},
               core:run_parser(Parser, <<"abc">>)).

%% Integer parsing
int_positive_test() ->
  Parser = num:int_parser(),
  ?assertEqual({ok, 123, <<" rest">>},
               core:run_parser(Parser, <<"123 rest">>)).

int_negative_test() ->
  Parser = num:int_parser(),
  ?assertEqual({ok, -123, <<" rest">>},
               core:run_parser(Parser, <<"-123 rest">>)).

int_fail_test() ->
  Parser = num:int_parser(),
  ?assertMatch({error, _, _},
               core:run_parser(Parser, <<"abc">>)).

%% Float parsing
float_positive_test() ->
  Parser = num:float_parser(),
  ?assertEqual({ok, 123.45, <<" rest">>},
               core:run_parser(Parser, <<"123.45 rest">>)).

float_negative_test() ->
  Parser = num:float_parser(),
  ?assertEqual({ok, -123.45, <<" rest">>},
               core:run_parser(Parser, <<"-123.45 rest">>)).

float_fail_test() ->
  Parser = num:float_parser(),
  ?assertMatch({error, _, _},
               core:run_parser(Parser, <<"123">>)).

%% Number parsing (either int or float)
number_int_test() ->
  Parser = num:number_parser(),
  ?assertEqual({ok, 123, <<" rest">>},
               core:run_parser(Parser, <<"123 rest">>)).

number_float_test() ->
  Parser = num:number_parser(),
  ?assertEqual({ok, 123.45, <<" rest">>},
               core:run_parser(Parser, <<"123.45 rest">>)).

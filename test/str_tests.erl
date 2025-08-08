-module(str_tests).

-include_lib("eunit/include/eunit.hrl").

%% Basic string matching
satisfy_match_test() ->
  Parser = str:satisfy(<<"hello">>),
  ?assertEqual({ok, <<"hello">>, <<" world">>},
               core:run_parser(Parser, <<"hello world">>)).

satisfy_no_match_test() ->
  Parser = str:satisfy(<<"hello">>),
  ?assertMatch({error, _, _},
               core:run_parser(Parser, <<"goodbye">>)).

%% Whitespace handling
trim_ws_test() ->
  Parser = str:trim_ws(),
  ?assertEqual({ok, "", <<"hello">>},
               core:run_parser(Parser, <<"   hello">>)).

trim_ws_empty_test() ->
  Parser = str:trim_ws(),
  ?assertEqual({ok, "", <<>>},
               core:run_parser(Parser, <<"   ">>)).

%% Symbol parsing (with whitespace)
symbol_test() ->
  Parser =
    str:symbol(
      str:satisfy(<<"hello">>)),
  ?assertEqual({ok, <<"hello">>, <<"world">>},
               core:run_parser(Parser, <<"  hello   world">>)).

symbol_fail_test() ->
  Parser =
    str:symbol(
      str:satisfy(<<"hello">>)),
  ?assertMatch({error, _, _},
               core:run_parser(Parser, <<"  goodbye  ">>)).

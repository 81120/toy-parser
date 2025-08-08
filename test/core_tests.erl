-module(core_tests).

-include_lib("eunit/include/eunit.hrl").

%% Basic parser operations
pure_test() ->
  Parser = core:pure(42),
  ?assertEqual({ok, 42, <<"input">>},
               core:run_parser(Parser, <<"input">>)).

fail_test() ->
  Parser = core:fail(<<"error message">>),
  ?assertEqual({error, <<"error message">>, <<"input">>},
               core:run_parser(Parser, <<"input">>)).

%% Monadic operations
bind_test() ->
  Parser =
    core:bind(
      core:pure(42),
      fun(X) ->
         core:pure(X * 2)
      end),
  ?assertEqual({ok, 84, <<"input">>},
               core:run_parser(Parser, <<"input">>)).

fmap_test() ->
  Parser =
    core:fmap(
      core:pure(42),
      fun(X) ->
         X * 2
      end),
  ?assertEqual({ok, 84, <<"input">>},
               core:run_parser(Parser, <<"input">>)).

%% Alternative combinators
alternative_empty_test() ->
  Parser = core:alternative([]),
  ?assertEqual({error,
                <<"No alternatives">>,
                <<"input">>},
               core:run_parser(Parser, <<"input">>)).

alternative_success_test() ->
  Parser =
    core:alternative([core:fail(<<"first error">>),
                      core:pure(42),
                      core:pure(84)]),
  ?assertEqual({ok, 42, <<"input">>},
               core:run_parser(Parser, <<"input">>)).

alternative_all_fail_test() ->
  Parser =
    core:alternative([core:fail(<<"first error">>),
                      core:fail(<<"second error">>)]),
  {error, _, <<"input">>} =
    core:run_parser(Parser, <<"input">>).

%% Repetition combinators
zero_or_more_test() ->
  % Create a parser that matches a specific character
  CharParser =
    fun(Input) ->
       case Input of
         <<"a", Rest/binary>> ->
           {ok, $a, Rest};
         _ ->
           {error, <<"expected 'a'">>, Input}
       end
    end,
  Parser = core:zero_or_more(CharParser),
  {ok, Values, Rest} =
    core:run_parser(Parser, <<"aaa rest">>),
  ?assertEqual([$a, $a, $a], Values),
  ?assertEqual(<<" rest">>, Rest).

one_or_more_success_test() ->
  Parser =
    core:one_or_more(
      core:pure(1)),
  {ok, [1 | Rest], _} =
    core:run_parser(Parser, <<"input">>),
  ?assert(is_list(Rest)).

one_or_more_fail_test() ->
  Parser =
    core:one_or_more(
      core:fail(<<"error">>)),
  ?assertEqual({error,
                <<"Expected one or more">>,
                <<"input">>},
               core:run_parser(Parser, <<"input">>)).

%% Sequence combinators
seq_map_test() ->
  Parser =
    core:seq_map([core:pure(1),
                  core:pure(2),
                  core:pure(3)]),
  {ok, Values, _} = core:run_parser(Parser, <<"input">>),
  ?assertEqual([3, 2, 1], Values).

seq_reduce_test() ->
  Parser =
    core:seq_reduce([core:pure(1),
                     core:pure(2),
                     core:pure(3)],
                    0,
                    fun(Acc, Val) ->
                       Acc + Val
                    end),
  ?assertEqual({ok, 6, <<"input">>},
               core:run_parser(Parser, <<"input">>)).

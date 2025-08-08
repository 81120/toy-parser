-module(parsec).

-export([alternative/2,
         flat_map/2,
         fmap/2,
         main/0,
         pure/1,
         run_parser/2,
         sign_parser/0,
         str_parser/1]).

%% A simple parser combinator library in Erlang.
run_parser(Parser, Input) when is_binary(Input) ->
  Parser(Input).

pure(Val) ->
  fun(Input) when is_binary(Input) ->
     {ok, Val, Input}
  end.

flat_map(Parser, Fun) ->
  fun(Input) when is_binary(Input) ->
     case run_parser(Parser, Input) of
       {ok, Val, Rest} ->
         run_parser(Fun(Val), Rest);
       {error, _} = Error ->
         Error
     end
  end.

fmap(Parser, Fun) ->
  flat_map(Parser,
           fun(Val) ->
              pure(Fun(Val))
           end).

alternative(Parser1, Parser2) ->
  fun(Input) when is_binary(Input) ->
     case run_parser(Parser1, Input) of
       {ok, Val, Rest} ->
         {ok, Val, Rest};
       {error, _} ->
         run_parser(Parser2, Input)
     end
  end.

str_parser(Str) when is_binary(Str) ->
  fun(Input) when is_binary(Input) ->
     StrLen = byte_size(Str),
     case Input of
       <<Prefix:StrLen/binary, Rest/binary>>
         when Prefix =:= Str ->
         {ok, Str, Rest};
       _ ->
         {error, not_matched}
     end
  end.

const(Val) ->
  fun(_) ->
     Val
  end.

sign_parser() ->
  alternative(fmap(str_parser(<<"-">>), const(-1)),
              alternative(fmap(str_parser(<<"+">>), const(1)),
                          pure(1))).

main() ->
  Parser =
    alternative(str_parser(<<"hello">>),
                str_parser(<<"world">>)),
  io:format("~p~n",
            [run_parser(Parser, <<"hello, parser!">>)]),
  io:format("~p~n",
            [run_parser(Parser, <<"world, parser!">>)]),
  io:format("~p~n", [run_parser(Parser, <<"unknown">>)]),
  io:format("~p~n",
            [run_parser(sign_parser(), <<"1235">>)]).

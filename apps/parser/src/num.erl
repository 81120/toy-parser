-module(num).

-export([digits_parser/0,
         float_parser/0,
         int_parser/0,
         number_parser/0]).

sign_parser() ->
  core:alternative([str:satisfy(<<"+">>),
                    str:satisfy(<<"-">>),
                    core:pure(<<"+">>)]).

read_digits(<<C, Rest/binary>>, Acc)
  when C >= $0 andalso C =< $9 ->
  read_digits(Rest, <<Acc/binary, C>>);
read_digits(Rest, Acc) ->
  {Acc, Rest}.

read_digits(Input) ->
  read_digits(Input, <<>>).

digits_parser() ->
  fun(Input) when is_binary(Input) ->
     {Digits, Rest} = read_digits(Input),
     case Digits of
       <<>> ->
         {error, <<"Expected digits">>, Input};
       _ ->
         {ok, Digits, Rest}
     end
  end.

int_parser() ->
  core:fmap(
    core:seq_reduce([sign_parser(), digits_parser()],
                    <<>>,
                    fun(Acc, Val) ->
                       <<Acc/binary, Val/binary>>
                    end),
    fun(X) ->
       binary_to_integer(X)
    end).

float_parser() ->
  core:fmap(
    core:seq_reduce([sign_parser(),
                     digits_parser(),
                     str:satisfy(<<".">>),
                     digits_parser()],
                    <<>>,
                    fun(Acc, Val) ->
                       <<Acc/binary, Val/binary>>
                    end),
    fun(X) ->
       binary_to_float(X)
    end).

number_parser() ->
  core:alternative(float_parser(), int_parser()).

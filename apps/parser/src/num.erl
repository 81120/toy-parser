-module(num).

%% Number parsing module
%% Provides parsers for parsing numbers including:
%% * Integers (with optional sign)
%% * Floating point numbers
%% * Sequences of digits
%% All parsers handle leading and trailing whitespace

-export([digits_parser/0,
         float_parser/0,
         int_parser/0,
         number_parser/0]).

%% Types
-type parser() ::
  fun((binary()) ->
        {ok, any(), binary()} | {error, any(), binary()}).

%% Type specifications
-spec digits_parser() -> parser().
-spec float_parser() -> parser().
-spec int_parser() -> parser().
-spec number_parser() -> parser().
%% Parse number signs (+/-) with + as default
-spec sign_parser() -> parser().
sign_parser() ->
  core:alternative([str:satisfy(<<"+">>),
                    str:satisfy(<<"-">>),
                    core:pure(<<"+">>)]).

%% Read consecutive digits from input
-spec read_digits(binary(), binary()) ->
                   {binary(), binary()}.
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

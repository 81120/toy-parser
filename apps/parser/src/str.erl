-module(str).

%% String parsing utilities
%% Provides functions for:
%% * Exact string matching
%% * String literal parsing with escapes
%% * Whitespace handling
%% * Symbol parsing (tokens with whitespace)

-export([read_string/1,
         satisfy/1,
         symbol/1,
         trim_ws/0]).

%% Types
-type parser() :: core:parser().

%% Match an exact string
-spec satisfy(binary()) -> parser().
satisfy(Str) when is_binary(Str) ->
  fun(Input) when is_binary(Input) ->
     StrLen = byte_size(Str),
     case Input of
       <<Prefix:StrLen/binary, Rest/binary>>
         when Prefix =:= Str ->
         {ok, Str, Rest};
       _ ->
         {error, <<"Expected '", Str/binary, "'">>, Input}
     end
  end.

%% Remove leading whitespace from binary
%% Whitespace includes space, tab, CR, and LF
-spec trim_leading_ws(binary()) -> binary().
trim_leading_ws(<<C, Rest/binary>>)
  when C == 32
       orelse C == $\t
       orelse C == $\r
       orelse C == $\n ->
  trim_leading_ws(Rest);
trim_leading_ws(Rest) ->
  Rest.

%% Parser that consumes leading whitespace
-spec trim_ws() -> parser().
trim_ws() ->
  fun(Input) when is_binary(Input) ->
     {ok, "", trim_leading_ws(Input)}
  end.

%% Wraps a parser with whitespace handling
%% The resulting parser will consume whitespace before and after the main parser
-spec symbol(core:parser()) -> parser().
symbol(Parser) ->
  core:omit_left(trim_ws(),
                 core:omit_right(Parser, trim_ws())).

read_string(<<C, Rest/binary>>, Acc, false)
  when C =:= $\" ->
  read_string(Rest, Acc, true);
read_string(<<C, Rest/binary>>, Acc, true)
  when C =:= $\" ->
  {ok, Acc, Rest};
read_string(<<C, Rest/binary>>, Acc, true)
  when C =/= $\" ->
  read_string(Rest, <<Acc/binary, C>>, true);
read_string(Rest, _, _) ->
  {error, <<>>, Rest}.

read_string(Str) ->
  read_string(Str, <<>>, false).

-module(str).

-export([read_string/1,
         satisfy/1,
         symbol/1,
         trim_ws/0]).

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

trim_leading_ws(<<C, Rest/binary>>)
  when C == 32
       orelse C == $\t
       orelse C == $\r
       orelse C == $\n ->
  trim_leading_ws(Rest);
trim_leading_ws(Rest) ->
  Rest.

trim_ws() ->
  fun(Input) when is_binary(Input) ->
     {ok, "", trim_leading_ws(Input)}
  end.

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

-module(core).

-export([alternative/1,
         alternative/2,
         bind/2,
         fail/1,
         fmap/2,
         lazy/1,
         omit_all_left/1,
         omit_left/2,
         omit_right/2,
         one_or_more/1,
         pure/1,
         run_parser/2,
         sep_by/2,
         seq_map/1,
         seq_reduce/3,
         zero_or_more/1]).

run_parser(Parser, Input) when is_binary(Input) ->
  Parser(Input).

pure(Val) ->
  fun(Input) when is_binary(Input) ->
     {ok, Val, Input}
  end.

fail(Err) ->
  fun(Input) when is_binary(Input) ->
     {error, Err, Input}
  end.

bind(Parser, Func) ->
  fun(Input) when is_binary(Input) ->
     case run_parser(Parser, Input) of
       {ok, Val, Rest} ->
         run_parser(Func(Val), Rest);
       Error ->
         Error
     end
  end.

fmap(Parser, Func) ->
  bind(Parser,
       fun(Val) ->
          pure(Func(Val))
       end).

alternative([]) ->
  fail(<<"No alternatives">>);
alternative([P]) ->
  P;
alternative([P | Ps]) ->
  alternative(P, alternative(Ps)).

alternative(Pa, Pb) ->
  fun(Input) when is_binary(Input) ->
     case run_parser(Pa, Input) of
       {ok, _, _} = Res ->
         Res;
       _ ->
         run_parser(Pb, Input)
     end
  end.

omit_left(Pa, Pb) ->
  bind(Pa,
       fun(_) ->
          Pb
       end).

omit_right(Pa, Pb) ->
  bind(Pa,
       fun(Res) ->
          omit_left(Pb, pure(Res))
       end).

omit_all_left([]) ->
  fail(<<"No parsers">>);
omit_all_left([P]) ->
  P;
omit_all_left([P | Ps]) ->
  omit_left(P, omit_all_left(Ps)).

zero_or_more(Parser) ->
  fun(Input) when is_binary(Input) ->
     case run_parser(Parser, Input) of
       {ok, Val, Rest} ->
         case run_parser(zero_or_more(Parser), Rest) of
           {ok, Values, FinalRest} ->
             {ok, [Val | Values], FinalRest}
         end;
       _ ->
         {ok, [], Input}
     end
  end.

one_or_more(Parser) ->
  fun(Input) when is_binary(Input) ->
     case run_parser(Parser, Input) of
       {ok, Val, Rest} ->
         case run_parser(zero_or_more(Parser), Rest) of
           {ok, Values, FinalRest} ->
             {ok, [Val | Values], FinalRest}
         end;
       _ ->
         {error, <<"Expected one or more">>, Input}
     end
  end.

sep_by(Parser, Sep) ->
  fun(Input) when is_binary(Input) ->
     case run_parser(Parser, Input) of
       {ok, Val, Rest} ->
         case run_parser(zero_or_more(omit_left(Sep, Parser)),
                         Rest)
         of
           {ok, Values, FinalRest} ->
             {ok, [Val | Values], FinalRest}
         end;
       _ ->
         {error, <<"Expected at least one">>, Input}
     end
  end.

seq_map([], Acc) ->
  pure(Acc);
seq_map([P | Ps], Acc) ->
  bind(P,
       fun(Val) ->
          seq_map(Ps, [Val | Acc])
       end).

seq_map(Ps) when is_list(Ps) ->
  seq_map(Ps, []).

seq_reduce([], Acc, _) ->
  pure(Acc);
seq_reduce([P | Ps], Acc, Func) ->
  bind(P,
       fun(Val) ->
          seq_reduce(Ps, Func(Acc, Val), Func)
       end).

lazy(Func) ->
  fun(Input) when is_binary(Input) ->
     P = Func(),
     run_parser(P, Input)
  end.

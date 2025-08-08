-module(core).

%% Core module implementing parser combinators
%% This module provides the fundamental building blocks for creating parsers:
%% * Monadic operations (bind, pure)
%% * Alternatives and choice
%% * Repetition (zero_or_more, one_or_more)
%% * Sequences and combinations

-type parser() ::
  fun((binary()) ->
        {ok, any(), binary()} | {error, any(), binary()}).

-export_type([parser/0]).

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

%% Run a parser with input
-spec run_parser(parser(), binary()) ->
                  {ok, term(), binary()} | {error, term(), binary()}.
run_parser(Parser, Input) when is_binary(Input) ->
  Parser(Input).

%% Create a parser that always succeeds with a value
-spec pure(Val :: term()) -> parser().
pure(Val) ->
  fun(Input) when is_binary(Input) ->
     {ok, Val, Input}
  end.

%% Create a parser that always fails with an error
-spec fail(Err :: binary()) -> parser().
fail(Err) ->
  fun(Input) when is_binary(Input) ->
     {error, Err, Input}
  end.

%% Monadic bind operation
%% Creates a new parser by chaining the result of one parser to another
-spec bind(parser(), fun((term()) -> parser())) ->
            parser().
bind(Parser, Func) ->
  fun(Input) when is_binary(Input) ->
     case run_parser(Parser, Input) of
       {ok, Val, Rest} ->
         run_parser(Func(Val), Rest);
       Error ->
         Error
     end
  end.

%% Functor mapping operation
%% Applies a function to the result of a parser if it succeeds
-spec fmap(parser(), fun((term()) -> term())) ->
            parser().
fmap(Parser, Func) ->
  bind(Parser,
       fun(Val) ->
          pure(Func(Val))
       end).

%% Tries multiple parsers in sequence until one succeeds
-spec alternative([parser()]) -> parser().
alternative([]) ->
  fail(<<"No alternatives">>);
alternative([P]) ->
  P;
alternative([P | Ps]) ->
  alternative(P, alternative(Ps)).

%% Tries one parser, if it fails tries the second parser
-spec alternative(parser(), parser()) -> parser().
alternative(Pa, Pb) ->
  fun(Input) when is_binary(Input) ->
     case run_parser(Pa, Input) of
       {ok, _, _} = Res ->
         Res;
       _ ->
         run_parser(Pb, Input)
     end
  end.

%% Runs two parsers in sequence, discarding the result of the first
-spec omit_left(parser(), parser()) -> parser().
omit_left(Pa, Pb) ->
  bind(Pa,
       fun(_) ->
          Pb
       end).

%% Runs two parsers in sequence, discarding the result of the second
-spec omit_right(parser(), parser()) -> parser().
omit_right(Pa, Pb) ->
  bind(Pa,
       fun(Res) ->
          omit_left(Pb, pure(Res))
       end).

%% Runs multiple parsers in sequence, keeping only the last result
-spec omit_all_left([parser()]) -> parser().
omit_all_left([]) ->
  fail(<<"No parsers">>);
omit_all_left([P]) ->
  P;
omit_all_left([P | Ps]) ->
  omit_left(P, omit_all_left(Ps)).

%% Parse zero or more occurrences of a pattern
-spec zero_or_more(parser()) -> parser().
zero_or_more(Parser) ->
  fun(Input) when is_binary(Input) ->
     case run_parser(Parser, Input) of
       {ok, Val, Rest} ->
         case run_parser(zero_or_more(Parser), Rest) of
           {ok, Values, FinalRest} when is_list(Values) ->
             {ok, [Val | Values], FinalRest}
         end;
       _ ->
         {ok, [], Input}
     end
  end.

%% Parse one or more occurrences of a pattern
-spec one_or_more(parser()) -> parser().
one_or_more(Parser) ->
  fun(Input) when is_binary(Input) ->
     case run_parser(Parser, Input) of
       {ok, Val, Rest} ->
         case run_parser(zero_or_more(Parser), Rest) of
           {ok, Values, FinalRest} when is_list(Values) ->
             {ok, [Val | Values], FinalRest}
         end;
       _ ->
         {error, <<"Expected one or more">>, Input}
     end
  end.

%% Parse items separated by a separator
-spec sep_by(parser(), parser()) -> parser().
sep_by(Parser, Sep) ->
  fun(Input) when is_binary(Input) ->
     case run_parser(Parser, Input) of
       {ok, Val, Rest} ->
         case run_parser(zero_or_more(omit_left(Sep, Parser)),
                         Rest)
         of
           {ok, Values, FinalRest} when is_list(Values) ->
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

%% Creates a parser that is evaluated lazily
%% Useful for recursive parser definitions
-spec lazy(fun(() -> parser())) -> parser().
lazy(Func) ->
  fun(Input) when is_binary(Input) ->
     run_parser(Func(), Input)
  end.

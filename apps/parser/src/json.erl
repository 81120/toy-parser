%% JSON parser implementation using parser combinators
-module(json).

%% Types
-type json_value() ::
  {null, binary()} |
  {boolean, boolean()} |
  {number, number()} |
  {string, binary()} |
  {array, [json_value()]} |
  {object, [{binary(), json_value()}]}.

-export_type([json_value/0]).

-export([array_parser/0,
         boolean_parser/0,
         false_parser/0,
         json_parser/0,
         null_parser/0,
         number_parser/0,
         object_parser/0,
         string_parser/0,
         true_parser/0]).

%% Function specifications
-spec null_parser() -> core:parser().
-spec true_parser() -> core:parser().
-spec false_parser() -> core:parser().
-spec boolean_parser() -> core:parser().
-spec number_parser() -> core:parser().
-spec string_parser() -> core:parser().
-spec array_parser() -> core:parser().
-spec object_parser() -> core:parser().
-spec json_parser() -> core:parser().
null_parser() ->
  core:fmap(
    str:symbol(
      str:satisfy(<<"null">>)),
    fun(_) ->
       {null, <<>>}
    end).

true_parser() ->
  core:fmap(
    str:symbol(
      str:satisfy(<<"true">>)),
    fun(_) ->
       {boolean, true}
    end).

false_parser() ->
  core:fmap(
    str:symbol(
      str:satisfy(<<"false">>)),
    fun(_) ->
       {boolean, false}
    end).

boolean_parser() ->
  core:alternative(true_parser(), false_parser()).

number_parser() ->
  core:fmap(
    str:symbol(
      num:number_parser()),
    fun(X) ->
       {number, X}
    end).

string_parser() ->
  core:fmap(
    str:symbol(fun(Input) when is_binary(Input) ->
                  case str:read_string(Input) of
                    {ok, Str, Rest} ->
                      {ok, Str, Rest};
                    {error, Err, Rest} ->
                      {error, Err, Rest}
                  end
               end),
    fun(X) ->
       {string, X}
    end).

json_parser() ->
  core:alternative([core:lazy(fun() ->
                                 object_parser()
                              end),
                    core:lazy(fun() ->
                                 array_parser()
                              end),
                    string_parser(),
                    number_parser(),
                    boolean_parser(),
                    null_parser()]).

array_parser() ->
  core:fmap(
    str:symbol(
      core:omit_left(
        str:satisfy(<<"[">>),
        core:omit_right(
          core:sep_by(json_parser(), str:satisfy(<<",">>)),
          str:satisfy(<<"]">>)))),
    fun(Elements) ->
       {array, Elements}
    end).

object_parser() ->
  core:fmap(
    str:symbol(
      core:omit_left(
        str:satisfy(<<"{">>),
        core:omit_right(
          core:sep_by(
            core:bind(string_parser(),
                      fun({string, Key}) ->
                         core:omit_left(
                           str:satisfy(<<":">>),
                           core:fmap(json_parser(),
                                     fun({Type, Val}) ->
                                        {Key, {Type, Val}}
                                     end))
                      end),
            str:satisfy(<<",">>)),
          str:satisfy(<<"}">>)))),
    fun(Pairs) ->
       {object, Pairs}
    end).

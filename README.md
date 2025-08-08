# parser

An OTP application

## Build

    $ rebar3 compile

## Demo

```
1> {ok, Json} = file:read("./package.json").
** exception error: undefined function file:read/1
2> {ok, Json} = file:read_file("./package.json").
{ok,<<"{\n  \"name\": \"parsec\",\n  \"version\": \"1.0.0\",\n  \"main\": \"index.js\",\n  \"scripts\": {\n    \"build\": \"tsc\",\n    \"de"...>>}
3> core:run_parser(json:json_parser(), Json).
{ok,{object,[{<<"name">>,{string,<<"parsec">>}},
             {<<"version">>,{string,<<"1.0.0">>}},
             {<<"main">>,{string,<<"index.js">>}},
             {<<"scripts">>,
              {object,[{<<"build">>,{string,<<"tsc">>}},
                       {<<"dev">>,{string,<<"node ./dist/index.js">>}}]}},
             {<<"author">>,{string,<<>>}},
             {<<"license">>,{string,<<"ISC">>}},
             {<<"description">>,{string,<<>>}},
             {<<"devDependencies">>,
              {object,[{<<"typescript">>,{string,<<"^5.8.2">>}}]}},
             {<<"functions">>,
              {array,[{object,[{<<"name">>,{string,<<"parse">>}},
                               {<<"description">>,
                                {string,<<"Parses a str"...>>}},
                               {<<"parameters">>,{array,[{object,...}]}},
                               {<<"returns">>,{object,[{...}|...]}}]},
                      {string,<<"TsBuildIn">>},
                      {number,20},
                      {object,[{<<"enable">>,{boolean,true}}]},
                      {object,[{<<"extra">>,{null,<<>>}}]}]}}]},
    <<>>}
4>
```

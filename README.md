# Erlang Parser Combinator Library

A parser combinator library implemented in Erlang, featuring composable parsers for building complex parsers from simple ones.

## Features

- **Modular Design**: Core parser combinators separated from specific parsers
- **String Parsing**: Flexible string and character parsing utilities
- **Number Parsing**: Support for integers, floats, and custom number formats
- **JSON Parser**: Complete JSON parser implementation as a usage example
- **Type Safety**: Comprehensive type specifications for all functions
- **Well Documented**: Detailed documentation and usage examples
- **Zero Dependencies**: Pure Erlang implementation

## Getting Started

See the [User Guide](docs/guide.md) for detailed documentation and examples.

## Quick Start

### Installation

Add to your `rebar.config`:

```erlang
{deps, [
    {parser, {git, "https://github.com/81120/toy-parser.git", {tag, "1.0.0"}}}
]}.
```

### Basic Usage

Create and use a simple parser:

```erlang
% String parsing
1> Parser = str:satisfy(<<"hello">>).
2> core:run_parser(Parser, <<"hello world">>).
{ok, <<"hello">>, <<" world">>}

% Number parsing
3> NumParser = num:number_parser().
4> core:run_parser(NumParser, <<"42.5 rest">>).
{ok, 42.5, <<" rest">>}

% JSON parsing
5> JsonParser = json:json_parser().
6> Input = <<"{\"name\":\"test\",\"value\":123}">>.
7> {ok, Result, _} = core:run_parser(JsonParser, Input).
{ok, {object, [{<<"name">>, {string, <<"test">>}},
               {<<"value">>, {number, 123}}]},
     <<>>}
```

### Building Complex Parsers

Combine parsers to create more complex ones:

```erlang
% Parse "key: value" pairs
KeyValueParser = core:bind(
    str:satisfy(<<"key">>),
    fun(Key) ->
        core:bind(
            str:satisfy(<<": ">>),
            fun(_) ->
                core:fmap(
                    str:satisfy(<<"value">>),
                    fun(Value) -> {Key, Value} end
                )
            end
        )
    end
).
```

## Build

```bash
$ rebar3 compile
```

## Documentation

- [User Guide](docs/guide.md) - Comprehensive documentation with examples
- [API Reference](docs/api.md) - Detailed API documentation
- Module specs - Available through `erl -man`

## Project Structure

```
apps/parser/
├── src/
│   ├── core.erl    # Core parser combinators
│   ├── str.erl     # String parsing utilities
│   ├── num.erl     # Number parsing
│   └── json.erl    # JSON parser implementation
└── test/           # Unit tests
```

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create your feature branch
3. Add tests for new features
4. Ensure all tests pass
5. Submit a pull request

## License

MIT License - see [LICENSE.md](LICENSE.md) for details

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

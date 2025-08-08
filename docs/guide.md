# Erlang Parser Combinator Library Guide

## Overview

This is a parser combinator library implemented in Erlang. Parser combinators are a way to build complex parsers by combining smaller, simpler parsers. The library provides a set of basic parsers and combinators that can be used to build more complex parsers.

## Project Structure

The project consists of several modules:

### Core Module (`core.erl`)

The foundation of the parser combinator library, providing:

- Basic parser types and operations
- Monadic operations (bind, pure)
- Parser combinators (alternative, sequence, repetition)
- Helper functions for parser composition

Key functions:

```erlang
pure(Value) -> parser()          % Create a parser that always succeeds with a value
bind(Parser, Function) -> parser() % Chain parsers together
fmap(Parser, Function) -> parser() % Transform parser results
alternative(Parsers) -> parser()   % Try multiple parsers in sequence
```

### String Module (`str.erl`)

Provides string-specific parsing utilities:

- Exact string matching
- Whitespace handling
- Symbol parsing (tokens with whitespace)

Key functions:

```erlang
satisfy(String) -> parser()     % Match an exact string
symbol(Parser) -> parser()      % Parse with whitespace handling
trim_ws() -> parser()          % Consume whitespace
```

### Number Module (`num.erl`)

Handles numeric parsing:

- Integer parsing
- Float parsing
- Sign handling
- Digit sequence parsing

Key functions:

```erlang
int_parser() -> parser()       % Parse integers
float_parser() -> parser()     % Parse floating point numbers
number_parser() -> parser()    % Parse any number
```

### JSON Module (`json.erl`)

Implements a JSON parser using the combinator library:

- Full JSON value parsing
- Object and array support
- String literals
- Numbers
- Boolean and null values

Key functions:

```erlang
json_parser() -> parser()      % Parse any JSON value
object_parser() -> parser()    % Parse JSON objects
array_parser() -> parser()     % Parse JSON arrays
```

## Usage Examples

### Basic String Parsing

```erlang
% Parse a simple string
Parser = str:satisfy(<<"hello">>),
{ok, <<"hello">>, Rest} = core:run_parser(Parser, <<"hello world">>).

% Parse with whitespace
Parser2 = str:symbol(str:satisfy(<<"hello">>)),
{ok, <<"hello">>, Rest2} = core:run_parser(Parser2, <<"  hello  world">>).
```

### Number Parsing

```erlang
% Parse an integer
Parser = num:int_parser(),
{ok, 123, Rest} = core:run_parser(Parser, <<"123 rest">>).

% Parse a float
Parser2 = num:float_parser(),
{ok, 123.45, Rest2} = core:run_parser(Parser2, <<"123.45 rest">>).
```

### JSON Parsing

```erlang
% Parse a JSON object
Parser = json:json_parser(),
Input = <<"{\"name\": \"test\", \"value\": 123}">>,
{ok, {object, [{<<"name">>, {string, <<"test">>}},
              {<<"value">>, {number, 123}}]}, Rest} = core:run_parser(Parser, Input).
```

## Building Complex Parsers

The library's power comes from combining parsers. Here's how to build more complex parsers:

### Sequence of Parsers

```erlang
% Parse "key: value"
KeyValueParser = core:seq_reduce(
    [str:satisfy(<<"key">>),
     str:satisfy(<<":">>),
     str:satisfy(<<"value">>)],
    <<>>,
    fun(Acc, Val) -> <<Acc/binary, Val/binary>> end
).
```

### Alternative Parsers

```erlang
% Parse either "yes" or "no"
YesNoParser = core:alternative([
    str:satisfy(<<"yes">>),
    str:satisfy(<<"no">>)
]).
```

### Repetition

```erlang
% Parse comma-separated values
CommaSepParser = core:sep_by(
    str:satisfy(<<"value">>),
    str:satisfy(<<",">>)
).
```

## Error Handling

Parsers return either:

- `{ok, Value, Rest}` on success
- `{error, Message, Rest}` on failure

Error messages include the expected input and the remaining unparsed input for debugging.

## Best Practices

1. **Composition**: Build complex parsers from simple ones
2. **Whitespace**: Use `str:symbol/1` to handle whitespace automatically
3. **Error Messages**: Provide meaningful error messages in your parsers
4. **Type Specifications**: Use the provided type specs for better code clarity
5. **Testing**: Test parsers with both valid and invalid inputs

## Contributing

Contributions are welcome! Please ensure:

- Tests are included
- Type specifications are provided
- Documentation is updated
- Code follows the existing style

## License

This project is licensed under MIT License. See LICENSE.md for details.

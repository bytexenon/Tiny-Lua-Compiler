# TLC API Reference

TLC provides two levels of API access: high-level functions that handle the full pipeline, and low-level functions that let you chain individual stages manually.

## High-Level API

These functions take Lua source code as a string and handle everything internally.

### `tlc.run(code)`

Compiles and executes Lua code in one step. Returns whatever the code returns.

```lua
local tlc = require("tlc")
tlc.run("print('Hello, World!')")
-- Output: Hello, World!

local result = tlc.run("return 2 + 2")
print(result) -- 4
```

### `tlc.compile(code)`

Compiles Lua source code to a Lua 5.1 bytecode string. The output is binary-compatible with `luac` and can be loaded with `loadstring` or saved to a `.luac` file.

```lua
local bytecode = tlc.compile("print('test')")

-- Save to a file:
local f = io.open("output.luac", "wb")
f:write(bytecode)
f:close()
```

### `tlc.compileToProto(code)`

Compiles Lua source code to a function prototype table (TLC's internal representation). This is useful if you want to inspect the compiled output or execute it with TLC's VM.

```lua
local proto = tlc.compileToProto("return 2 + 2")

-- proto.code:         list of bytecode instructions
-- proto.constants:    constant pool (numbers, strings)
-- proto.maxStackSize: register count needed
-- proto.numParams:    number of parameters
-- proto.numUpvals:    number of upvalues
-- proto.isVararg:     whether the function uses ...
-- proto.protos:       nested function prototypes
```

### `tlc.parse(code)`

Tokenizes and parses Lua source code, returning an Abstract Syntax Tree (AST). See [ast.md](ast.md) for the full node specification.

```lua
local ast = tlc.parse("local x = 42")
-- Returns:
-- {
--   kind = "Program",
--   body = {
--     kind = "Block",
--     statements = {
--       {
--         kind = "LocalDeclarationStatement",
--         variables = {"x"},
--         initializers = {{kind = "NumericLiteral", value = 42, raw = "42"}}
--       }
--     }
--   }
-- }
```

### `tlc.tokenize(code)`

Tokenizes Lua source code and returns a list of token tables. Each token has a `kind` field, an optional `value` field, and an optional `raw` field (for numbers).

```lua
local tokens = tlc.tokenize("local x = 42")
-- Returns:
-- {
--   {kind = "Keyword",    value = "local"},
--   {kind = "Identifier", value = "x"},
--   {kind = "Equals"},
--   {kind = "Number",     value = 42, raw = "42"},
--   {kind = "EOF"}
-- }
```

**Token kinds:** `Keyword`, `Identifier`, `Number`, `String`, `Operator`, `Vararg`, `Dot`, `Equals`, `Colon`, `Semicolon`, `Comma`, `LeftParen`, `RightParen`, `LeftBrace`, `RightBrace`, `LeftBracket`, `RightBracket`, `EOF`.

## Low-Level API

These functions let you run individual pipeline stages. Each takes the output of the previous stage as input.

### `tlc.parseTokens(tokens)`

Parses a list of tokens (from `tlc.tokenize`) into an AST.

```lua
local tokens = tlc.tokenize("x = 1")
local ast    = tlc.parseTokens(tokens)
```

### `tlc.generate(ast)`

Compiles an AST (from `tlc.parse` or `tlc.parseTokens`) into a function prototype.

```lua
local ast   = tlc.parse("return 1 + 1")
local proto = tlc.generate(ast)
```

### `tlc.emit(proto)`

Serializes a function prototype into a Lua 5.1 bytecode string.

```lua
local proto    = tlc.compileToProto("print('hi')")
local bytecode = tlc.emit(proto)
```

### `tlc.execute(proto)`

Executes a function prototype in TLC's virtual machine. Returns whatever the prototype returns.

```lua
local proto  = tlc.compileToProto("return 42")
local result = tlc.execute(proto)
print(result) -- 42
```

## Class Constructors

For full control, you can instantiate each pipeline stage directly:

```lua
local tlc = require("tlc")

-- Tokenize
local tokenizer = tlc.Tokenizer.new("local x = 1 + 2")
local tokens    = tokenizer:tokenize()

-- Parse
local parser = tlc.Parser.new(tokens)
local ast    = parser:parse()

-- Compile
local generator = tlc.CodeGenerator.new(ast)
local proto     = generator:generate()

-- Emit bytecode
local emitter  = tlc.BytecodeEmitter.emit(proto)

-- Run the prototype in TLC's VM
local vm = tlc.VirtualMachine.execute(proto)
```

## Full Pipeline Example

```lua
local tlc = require("tlc")

local code = [[
  local function fibonacci(n)
    if n <= 1 then return n end
    return fibonacci(n - 1) + fibonacci(n - 2)
  end

  for i = 0, 10 do
    print(fibonacci(i))
  end
]]

-- One-liner
tlc.run(code)

-- Step-by-step
local tokens = tlc.tokenize(code)
local ast    = tlc.parseTokens(tokens)
local proto  = tlc.generate(ast)
tlc.execute(proto)

-- Compile to bytecode file
local bytecode = tlc.compile(code)
local f = io.open("fibonacci.luac", "wb")
f:write(bytecode)
f:close()
```

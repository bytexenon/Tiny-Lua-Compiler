<div align="center">

![Tiny Lua Compiler (TLC)](https://github.com/bytexenon/Tiny-Lua-Compiler/assets/125568681/41cf5285-e31d-4b27-a8a8-ee83a7300f1f)

**A minimal, educational Lua 5.1 compiler written in pure Lua**

_Inspired by [Jamie Kyle's The Super Tiny Compiler](https://github.com/jamiebuilds/the-super-tiny-compiler) written in JavaScript_

</div>

## Features

- **Heavily Commented & Educational**: The code is written to be read, with over 50% of the file dedicated to comments explaining the what, how, and why of each step.
- [**Self-compiling**](<https://en.wikipedia.org/wiki/Self-hosting_(compilers)>): TLC can compile itself and run the result inside its own VM (Virtual Machine).
- **Zero dependencies**: Written in standard Lua 5.1 with no external libraries. Just one file and the Lua interpreter are all you need.
- **Complete Pipeline:** Includes a tokenizer, parser, code generator, compiler, and a virtual machine, all in one file.
- **Speed**: While education is the priority, the tokenizer uses optimized lookups and the compiler is designed efficiently, making it quite fast for a compiler written in a high-level language.
- **Tests**: Includes a comprehensive test suite with nearly 100% coverage, ensuring the compiler is as reliable as it is educational.

### [Want to jump into the code? Click here](https://github.com/bytexenon/Tiny-Lua-Compiler/blob/main/tlc.lua)

---

### Why should I care?

That's fair, most people don't really have to think about compilers in their day
jobs. However, compilers are all around you, tons of the tools you use are based
on concepts borrowed from compilers.

### Why Lua?

Lua is a simple programming language that is easy to learn and use. It doesn't
have complex syntax or a lot of features, which makes it a great language to
make a compiler for.

### But compilers are scary!

Yes, they are. But that's our fault (the people who write compilers), we've
taken something that is reasonably straightforward and made it so scary that
most think of it as this totally unapproachable thing that only the nerdiest of
the nerds are able to understand.

### Example usage?

The compiler is written in a way that it can be used as a library.
Here is an example of how you can use it:

```lua
--[[
  EXAMPLE: COMPILE AND EXECUTE LUA CODE WITH TLC
  ----------------------------------------------
  Demonstrates the full compilation pipeline from
  source code to execution.
--]]

local tlc = require("tlc")

-- 1. Source code to compile
local source = [[
  for i = 1, 3 do
    print("Hello from TLC! " .. i)
  end
]]

-- 2. Compilation Pipeline
-- -----------------------

-- Tokenize: Raw text -> Stream of tokens
local tokens = tlc.tokenize(source)

-- Parse: Stream of tokens -> Abstract Syntax Tree (AST)
local ast = tlc.parseTokens(tokens)

-- Generate Code: AST -> Function Prototype (Intermediate Representation)
local proto = tlc.generate(ast)

-- 3. Execution
-- Run the prototype in the Virtual Machine
tlc.execute(proto)

--[[
  EXPECTED OUTPUT:
  Hello from TLC! 1
  Hello from TLC! 2
  Hello from TLC! 3
--]]
```

### Okay so where do I begin?

Awesome! Head on over to the [tlc.lua](https://github.com/bytexenon/Tiny-Lua-Compiler/blob/main/tlc.lua) file.

### What isn't covered? (Non-Goals)

Because TLC is designed to fit in a single file and be easily understood, we decided to leave out features that add significant complexity without teaching core compiler concepts:

- **Debug Symbols:** We don't strip line numbers or debug info, we never generate them! This drastically simplifies the Tokenizer and Parser.
- **[Constant Folding](https://en.wikipedia.org/wiki/Constant_folding):** Standard Lua converts `local x = 2 + 3` into `local x = 5` at compile time. TLC calculates this at runtime.
- **Unused Opcodes:** We skip `TESTSET` (it's just an optimization), and massive table constructors (over ~25k items).

Everything else should work just like standard Lua 5.1!

### Tests

Run the test suite with:

```bash
lua tests/test.lua
```

### AST Node Specifications

The parser generates a tree of nodes. Here is the specification for each node type.

<details>
<summary>Click to expand AST Specification</summary>

#### **Literals & Identifiers**
*   `{ kind = "Variable", name = <string>, variableType = <"Local" | "Global" | "Upvalue"> }`
*   `{ kind = "StringLiteral", value = <string> }`
*   `{ kind = "NumericLiteral", value = <number> }`
*   `{ kind = "BooleanLiteral", value = <bool> }`
*   `{ kind = "NilLiteral" }`
*   `{ kind = "VarargExpression" }`

#### **Expressions**
*   `{ kind = "FunctionExpression", body = <Block>, parameters = <list_of_strings>, isVarArg = <bool> }`
*   `{ kind = "UnaryOperator", operator = <string>, operand = <node> }`
*   `{ kind = "BinaryOperator", operator = <string>, left = <node>, right = <node> }`
*   `{ kind = "FunctionCall", callee = <node>, arguments = <list_of_nodes>, isMethodCall = <bool> }`
*   `{ kind = "IndexExpression", base = <node>, index = <node>, isPrecomputed = <bool>? }`
*   `{ kind = "TableConstructor", elements = <list_of_TableElement> }`
*   `{ kind = "TableElement", key = <node>, value = <node>, isImplicitKey = <bool> }`
*   `{ kind = "ParenthesizedExpression", expression = <node> }`

#### **Statements**
*   `{ kind = "LocalDeclarationStatement", variables = <list_of_strings>, initializers = <list_of_nodes> }`
*   `{ kind = "LocalFunctionDeclaration", name = <string>, body = <FunctionExpression> }`
*   `{ kind = "AssignmentStatement", lvalues = <list_of_nodes>, expressions = <list_of_nodes> }`
*   `{ kind = "CallStatement", expression = <FunctionCall> }`
*   `{ kind = "IfClause", condition = <node>, body = <Block> }`
*   `{ kind = "IfStatement", clauses = <list_of_IfClauses>, elseClause = <Block>? }`
*   `{ kind = "WhileStatement", condition = <node>, body = <Block> }`
*   `{ kind = "RepeatStatement", body = <Block>, condition = <node> }`
*   `{ kind = "ForNumericStatement", variable = <Identifier>, start = <node>, limit = <node>, step = <node>?, body = <Block> }`
*   `{ kind = "ForGenericStatement", iterators = <list_of_strings>, expressions = <list_of_nodes>, body = <Block> }`
*   `{ kind = "DoStatement", body = <Block> }`
*   `{ kind = "ReturnStatement", expressions = <list_of_nodes> }`
*   `{ kind = "BreakStatement" }`

#### **Program Structure**
*   `{ kind = "Block", statements = <list_of_statement> }`
*   `{ kind = "Program", body = <Block> }`

</details>

### Support The Tiny Lua Compiler (TLC)

I don't take donations, but you can support TLC by starring the repository and sharing it with others.
If you find a bug or have a feature request, feel free to open an issue or submit a pull request.
<div align="center">

![The Tiny Lua Compiler (TLC)](https://github.com/ByteXenon/TinyLua/assets/125568681/41cf5285-e31d-4b27-a8a8-ee83a7300f1f)

**A minimal, educational Lua 5.1 compiler written in Lua**

_Inspired by [Jamie Kyle's The Super Tiny Compiler](https://github.com/jamiebuilds/the-super-tiny-compiler) written in JavaScript_

</div>

## Features

- **Heavily Commented & Educational**: The code is written to be read, with over 50% of the file dedicated to comments explaining the what, how, and why of each step.
- [**Self-compiling**](<https://en.wikipedia.org/wiki/Self-hosting_(compilers)>): TLC can compile itself and run the result inside its own VM (Virtual Machine).
- **Zero dependencies**: Written in standard Lua 5.1 with no external libraries. Just one file and the Lua interpreter are all you need.
- **Complete Pipeline:** Includes a tokenizer, parser, code generator, compiler, and a virtual machine, all in one file.
- **Speed**: While education is the priority, the tokenizer uses optimized lookups and the compiler is designed efficiently, making it quite fast for a compiler written in a high-level language.
- **100% test coverage**: TLC has a test suite that covers 100% of the code. Want to see it in action? Run `lua tests/test.lua` in your terminal.

### [Want to jump into the code? Click here](https://github.com/bytexenon/The-Tiny-Lua-Compiler/blob/main/the-tiny-lua-compiler.lua)

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

local tlc = require("the-tiny-lua-compiler")

-- 1. Source code to compile
local source = [[
  for i = 1, 3 do
    print("Hello from TLC! " .. i)
  end
]]

-- 2. Compilation Pipeline
-- -----------------------

-- Tokenize: Raw text -> Stream of tokens
local tokens = tlc.Tokenizer.new(source):tokenize()

-- Parse: Stream of tokens -> Abstract Syntax Tree (AST)
local ast = tlc.Parser.new(tokens):parse()

-- Generate Code: AST -> Function Prototype (Intermediate Representation)
local proto = tlc.CodeGenerator.new(ast):generate()

-- 3. Execution
-- Run the prototype in the Virtual Machine
tlc.VirtualMachine.new(proto):execute()

--[[
  EXPECTED OUTPUT:
  Hello from TLC! 1
  Hello from TLC! 2
  Hello from TLC! 3
--]]
```

### Okay so where do I begin?

Awesome! Head on over to the [the-tiny-lua-compiler.lua](https://github.com/bytexenon/The-Tiny-Lua-Compiler/blob/main/the-tiny-lua-compiler.lua) file.

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
*   `{ TYPE = "Variable", Name = <string>, VariableType = <"Local" | "Global" | "Upvalue"> }`
*   `{ TYPE = "StringLiteral", Value = <string> }`
*   `{ TYPE = "NumericLiteral", Value = <number> }`
*   `{ TYPE = "BooleanLiteral", Value = <bool> }`
*   `{ TYPE = "NilLiteral" }`
*   `{ TYPE = "VarargExpression" }`

#### **Expressions**
*   `{ TYPE = "FunctionExpression", Body = <Block>, Parameters = <list_of_strings>, IsVarArg = <bool> }`
*   `{ TYPE = "UnaryOperator", Operator = <string>, Operand = <node> }`
*   `{ TYPE = "BinaryOperator", Operator = <string>, Left = <node>, Right = <node> }`
*   `{ TYPE = "FunctionCall", Callee = <node>, Arguments = <list_of_nodes>, IsMethodCall = <bool> }`
*   `{ TYPE = "IndexExpression", Base = <node>, Index = <node>, IsPrecomputed = <bool>? }`
*   `{ TYPE = "TableConstructor", Elements = <list_of_TableElement> }`
*   `{ TYPE = "TableElement", Key = <node>, Value = <node>, IsImplicitKey = <bool> }`
*   `{ TYPE = "ParenthesizedExpression", Expression = <node> }`

#### **Statements**
*   `{ TYPE = "LocalDeclarationStatement", Variables = <list_of_strings>, Initializers = <list_of_nodes> }`
*   `{ TYPE = "LocalFunctionDeclaration", Name = <string>, Body = <FunctionExpression> }`
*   `{ TYPE = "AssignmentStatement", LValues = <list_of_nodes>, Expressions = <list_of_nodes> }`
*   `{ TYPE = "CallStatement", Expression = <FunctionCall> }`
*   `{ TYPE = "IfClause", Condition = <node>, Body = <Block> }`
*   `{ TYPE = "IfStatement", Clauses = <list_of_IfClauses>, ElseClause = <Block>? }`
*   `{ TYPE = "WhileStatement", Condition = <node>, Body = <Block> }`
*   `{ TYPE = "RepeatStatement", Body = <Block>, Condition = <node> }`
*   `{ TYPE = "ForNumericStatement", Variable = <Identifier>, Start = <node>, End = <node>, Step = <node>?, Body = <Block> }`
*   `{ TYPE = "ForGenericStatement", Iterators = <list_of_strings>, Expressions = <list_of_nodes>, Body = <Block> }`
*   `{ TYPE = "DoStatement", Body = <Block> }`
*   `{ TYPE = "ReturnStatement", Expressions = <list_of_nodes> }`
*   `{ TYPE = "BreakStatement" }`

#### **Program Structure**
*   `{ TYPE = "Block", Statements = <list_of_statement> }`
*   `{ TYPE = "Program", Body = <Block> }`

</details>

### Support The Tiny Lua Compiler (TLC)

I don't take donations, but you can support TLC by starring the repository and sharing it with others.
If you find a bug or have a feature request, feel free to open an issue or submit a pull request.

---

[![cc-by-4.0](https://licensebuttons.net/l/by/4.0/80x15.png)](http://creativecommons.org/licenses/by/4.0/)

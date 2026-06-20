<div align="center">

![Tiny Lua Compiler (TLC)](https://github.com/bytexenon/Tiny-Lua-Compiler/assets/125568681/41cf5285-e31d-4b27-a8a8-ee83a7300f1f)

**An educational Lua 5.1 compiler and virtual machine in one Lua file**

_Inspired by [Jamie Kyle's The Super Tiny Compiler](https://github.com/jamiebuilds/the-super-tiny-compiler)_

![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)
![Lua](https://img.shields.io/badge/Lua-5.1--5.5-blue)

</div>

**Tiny Lua Compiler** (TLC) is a complete Lua 5.1 compiler written in pure Lua.
It tokenizes source code, builds an AST (Abstract Syntax Tree), lowers it into
Lua 5.1 function prototypes, emits real PUC Lua 5.1 bytecode, and executes
those prototypes in its own register-based VM. The whole core lives in
[tlc.lua](tlc.lua), and the file itself is only 4,700 lines long, or about
3,000 with the educational comments removed.

Most compiler learning material falls into one of two buckets: on one side are
toy compilers that are easy to finish but skip the parts that make real
languages interesting. On the other are production compilers that are real, but
so large that the main ideas get buried under architecture and history. TLC is
meant to sit in the middle. It is small enough to read in a weekend, but real
enough to deal with lexical scoping, closures, upvalues, varargs, multiple
returns, method calls, loops, tail calls, bytecode encoding, and execution.

It is not a production compiler, and it is not trying to replace the standard
Lua implementation. It is an educational compiler that tries to stay honest:
small enough to understand and complete enough to be interesting.

Tiny Lua Compiler is not a port of the standard Lua implementation. It was
built completely from scratch. That keeps it extremely small and straightforward,
but it also means some edge cases may be handled differently from standard Lua.
The test suite covers a wide range of Lua features, but it is not exhaustive.
If you find a case where TLC behaves differently from standard Lua, please report
it as a bug.

## It can compile itself

TLC can compile its own source code and run the result inside its own virtual machine:

```lua
local tlc  = require("tlc")
local tlc2 = tlc.run(io.open("tlc.lua"):read("*a"))
tlc2.run("print('Hello from a compiler running inside itself')")
```

That means a compiler written in Lua compiles a compiler written in Lua, and
the compiled compiler then runs new Lua code, all without leaving the host
process.

## Try it

```bash
git clone https://github.com/bytexenon/Tiny-Lua-Compiler.git
cd Tiny-Lua-Compiler

# Run the code inside TLC's own VM.
lua5.1 -e "require('tlc').run(\"print('Hello from TLC!')\")"

# Compile to a binary .luac chunk and run it with the standard Lua VM.
lua5.1 -e "io.open('out.luac','wb'):write(require('tlc').compile('print(42)'))"
lua5.1 out.luac

lua5.1 tests/test.lua
```

You can also use TLC as a library, at whatever level of detail you need:

```lua
local tlc = require("tlc")

-- One-liner: compile and run.
tlc.run("print('Hello from TLC!')")

-- Compile to a binary .luac chunk that the standard Lua VM can load.
local bytecode = tlc.compile("return 21 * 2")
-- io.open("out.luac", "wb"):write(bytecode) -- Save to disk if you want.

-- Walk the pipeline stage by stage.
local tokens = tlc.tokenize("local x = 1 + 2; return x")
local ast    = tlc.parseTokens(tokens)
local proto  = tlc.generate(ast)
local value  = tlc.execute(proto)

print(value) -- 3
```

## Why this file is worth reading

The code runs in a straight line. Utilities first, then the tokenizer, the
parser, the code generator, the bytecode emitter, the VM, and the public API,
in that order, with nothing out of place. You can trace a single source program
through every stage without losing the thread.

The implementation also keeps the details that toy compilers skip. Character
classification uses precomputed lookup tables. Operator matching uses a trie for
longest-prefix matching, with no hardcoded lookahead if-statements. Expression
parsing uses precedence climbing rather than a grammar rule per level.
Concatenation chains are flattened into a single `CONCAT`. Floating-point
numbers are packed to IEEE 754 by hand, without `string.pack`. Upvalue capture
and `OP_CLOSE` are handled explicitly.

These are not polish. They are where real compiler behavior starts to show up.
Skip them and you learn the shape of compilation. Keep them and you learn how
it actually works.

## What TLC covers, and what it does not

TLC covers a large enough slice of Lua 5.1 to feel real:

- Lexical scoping, closures, upvalue capture and closing
- Numeric and generic `for`, `while`, `repeat`, `do`, `break`, `return`, `if` / `elseif` / `else`, etc.
- Method calls (`:` syntax), table constructors
- Multiple returns, varargs (`...`), tail call optimization
- Long strings, string escapes, hex numbers, scientific notation
- Correct PUC Lua 5.1 bytecode emission - bytecode loads in the standard VM

What it deliberately leaves out is just as important. TLC does not
implement any optimizations, also it does not emit debug information, meaning the
table that maps each instruction to a source line; without it, error messages
show no line numbers, but the bytecode is otherwise correct.

The biggest omissions are in the VM, one of them being metamethod dispatch.
Write `a + b` when `a` is a table, and standard Lua will check for the `__add`
metamethod. TLC's VM skips that entirely and lets the native Lua runtime
handle it instead. That removes a real feature, but it keeps the VM from
becoming an object system. Additionally, TLC's VM doesn't have a garbage
collector (GC); it relies on the host's GC to collect garbage instead.

These tradeoffs are deliberate. TLC is trying to be a real compiler you can actually
finish reading.

## Correctness

The test suite compiles each case with both TLC and standard Lua, then compares
the results side by side. No hand-written expectations: if TLC produces
different output, the test fails.

This catches the mistakes educational compilers usually get away with: wrong
operator precedence, broken closure semantics, multi-return adjustment errors,
loop control flow bugs, and literal parsing mistakes, among others.

## API

```lua
local tlc = require("tlc")

tlc.run(code, env?, ...?)
tlc.compile(code)
tlc.compileToProto(code)
tlc.parse(code)
tlc.tokenize(code)

tlc.parseTokens(tokens)
tlc.generate(ast)
tlc.emit(proto)
tlc.execute(proto, env?, ...?)
```

[docs/api.md](docs/api.md) documents the public API and
[docs/ast.md](docs/ast.md) documents the AST shape.

## Where to start

Read this file for the big picture, then read [tlc.lua](tlc.lua) from top to
bottom. After that, [docs/api.md](docs/api.md) and [docs/ast.md](docs/ast.md)
fill in the reference material, and [tests](tests) show the behavior TLC covers.

TLC runs on Lua 5.1 through 5.5, although the generated bytecode targets Lua
5.1.

Contributions are welcome; see [CONTRIBUTING.md](CONTRIBUTING.md). If you
report a bug, please include the input code, expected behavior, actual
behavior, and Lua version.

## See also

- [The Super Tiny Compiler](https://github.com/jamiebuilds/the-super-tiny-compiler) - the original inspiration; a compiler written in JavaScript in ~200 lines
- [FiOne](https://github.com/Rerumu/FiOne) - a Lua-in-Lua VM, more complete than TLC's but less focused on readability
- [Ravi's Lua 5.3 bytecode reference](https://the-ravi-programming-language.readthedocs.io/en/latest/lua_bytecode_reference.html#) - was extremely useful for understanding Lua internals. Some opcode descriptions are taken from there.
- [Lua 5.1 source](https://www.lua.org/source/5.1/) - the reference implementation; `llex.c`, `lparser.c`, and `lvm.c` are the most relevant files
- A No-Frills Introduction to Lua 5.1 VM Instructions - great reference for the Lua 5.1 internals.

## License

MIT. See [LICENSE](LICENSE).

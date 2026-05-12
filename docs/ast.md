# TLC AST Node Specification

The parser produces an Abstract Syntax Tree (AST) made of nested Lua tables. Each node has a `kind` field that identifies its type. This document specifies every node type TLC can produce.

## Program Structure

### Program

The root node of every AST.

```lua
{ kind = "Program", body = <Block> }
```

### Block

A sequence of statements. Every scope boundary (function body, if-clause, loop body) produces a Block.

```lua
{ kind = "Block", statements = <list_of_statements> }
```

## Literals and Identifiers

### NumericLiteral

A number constant. `value` is the numeric value, `raw` is the original source text.

```lua
{ kind = "NumericLiteral", value = <number>, raw = <string> }
-- Example: 42, 3.14, 0xFF, 1e10
```

### StringLiteral

A string constant (single-quoted, double-quoted, or long string).

```lua
{ kind = "StringLiteral", value = <string> }
-- Example: "hello", 'world', [[long string]]
```

### BooleanLiteral

```lua
{ kind = "BooleanLiteral", value = <boolean> }
-- value is true or false
```

### NilLiteral

```lua
{ kind = "NilLiteral" }
```

### Identifier

A variable or function name reference.

```lua
{ kind = "Identifier", value = <string> }
-- Example: x, myVar, _G
```

### VarargExpression

The `...` expression inside a vararg function.

```lua
{ kind = "VarargExpression" }
```

## Expressions

### FunctionExpression

An anonymous function definition. Also used as the `body` field in `LocalFunctionDeclaration` and as the value in desugared function declaration assignments.

```lua
{
  kind       = "FunctionExpression",
  body       = <Block>,
  parameters = <list_of_strings>,
  isVararg   = <boolean>
}
```

### UnaryOperator

A unary operation: `-x`, `not x`, or `#x`.

```lua
{
  kind     = "UnaryOperator",
  operator = <string>,   -- "-", "not", "#"
  operand  = <node>
}
```

### BinaryOperator

A binary operation.

```lua
{
  kind     = "BinaryOperator",
  operator = <string>,   -- "+", "-", "*", "/", "%", "^", "..",
                         -- "==", "~=", "<", ">", "<=", ">=",
                         -- "and", "or"
  left     = <node>,
  right    = <node>
}
```

### FunctionCall

A function or method call.

```lua
{
  kind         = "FunctionCall",
  callee       = <node>,
  arguments    = <list_of_nodes>,
  isMethodCall = <boolean>?   -- nil by default, true for obj:method() syntax 
}
```

When `isMethodCall` is true, `callee` is an `IndexExpression` where the method name is the index, and the object is implicitly passed as the first argument by the VM.

### IndexExpression

Table field access via `table.key` or `table[expr]`.

```lua
{
  kind          = "IndexExpression",
  base          = <node>,            -- the table
  index         = <node>,            -- the key
}
```

For dot syntax (`t.key`), `index` is a `StringLiteral` node. For bracket syntax (`t[expr]`), `index` is an arbitrary expression.

### TableConstructor

A table literal `{ ... }`.

```lua
{
  kind     = "TableConstructor",
  elements = <list_of_TableElement>
}
```

### TableElement

A single entry in a table constructor.

```lua
{
  kind       = "TableElement",
  key        = <node>,
  value      = <node>,
  isImplicit = <boolean>   -- true for positional entries like {1, 2, 3}
}
```

For implicit keys (`{expr1, expr2}`), `key` is a `NumericLiteral` with sequential indices starting at 1.

### ParenthesizedExpression

An expression wrapped in parentheses. This is **not** purely cosmetic -- in Lua, parentheses force a multi-return expression to adjust to a single value. `(f())` returns one value, `f()` may return many.

```lua
{
  kind       = "ParenthesizedExpression",
  expression = <node>
}
```

## Statements

### LocalDeclarationStatement

`local var1, var2 = expr1, expr2`

```lua
{
  kind         = "LocalDeclarationStatement",
  variables    = <list_of_strings>,
  initializers = <list_of_nodes>     -- may be empty
}
```

### LocalFunctionDeclaration

`local function name(...) ... end`

```lua
{
  kind = "LocalFunctionDeclaration",
  name = <string>,
  body = <FunctionExpression>
}
```

### AssignmentStatement

`lvalue1, lvalue2 = expr1, expr2`

Each lvalue is either an `Identifier` or `IndexExpression` node.

```lua
{
  kind        = "AssignmentStatement",
  lvalues     = <list_of_nodes>,
  expressions = <list_of_nodes>
}
```

### CallStatement

A function call used as a statement (the return value is discarded).

```lua
{
  kind       = "CallStatement",
  expression = <FunctionCall>
}
```

### IfStatement

```lua
{
  kind       = "IfStatement",
  clauses    = <list_of_IfClauses>,
  elseClause = <Block> or nil
}
```

### IfClause

A single `if` or `elseif` branch.

```lua
{
  kind      = "IfClause",
  condition = <node>,
  body      = <Block>
}
```

### WhileStatement

`while condition do ... end`

```lua
{
  kind      = "WhileStatement",
  condition = <node>,
  body      = <Block>
}
```

### RepeatStatement

`repeat ... until condition`

```lua
{
  kind      = "RepeatStatement",
  body      = <Block>,
  condition = <node>
}
```

Note: In Lua, locals declared inside the repeat block are visible in the `until` condition. The parser handles this scoping correctly.

### ForNumericStatement

`for var = start, limit[, step] do ... end`

```lua
{
  kind     = "ForNumericStatement",
  variable = <string>,
  start    = <node>,
  limit    = <node>,
  step     = <node> or nil,   -- nil means implicit step of 1
  body     = <Block>
}
```

### ForGenericStatement

`for iter1, iter2 in expr1, expr2 do ... end`

```lua
{
  kind       = "ForGenericStatement",
  iterators  = <list_of_strings>,
  expressions = <list_of_nodes>,
  body       = <Block>
}
```

### DoStatement

`do ... end`

```lua
{
  kind = "DoStatement",
  body = <Block>
}
```

### ReturnStatement

`return expr1, expr2, ...`

```lua
{
  kind        = "ReturnStatement",
  expressions = <list_of_nodes>   -- may be empty
}
```

### BreakStatement

```lua
{ kind = "BreakStatement" }
```

## Example

Given the source code:

```lua
local x = 1 + 2
print(x)
```

TLC produces:

```lua
{
  kind = "Program",
  body = {
    kind = "Block",
    statements = {
      {
        kind = "LocalDeclarationStatement",
        variables = {"x"},
        initializers = {
          {
            kind = "BinaryOperator",
            operator = "+",
            left  = {kind = "NumericLiteral", value = 1, raw = "1"},
            right = {kind = "NumericLiteral", value = 2, raw = "2"}
          }
        }
      },
      {
        kind = "CallStatement",
        expression = {
          kind = "FunctionCall",
          callee = {kind = "Identifier", value = "print"},
          arguments = {
            {kind = "Identifier", value = "x"}
          }
        }
      }
    }
  }
}
```

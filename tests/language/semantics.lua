local tlc = require("tlc")

return function(suite)
  suite:describe("Language semantics", function()
    suite:describe("Expressions", function()
      suite:it("handles all numeric formats", function()
        suite:assertMatchesLua([[
          return 123 + 0.5 + 1e2 + .25e1 + 0.2e-1 + 0x5p5
        ]])
      end)

      suite:it("handles string delimiters and long strings", function()
        suite:assertMatchesLua([===[
          return "double"
            .. 'single'
            .. [[multi-line]]
            .. [=[nested]=]
            .. [==[deeply nested]==]
        ]===])
      end)

      suite:it("implements Lua 5.1 short-string escape semantics", function()
        local result = suite:compileAndRun([[return "\a\b\f\n\r\t\v\z\\\"\'" ]])
        local expected = "\7\8\12\10\13\9\11\122\92\34\39"

        suite:assertEqual(expected, result)
      end)

      suite:it("respects arithmetic precedence", function()
        suite:assertMatchesLua([[return 2 + 3 * 4 ^ 2 / 2 - 1]])
      end)

      suite:it("treats exponentiation as right-associative", function()
        suite:assertMatchesLua([[return 2 ^ 3 ^ 2]])
      end)

      suite:it("respects concatenation right-associativity", function()
        suite:assertMatchesLua([[return "a" .. 1 .. 2, ("a" .. 1) .. 2]])
      end)

      suite:it("handles mixed relational and logical precedence", function()
        suite:assertMatchesLua([[
          return "a" .. "b" == "ab" and not (2 > 3 or 5 < 4)
        ]])
      end)

      suite:it("handles unary operators", function()
        suite:assertMatchesLua([[return -10 + -(-5), not (not true), #"abc"]])
      end)

      suite:it("short-circuits 'and' and 'or' correctly", function()
        suite:assertMatchesLua([[
          local x = 5
          local y = (x > 10 and error("fail")) or 42
          return y, 1 or error("fail")
        ]])
      end)

      suite:it(
        "returns the original operand values for 'and' and 'or'",
        function()
          suite:assertMatchesLua(
            [[return 0 and 42, false or "fallback", nil or 9]]
          )
        end
      )

      suite:it(
        "adjusts multi-return expressions when wrapped in parentheses",
        function()
          suite:assertMatchesLua([[
          local function f()
            return 1, 2, 3
          end

          local a, b, c = (f())
          return a, b, c
        ]])
        end
      )
    end)

    suite:describe("Assignments and blocks", function()
      suite:it("handles local and global assignments", function()
        suite:assertMatchesLua([[
          local a = 1
          b = a + 2
          return b
        ]])
      end)

      suite:it("handles swaps and chained assignments", function()
        suite:assertMatchesLua([[
          local a, b, c = 1, 2, 3
          a, b = b, a
          c = a + b
          return a, b, c
        ]])
      end)

      suite:it("pads missing assignment values with nil", function()
        suite:assertMatchesLua([[
          local a, b, c = 1, 2
          return a, b, c
        ]])
      end)

      suite:it("discards extra assignment values", function()
        suite:assertMatchesLua([[
          local a = 1, 2, 3
          return a
        ]])
      end)

      suite:it(
        "evaluates assignment targets before performing writes",
        function()
          suite:assertMatchesLua([[
          local t = {10, 20}
          local i = 1
          i, t[i] = 2, 99
          return i, t[1], t[2]
        ]])
        end
      )

      suite:it(
        "expands multi-return calls in the final assignment slot",
        function()
          suite:assertMatchesLua([[
          local function f()
            return 1, 2, 3
          end

          local a, b, c, d = 0, f()
          return a, b, c, d
        ]])
        end
      )

      suite:it("handles if, elseif, and else blocks", function()
        suite:assertMatchesLua([[
          local x = 10
          if x > 20 then
            return 1
          elseif x > 5 then
            return 2
          else
            return 3
          end
        ]])
      end)

      suite:it("uses do-end blocks for lexical scoping", function()
        suite:assertMatchesLua([[
          local a = 1
          do
            local a = 2
          end
          return a
        ]])
      end)
    end)

    suite:describe("Loops", function()
      suite:it("handles while loops", function()
        suite:assertMatchesLua([[
          local i = 1
          local sum = 0
          while i <= 5 do
            sum = sum + i
            i = i + 1
          end
          return sum
        ]])
      end)

      suite:it("handles repeat-until loops", function()
        suite:assertMatchesLua([[
          local i = 1
          local sum = 0
          repeat
            sum = sum + i
            i = i + 1
          until i > 5
          return sum
        ]])
      end)

      suite:it("handles basic numeric for loops", function()
        suite:assertMatchesLua([[
          local sum = 0
          for i = 1, 5 do
            sum = sum + i
          end
          return sum
        ]])
      end)

      suite:it("handles numeric for loops with negative steps", function()
        suite:assertMatchesLua([[
          local sum = 0
          for i = 10, 1, -2 do
            sum = sum + i
          end
          return sum
        ]])
      end)

      suite:it("handles numeric for loops with floating-point steps", function()
        suite:assertMatchesLua([[
          local sum = 0
          for i = 0.5, 2.5, 0.5 do
            sum = sum + i
          end
          return sum
        ]])
      end)

      suite:it("does not execute zero-iteration numeric loops", function()
        suite:assertMatchesLua([[
          local ran = false
          for i = 3, 1 do
            ran = true
          end
          return ran
        ]])
      end)

      suite:itIf(
        _VERSION ~= "Lua 5.5",
        "keeps the numeric for control variable separate from the body local",
        function()
          suite:assertMatchesLua([[
            local sum = 0
            for i = 1, 10 do
              sum = sum + i
              i = i + 5
            end
            return sum
          ]])
        end,
        "behavior differs on Lua 5.5"
      )

      suite:it("handles generic for loops with ipairs", function()
        suite:assertMatchesLua([[
          local sum = 0
          for _, value in ipairs({5, 4, 3}) do
            sum = sum + value
          end
          return sum
        ]])
      end)

      suite:it("handles generic for loops with pairs", function()
        suite:assertMatchesLua([[
          local t = {a = 1, b = 2}
          local sum = 0
          for _, value in pairs(t) do
            sum = sum + value
          end
          return sum
        ]])
      end)

      suite:it("handles generic for loops with custom iterators", function()
        suite:assertMatchesLua([[
          local sum = 0
          local function iterator()
            local n = 0
            return function()
              n = n + 1
              return n <= 3 and n * 3 or nil
            end
          end

          for value in iterator() do
            sum = sum + value
          end
          return sum
        ]])
      end)

      suite:it("breaks out of numeric for loops", function()
        suite:assertMatchesLua([[
          local sum = 0
          for i = 1, 10 do
            sum = sum + i
            if i == 5 then
              break
            end
          end
          return sum
        ]])
      end)

      suite:it("breaks out of generic for loops", function()
        suite:assertMatchesLua([[
          local sum = 0
          for _, value in ipairs({1, 2, 3, 4, 5}) do
            sum = sum + value
            if value == 3 then
              break
            end
          end
          return sum
        ]])
      end)

      suite:it("breaks out of while loops", function()
        suite:assertMatchesLua([[
          local sum = 0
          while true do
            sum = sum + 1
            if sum == 5 then
              break
            end
          end
          return sum
        ]])
      end)

      suite:it("breaks out of repeat loops", function()
        suite:assertMatchesLua([[
          local sum = 0
          repeat
            sum = sum + 1
            if sum == 5 then
              break
            end
          until false
          return sum
        ]])
      end)

      suite:it("breaks out of nested loops", function()
        suite:assertMatchesLua([[
          local a, b = 0, 0
          for i = 1, 10 do
            for j = 1, 10 do
              a = a + 1
              b = b + 1
              if a % 10 == 4 then
                break
              end
            end
            a = a * b
            if b % 15 == 7 then
              break
            end
          end
          return a * b
        ]])
      end)

      suite:it("returns from inside loops", function()
        suite:assertMatchesLua([[
          for i = 1, 10 do
            if i == 5 then
              return i * 2
            end
          end
        ]])
      end)
    end)

    suite:describe("Closures and upvalues", function()
      suite:it("respects lexical scoping across nested blocks", function()
        suite:assertMatchesLua([[
          local x = 10
          do
            local x = 20
            x = x + 5
          end
          return x
        ]])
      end)

      suite:it("allows repeated local names in different scopes", function()
        suite:assertMatchesLua([[
          local x = 1
          do
            local x = 2
          end
          return x
        ]])
      end)

      suite:it("captures variables from outer functions", function()
        suite:assertMatchesLua([[
          local function outer()
            local x = 5
            return function()
              return x
            end
          end
          return outer()()
        ]])
      end)

      suite:it("allows captured upvalues to be mutated", function()
        suite:assertMatchesLua([[
          local function outer()
            local x = 5
            return function()
              x = x + 1
              return x
            end
          end

          local f = outer()
          f()
          return f()
        ]])
      end)

      suite:it("handles multi-level closures", function()
        suite:assertMatchesLua([[
          local function l1()
            local a = 1
            return function()
              local b = 2
              return function()
                return a + b
              end
            end
          end
          return l1()()()
        ]])
      end)

      suite:it("closes locals when leaving a do-end scope", function()
        suite:assertMatchesLua([[
          local f
          do
            local x = 42
            f = function()
              return x
            end
          end
          return f()
        ]])
      end)

      suite:it("closes locals when breaking out of a loop", function()
        suite:assertMatchesLua([[
          local f
          while true do
            local x = 42
            f = function()
              return x
            end
            break
          end
          return f()
        ]])
      end)

      suite:it("closes repeat-scope locals after the loop body", function()
        suite:assertMatchesLua([[
          local f
          local done = false
          repeat
            local x = 99
            f = function()
              return x
            end
            done = true
          until done
          return f()
        ]])
      end)

      suite:it("captures per-iteration locals independently", function()
        suite:assertMatchesLua([[
          local out = {}
          for i = 1, 3 do
            local x = i
            out[i] = function()
              return x
            end
          end
          return out[1](), out[2](), out[3]()
        ]])
      end)

      suite:it("supports recursive local function declarations", function()
        suite:assertMatchesLua([[
          local function fact(n)
            if n <= 1 then
              return 1
            end
            return n * fact(n - 1)
          end

          return fact(5)
        ]])
      end)
    end)

    suite:describe("Functions and calls", function()
      suite:it("handles anonymous function expressions", function()
        suite:assertMatchesLua([[
          local f = function()
            return 42
          end
          return f()
        ]])
      end)

      suite:it("handles named function declaration sugar", function()
        suite:assertMatchesLua([[
          function f()
            return 1
          end
          return f()
        ]])
      end)

      suite:it("handles table method definitions", function()
        suite:assertMatchesLua([[
          local t = {x = 10}
          function t:add(y)
            return self.x + y
          end
          return t:add(5)
        ]])
      end)

      suite:it("handles parenthesis-less string calls", function()
        suite:assertMatchesLua([[
          local captured = ""
          local function f(value)
            captured = value
          end
          f"hello"
          return captured
        ]])
      end)

      suite:it("handles parenthesis-less table calls", function()
        suite:assertMatchesLua([[
          local captured
          local function f(value)
            captured = value
          end
          f{1, 2}
          return captured[2]
        ]])
      end)

      suite:it("handles colon-style method calls", function()
        suite:assertMatchesLua([[
          local t = {
            x = 10,
            f = function(self, y)
              return self.x + y
            end,
          }
          return t:f(5)
        ]])
      end)

      suite:it("handles varargs", function()
        suite:assertMatchesLua([[
          local function f(...)
            local t = {...}
            return t[2], select("#", ...)
          end
          return f(1, 2, 3)
        ]])
      end)

      suite:it("handles chained calls and indexing", function()
        suite:assertMatchesLua([[
          local t = {
            f = function()
              return {
                g = function()
                  return 7
                end,
              }
            end,
          }

          return t.f().g()
        ]])
      end)
    end)

    suite:describe("Tables", function()
      suite:it("handles empty constructors", function()
        suite:assertMatchesLua([[return {}]])
      end)

      suite:it("handles comma and semicolon separators", function()
        suite:assertMatchesLua([[return {1, 2, 3}, {1; 2; 3}]])
      end)

      suite:it("handles trailing separators", function()
        suite:assertMatchesLua([[
          return {1, 2, 3,}, {a = 1, b = 2,}, {1; 2; 3;}, {a = 1; b = 2;}
        ]])
      end)

      suite:it("handles array-style constructors", function()
        suite:assertMatchesLua([=[return ({1, 2, 3})[2]]=])
      end)

      suite:it("handles hash-style and mixed constructors", function()
        suite:assertMatchesLua([[
          return ({a = 1, ["b"] = 2, [3] = 3, 4})["b"]
        ]])
      end)

      suite:it("expands the last multi-return table element", function()
        suite:assertMatchesLua([[
          local function f()
            return 3, 4, 5
          end
          local t = {1, 2, f()}
          return t[1], t[2], t[3], t[4], t[5]
        ]])
      end)

      suite:it(
        "adjusts non-final multi-return table elements to one value",
        function()
          suite:assertMatchesLua([[
          local function f()
            return 7, 8, 9
          end
          local t = {f(), 1}
          return t[1], t[2], t[3]
        ]])
        end
      )
    end)

    suite:describe("Compile-time errors", function()
      suite:it("errors on break outside a loop", function()
        suite:assertError(function()
          tlc.compileToProto("break")
        end, "no loop to break")
      end)

      suite:it("errors on break inside a plain do-end block", function()
        suite:assertError(function()
          tlc.compileToProto([[
          do
            break
          end
        ]])
        end, "no loop to break")
      end)

      suite:it("errors on numeric for loops with missing limits", function()
        suite:assertError(function()
          tlc.compileToProto("for i = 1 do return i end")
        end, "requires at least a start and limit expression")
      end)
    end)

    suite:describe("Integration", function()
      suite:it("computes fibonacci correctly", function()
        suite:assertMatchesLua([[
          local function fib(n)
            if n <= 1 then
              return n
            end
            return fib(n - 1) + fib(n - 2)
          end
          return fib(10)
        ]])
      end)

      suite:it("self-compiles and executes the result", function()
        local testCode = [==[
          local tlc = suite:compileAndRun(io.open("tlc.lua"):read("*a"))

          local code = [[
            local val1 = 2 * 10 + (function() return 2 * 9e2 end)()
            local sum = 0
            for i = 1, val1 do
              sum = sum + i
              if i % 10 == 0 then
                sum = sum - 5.1
              elseif i % 5 == 0 then
                sum = sum + 0xFF
              end
              if i == 15 then
                break
              end
            end
            return sum
          ]]

          return tlc.run(code)
        ]==]

        _G.suite = suite
        suite:assertEqual(624.9, suite:compileAndRun(testCode))
        _G.suite = nil
      end)
    end)
  end)
end

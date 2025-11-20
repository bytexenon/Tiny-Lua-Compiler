--* Dependencies *--
local tlc = require("the-tiny-lua-compiler")

--* Constants *--

-- Safety Limit for Infinite Loop Detection.
-- We use Lua's debug hook (`debug.sethook`) to count executed instructions
-- during each test run. If a test exceeds this limit, we assume it's stuck
-- in an infinite loop and terminate it with an error.
--
-- 15 million instructions is a high threshold, unlikely to be hit by correct,
-- non-trivial code within a reasonable time (~4 seconds processing time),
-- but low enough to catch runaway loops.
local INFINITE_LOOP_LIMIT = 15000000

-- A lookup table to convert non-printable escape characters back into their
-- readable '\\' + character form for clear error messages.
local ESCAPED_CHARACTER_CONVERSIONS = {
  ["\a"]  = "a",  -- Bell (alert sound)
  ["\b"]  = "b",  -- Backspace
  ["\f"]  = "f",  -- Form feed (printer page eject)
  ["\n"]  = "n",  -- New line
  ["\r"]  = "r",  -- Carriage return
  ["\t"]  = "t",  -- Horizontal tab
  ["\v"]  = "v",  -- Vertical tab
}

--* Local Functions *--

local function sanitizeString(str)
  return (str:gsub("[\a\b\f\n\r\t\v]", function(escapeChar)
    return "\\".. ESCAPED_CHARACTER_CONVERSIONS[escapeChar]
  end))
end

local function shallowCopy(original)
  if type(original) ~= "table" then
    return original
  end

  local copy = {}
  for i, v in pairs(original) do
    copy[i] = v
  end

  return copy
end

local function deepcompare(table1, table2, seen)
  seen = seen or {}
  if table1 == table2 then return true end
  if type(table1) ~= "table" or type(table2) ~= "table" then return false end
  if seen[table1] or seen[table2] then
    return seen[table1] == table2 or seen[table2] == table1
  end

  seen[table1] = table2
  seen[table2] = table1

  for i, v1 in pairs(table1) do
    local v2 = table2[i]
    if v2 == nil or not deepcompare(v1, v2, seen) then
      return false
    end
  end
  for i, _ in pairs(table2) do
    if table1[i] == nil then
      return false
    end
  end

  return true
end

local function tableTostring(tbl)
  local parts = {}
  for i, v in pairs(tbl) do
    table.insert(parts, tostring(i).." = "..tostring(v))
  end

  return "{" .. table.concat(parts, ", ") .. "}"
end

--* TLCTest *--
local TLCTest = {}
TLCTest.__index = TLCTest

function TLCTest.new()
  local self = setmetatable({}, TLCTest)
  self.ranTests = {}
  self.groups   = {}

  return self
end

function TLCTest:_getTestPath(name)
  local path = table.concat(self.groups, "->")
  path = (path == "" and path) or path .. "->"
  path = "\27[90m" .. path .. "\27[0m"

  return path .. name
end

function TLCTest:describe(name, func)
  table.insert(self.groups, name)
  func()
  table.remove(self.groups)
end

function TLCTest:it(name, func)
  local errorTable = nil
  local failed     = false

  if debug and debug.sethook then
    local function unhook() debug.sethook() end
    local function terminateInfiniteLoop()
      unhook()
      return error("TLCTest: Infinite loop detected after " .. INFINITE_LOOP_LIMIT .. " instructions")
    end

    debug.sethook(terminateInfiniteLoop, "", INFINITE_LOOP_LIMIT)
    xpcall(func, function(err)
      local message = err
      local traceback = debug.traceback("", 2):sub(2)

      failed = true
      errorTable = {
        message   = message,
        traceback = traceback
      }
    end)
    unhook()
  end

  -- Print test result --
  local path = self:_getTestPath(name)

  if failed then
    -- Something went wrong, test failed.
    io.write("\27[41m\27[30m FAIL \27[0m ")
    table.insert(self.ranTests, { status = "FAIL",
      name  = name,
      path  = path,
      error = errorTable
    })
  elseif not failed then
    -- No error occurred, test passed.
    io.write("\27[42m\27[30m PASS \27[0m ")
    table.insert(self.ranTests, { status = "PASS",
      name = name,
      path = path
    })
  end

  print(path)
end

function TLCTest:assertEqual(b, a, message)
  if a ~= b then
    local msg = ("Expected %s, got %s"):format(tostring(a), tostring(b))
    if message then msg = message .. " - " .. msg end
    return error(msg, 0)
  end
end

function TLCTest:assertDeepEqual(expected, actual, message)
  if deepcompare(expected, actual) then return true end

  local actualString = type(actual) == "table" and tableTostring(actual) or tostring(actual)
  local expectedString = type(expected) == "table" and tableTostring(expected) or tostring(expected)

  error("Expected returns do not match actual returns.\n" ..
        "    Expected: \t '" .. sanitizeString(expectedString) .. "'\n" ..
        "    Actual: \t '"   .. sanitizeString(actualString)   .. "'",
        0)

  if message then
    message = message .. " - " .. message
  end

  return error(message, 0)
end

function TLCTest:assertTrue(condition, message)
  if not condition then
    local msg = "Expected condition to be true, but got false"
    if message then msg = message .. " - " .. msg end
    return error(msg, 0)
  end
end

function TLCTest:assertFalse(condition, message)
  if condition then
    local msg = "Expected condition to be false, but got true"
    if message then msg = message .. " - " .. msg end
    return error(msg, 0)
  end
end

function TLCTest:runSandboxed(code)
  local func, err = loadstring(code)
  if not func then
    return false, tostring(err)
  end

  local newEnvironment = shallowCopy(_G)
  setfenv(func, newEnvironment)

  -- Execute the function in its new, sandboxed environment.
  return func()
end

function TLCTest:compileAndRun(code)
  local tokens    = tlc.Tokenizer.new(code):tokenize()
  local ast       = tlc.Parser.new(tokens):parse()
  local proto     = tlc.CodeGenerator.new(ast):generate()
  local bytecode  = tlc.BytecodeEmitter.new(proto):emit()

  return loadstring(bytecode)()
end

function TLCTest:assertCompileError(code)
  local success, err = xpcall(function()
    self:compileAndRun(code)
  end, function(e)
    return e .. debug.traceback("", 2)
  end)

  if success then
    error("Expected compilation to fail, but it succeeded", 0)
  end

  return err
end

function TLCTest:compileAndRunChecked(code)
  local expectedReturns = { xpcall(
    function()    return self:runSandboxed(code) end,
    function(err) return err .. debug.traceback("", 2) end)
  }

  local actualReturns = { xpcall(
    function()    return self:compileAndRun(code) end,
    function(err) return err .. debug.traceback("", 2) end)
  }

  local expectedResult, actualResult = table.remove(expectedReturns, 1),
                                       table.remove(actualReturns, 1)

  if not expectedResult or not actualResult then
    local errMsg = "Execution success status mismatch: "
    if not expectedResult then
      errMsg = errMsg .. "Standard Lua failed with: " .. tostring(expectedReturns[1])
    else
      errMsg = errMsg .. "TLC-compiled code failed with: " .. tostring(actualReturns[1])
    end
    error(errMsg, 0)
  end

  return self:assertDeepEqual(expectedReturns, actualReturns)
end

function TLCTest:summary()
  local pass, fail = 0, 0
  local errors = {}

  for _, test in ipairs(self.ranTests) do
    if test.status == "PASS" then
      pass = pass + 1
    elseif test.status == "FAIL" then
      fail = fail + 1
      table.insert(errors, test)
    end
  end

  print("\n\27[1mTest Results:\27[0m")
  print(("Passed: \27[32m%d\27[0m"):format(pass))
  print(("Failed: \27[31m%d\27[0m"):format(fail))
  print(("Total:  %d"):format(pass + fail))

  if fail > 0 then
    print("\n\27[1mErrors:\27[0m")
    for i, err in ipairs(errors) do
      print(("\n%d) \27[1m%s\27[0m"):format(i, err.path))
      print(("   \27[31m%s\27[0m"):format(err.error.message))
      print(("   \27[90m%s\27[0m"):format(err.error.traceback))
    end
  end

  os.exit((fail == 0 and 0) or 1)
end

--* Tests *--
local suite = TLCTest.new()

suite:describe("Lexical Conventions", function()
  suite:describe("Strings", function()
    suite:it("handles various string delimiters", function()
      suite:compileAndRunChecked([==[
        return "double" .. 'single' .. [[multi-line]] .. [=[nested]=]
      ]==])
    end)

    suite:it("handles empty strings of all kinds", function()
      suite:compileAndRunChecked([=[return "" .. '' .. [[]] .. [==[]==] ]=])
    end)

    suite:it("handles string escape sequences", function()
      suite:compileAndRunChecked([[return "\a\b\f\n\r\t\v\\\"\'"]])
    end)

    suite:it("handles numeric escape sequences", function()
      suite:compileAndRunChecked([[return "\9\99\101"]])
    end)
  end)

  suite:describe("Numbers", function()
    suite:it("handles various integer, hex, and float formats", function()
      suite:compileAndRunChecked([[return 123 + 0xA2 + 0X1F + 0.5 + 1e2 + 5]])
    end)

    suite:it("handles numbers starting with a decimal point", function()
      suite:compileAndRunChecked([[return .125]])
    end)

    suite:it("handles numbers with trailing decimal points", function()
      suite:compileAndRunChecked([[return 1. + 2]])
    end)
  end)

  suite:describe("Comments", function()
    suite:it("ignores single-line comments", function()
      suite:compileAndRunChecked([[ return 42 -- This is a comment ]])
    end)

    suite:it("ignores multi-line comments", function()
      suite:compileAndRunChecked([==[
        --[[ This is a multi-line comment. return "fail" ]]
        return 42
      ]==])
    end)

    suite:it("handles nested multi-line comments", function()
      suite:compileAndRunChecked([===[
        --[=[ This is a nested --[[ fake inner ]] multi-line comment ]=]
        return 42
      ]===])
    end)

    suite:it("handles comments at end of file without a newline", function()
      suite:compileAndRunChecked([[return 1 -- no newline]])
    end)
  end)

  suite:describe("Error Conditions", function()
    suite:it("errors on unterminated strings", function()
      suite:assertCompileError([[ local a = "hello ]])
    end)
    suite:it("errors on unterminated long comments", function()
      suite:assertCompileError([=[ --[[ this is not closed ]=])
    end)
  end)
end)

suite:describe("Expressions", function()
  suite:describe("Operators", function()
    suite:it("correctly handles arithmetic precedence", function()
      suite:compileAndRunChecked([[return 2 + 3 * 4 ^ 2 / 2 - 1]])
    end)

    suite:it("correctly handles mixed relational, logical, and concatenation precedence", function()
      suite:compileAndRunChecked([[return "a" .. "b" == "ab" and not (2 > 3 or 5 < 4)]])
    end)

    suite:it("handles right-associativity for power operator (^)", function()
      suite:compileAndRunChecked([[return 2 ^ 3 ^ 2]])
    end)

    suite:it("handles right-associativity for concatenation (..)", function()
      suite:compileAndRunChecked([[return "a" .. "b" .. "c"]])
    end)

    suite:it("respects parentheses to override precedence", function()
      suite:compileAndRunChecked([[return (2 + 3) * 4]])
    end)

    suite:it("handles unary operators (-, not, #)", function()
      suite:compileAndRunChecked([[return -10 + -(-5)]])
      suite:compileAndRunChecked([[return not (not true)]])
      suite:compileAndRunChecked([[local t = {1,2,3}; return #t .. #"abc"]])
    end)
  end)

  suite:describe("Relational and Logical Operators", function()
    suite:it("handles all relational operators correctly", function()
      suite:compileAndRunChecked([[return (3 < 5) and (5 <= 5) and (7 > 3) and (7 >= 7) and (5 ~= 3) and (5 == 5)]])
    end)

    suite:it("handles equality with nil", function()
      suite:compileAndRunChecked([[return nil == nil]])
    end)

    suite:it("handles short-circuiting for 'and'", function()
      suite:compileAndRunChecked([[local x = 5; local y = (x > 10 and error("fail")) or 42; return y]])
    end)

    suite:it("handles short-circuiting for 'or'", function()
      suite:compileAndRunChecked([[return 1 or error("fail")]])
    end)

    suite:it("'and' returns second operand if first is truthy", function()
      suite:compileAndRunChecked([[return 1 and 42]])
    end)
  end)
end)

suite:describe("Statements", function()
  suite:describe("Assignments", function()
    suite:it("handles simple local and global assignments", function()
      suite:compileAndRunChecked([[local a = 1; b = a + 2; return b]])
    end)

    suite:it("handles chained assignments", function()
      suite:compileAndRunChecked([[local a, b, c = 1, 2, 3; a, b = b, a; c = a + b; return c]])
    end)

    suite:it("handles mismatched assignment (padding with nil)", function()
      suite:compileAndRunChecked([[local a, b, c = 1, 2; return a, b, c]])
    end)

    suite:it("handles mismatched assignment (discarding extra values)", function()
      suite:compileAndRunChecked([[local a = 1, 2, 3; return a]])
    end)

    suite:it("handles multi-return function calls in assignments", function()
      suite:compileAndRunChecked([[
        local function f() return 1, 2, 3 end
        local a, b, c, d = 0, f()
        return a, b, c, d
      ]])
    end)
  end)

  suite:describe("Control Flow", function()
    suite:it("handles if-else statements", function()
      suite:compileAndRunChecked([[if true then return 1 else return 1 end]])
      suite:compileAndRunChecked([[if false then return 1 else return 2 end]])
    end)

    suite:it("handles if statement with no else part", function()
      suite:compileAndRunChecked([[if false then return 1 end; return 2]])
    end)

    suite:it("handles if-elseif-else statements", function()
      suite:compileAndRunChecked([[local x = 10; if x > 20 then return 1 elseif x > 5 then return 2 else return 1 end]])
      suite:compileAndRunChecked([[local x = 30; if x > 20 then return 1 elseif x > 5 then return 2 else return 2 end]])
      suite:compileAndRunChecked([[local x = 2; if x > 20 then return 1 elseif x > 5 then return 2 else return 3 end]])
    end)

    suite:it("handles do..end blocks for scoping", function()
      suite:compileAndRunChecked([[local a = 1; do local a = 2 end; return a]])
    end)
  end)

  suite:describe("Return Statements", function()
    suite:it("handles multiple return values", function()
      suite:compileAndRunChecked([[return 1, 2, 3]])
    end)

    suite:it("handles single return from multi-return function wrapped in parens", function()
      suite:compileAndRunChecked([[local function f() return 1, 2, 3 end; local a, b, c = (f()); return a, b, c]])
    end)

    suite:it("handles return from inside a loop", function()
      suite:compileAndRunChecked([[for i = 1, 10 do if i == 5 then return i*2 end end]])
    end)
  end)
end)

suite:describe("Loops", function()
  suite:it("handles while loops", function()
    suite:compileAndRunChecked([[local i = 5; local sum = 0; while i > 0 do sum = sum + i; i = i - 1 end; return sum]])
  end)

  suite:it("handles repeat-until loops", function()
    suite:compileAndRunChecked([[local i = 5; repeat i = i - 1 until i <= 0; return i]])
  end)

  suite:describe("Numeric For Loops", function()
    suite:it("handles basic numeric for loop", function()
      suite:compileAndRunChecked([[local sum = 0; for i = 1, 5 do sum = sum + i end; return sum]])
    end)

    suite:it("handles numeric for loop with a step value", function()
      suite:compileAndRunChecked([[local sum = 0; for i = 10, 1, -2 do sum = sum + i end; return sum]])
    end)

    suite:it("handles numeric for loop with a floating point range", function()
      suite:compileAndRunChecked([[local sum = 0; for i = 0.5, 2.5, 0.5 do sum = sum + i end; return sum]])
    end)
  end)

  suite:describe("Generic For Loops", function()
    suite:it("works with ipairs", function()
      suite:compileAndRunChecked([[local sum = 0; for _, v in ipairs({5, 4, 3}) do sum = sum + v end; return sum]])
    end)

    suite:it("works with pairs", function()
      suite:compileAndRunChecked(
      [[local t = {a=1, b=2}; local sum=0; for k, v in pairs(t) do sum = sum + v end; return sum]])
    end)

    suite:it("works with a custom iterator", function()
      suite:compileAndRunChecked([[
        local sum = 0
        for v in (function()
          local n=0;
          return function()
            n=n+1;
            return n<=3 and n*3 or nil
          end
        end)() do
          sum = sum + v
        end
        return sum
      ]])
    end)
  end)

  suite:describe("Break Statement", function()
    suite:it("breaks out of a numeric for loop", function()
      suite:compileAndRunChecked([[local sum = 0; for i = 1, 10 do sum=sum+i; if i==5 then break end end; return sum]])
    end)
    suite:it("breaks out of a generic for loop", function()
      suite:compileAndRunChecked(
      [[local sum = 0; for _, v in ipairs({1,2,3,4,5,6}) do sum=sum+v; if v==3 then break end end; return sum]])
    end)
    suite:it("breaks out of a while loop", function()
      suite:compileAndRunChecked([[local sum = 0; while true do sum=sum+1; if sum==5 then break end end; return sum]])
    end)
    suite:it("breaks out of a repeat loop", function()
      suite:compileAndRunChecked([[local sum = 0; repeat sum=sum+1; if sum==5 then break end until false; return sum]])
    end)
  end)
end)

suite:describe("Scoping and Closures", function()
  suite:it("respects basic lexical scoping in do..end blocks", function()
    suite:compileAndRunChecked([[local x = 10; do local x = 20; x = x + 5; end; return x]])
  end)

  suite:it("allows repeated local declarations in different scopes", function()
    suite:compileAndRunChecked([[local x = 1; do local x = 2 end; return x]])
  end)

  suite:describe("Upvalues", function()
    suite:it("captures variables from an outer function", function()
      suite:compileAndRunChecked([[
        local function outer()
          local x=5;
          return function()
            return x
          end
        end;
        return outer()()
      ]])
    end)

    suite:it("modifies captured upvalues", function()
      suite:compileAndRunChecked([[
        local function outer()
          local x=5;
          return function()
            x=x+1;
            return x
          end
        end;
        local i=outer();
        i();
        return i()
      ]])
    end)

    suite:it("handles multi-level closures", function()
      suite:compileAndRunChecked([[
        local function l1()
          local a=1;
          return function()
            local b=2;
            return function()
              return a+b
            end
          end
        end;
        return l1()()()
      ]])
    end)
  end)
end)

suite:describe("Functions", function()
  suite:describe("Definitions", function()
    suite:it("handles anonymous function expressions", function()
      suite:compileAndRunChecked([[
        local f = function()
          return 42
        end;
        return f()
      ]])
    end)
    suite:it("handles named function syntax sugar", function()
      suite:compileAndRunChecked([[function f() return 1 end; return f()]])
    end)
    suite:it("handles local function syntax for recursion", function()
      suite:compileAndRunChecked([[
        local function fact(n)
          if n==0 then
            return 1
          else
            return n*fact(n-1)
          end
        end;
        return fact(5)
      ]])
    end)
    suite:it("handles table method definitions", function()
      suite:compileAndRunChecked([[
        local t={x=10};
        function t:add(y)
          return self.x+y
        end;
        return t:add(5)
      ]])
    end)
  end)

  suite:describe("Calls", function()
    suite:it("handles parenthesis-less calls with a string literal", function()
      suite:compileAndRunChecked([[
        local s="";
        local function f(x)
          s=x
        end;
        f"hello";
        return s
      ]])
    end)
    suite:it("handles parenthesis-less calls with a table constructor", function()
      suite:compileAndRunChecked([[
        local t;
        local function f(x)
          t=x
        end;
        f{1,2};
        return t[2]
      ]])
    end)
    suite:it("handles method calls with the colon syntax", function()
      suite:compileAndRunChecked([[
        local t={x=10, f=function(self,y)
          return self.x+y
        end};
        return t:f(5)
      ]])
    end)
  end)

  suite:describe("Varargs", function()
    suite:it("captures variable arguments", function()
      suite:compileAndRunChecked([[
        local f = function(...)
          local t={...};
          return t[2]
        end;
        return f(1,2,3)
      ]])
    end)
  end)

  suite:describe("Tail Calls", function()
    suite:it("performs tail calls to avoid stack overflow", function()
      suite:compileAndRunChecked([[
          local function f(n)
            if n > 0 then
              return f(n - 1)
            else
              return 42
            end
          end
          return f(2000) -- Would normally cause a stack overflow
      ]])
    end)
  end)
end)

suite:describe("Tables", function()
  suite:it("handles comma and semicolon separators", function()
    suite:compileAndRunChecked([[return {1, 2, 3}]])
    suite:compileAndRunChecked([[return {1; 2; 3}]])
  end)

  suite:it("handles empty table constructors", function()
    suite:compileAndRunChecked([[return {}]])
  end)

  suite:it("handles trailing separators", function()
    suite:compileAndRunChecked([[return {1, 2, 3,}]])
    suite:compileAndRunChecked([[return {a=1, b=2, c=3,}]])
    suite:compileAndRunChecked([[return {1; 2; 3;}]])
    suite:compileAndRunChecked([[return {a=1; b=2; c=3;}]])
  end)

  suite:it("handles array-style constructors", function()
    suite:compileAndRunChecked([[return ({1, 2, 3})[2] ]])
  end)

  suite:it("handles hash-style and mixed constructors", function()
    suite:compileAndRunChecked([[return ({a = 1, ["b"] = 2, [3] = 3, 4})["b"] ]])
  end)

  suite:it("handles multi-return call as the last element", function()
    suite:compileAndRunChecked([[
      local function f() return 3, 4, 5 end
      local t = {1, 2, f()}
      return t[1], t[2], t[3], t[4], t[5]
    ]])
  end)
end)

suite:describe("Error Handling and Syntax", function()
  suite:it("detects basic syntax errors", function()
    suite:assertCompileError("return 1 + + 2")
  end)
end)

suite:describe("Complex General Tests", function()
  suite:it("correctly computes factorial", function()
    suite:compileAndRunChecked([[
      local function factorial(n)
        if n == 0 then
          return 1
        else
          return n * factorial(n - 1)
        end
      end

      return factorial(10)
    ]])
  end)

  suite:it("correctly computes fibonacci", function()
    suite:compileAndRunChecked([[
      local function fib(n)
        if n <= 1 then
          return n
        else
          return fib(n - 1) + fib(n - 2)
        end
      end

      return fib(10)
    ]])
  end)

  suite:it("Self-compilation", function()
    -- NOTE: This test might take a while to run.
    local testCode = [[
      local code = io.open("the-tiny-lua-compiler.lua"):read("*a")
      local tlc  = suite:compileAndRun(code)
      local code_to_run = "return 2 * 10 + (function() return 2 * 5 end)()"

      local tokens   = tlc.Tokenizer.new(code_to_run):tokenize()
      local ast      = tlc.Parser.new(tokens):parse()
      local proto    = tlc.CodeGenerator.new(ast):generate()
      local bytecode = tlc.BytecodeEmitter.new(proto):emit()
      local func     = loadstring(bytecode)

      return func()
    ]]

    _G.suite = suite
    suite:assertEqual(suite:compileAndRun(testCode), 30)
    _G.suite = nil
  end)
end)

return suite:summary()
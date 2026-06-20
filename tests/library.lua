local tlc = require("tlc")

local loadstring = (loadstring or load)

local INFINITE_LOOP_LIMIT = 15000000

local COLOR_PASS = "\27[42m\27[30m PASS \27[0m"
local COLOR_FAIL = "\27[41m\27[30m FAIL \27[0m"
local COLOR_SKIP = "\27[43m\27[30m SKIP \27[0m"

local Suite = {}
Suite.__index = Suite

local function serialize(value, seen)
  local valueType = type(value)
  if valueType == "string" then
    return string.format("%q", value)
  elseif valueType ~= "table" then
    return tostring(value)
  end

  seen = seen or {}
  if seen[value] then
    return "<cycle>"
  end
  seen[value] = true

  local keys = {}
  for key in pairs(value) do
    table.insert(keys, key)
  end

  table.sort(keys, function(left, right)
    return tostring(left) < tostring(right)
  end)

  local parts = {}
  for _, key in ipairs(keys) do
    table.insert(parts, tostring(key) .. " = " .. serialize(value[key], seen))
  end

  seen[value] = nil
  return "{" .. table.concat(parts, ", ") .. "}"
end

local function deepEqual(left, right, seen)
  seen = seen or {}
  if left == right then
    return true
  elseif type(left) ~= "table" or type(right) ~= "table" then
    return false
  elseif seen[left] or seen[right] then
    return seen[left] == right or seen[right] == left
  end

  seen[left] = right
  seen[right] = left

  for key, leftValue in pairs(left) do
    if not deepEqual(leftValue, right[key], seen) then
      return false
    end
  end

  for key in pairs(right) do
    if left[key] == nil then
      return false
    end
  end

  return true
end

local function matchShape(expected, actual, path)
  if type(expected) ~= "table" then
    return expected == actual, path, expected, actual
  elseif type(actual) ~= "table" then
    return false, path, expected, actual
  end

  for key, expectedValue in pairs(expected) do
    local childPath = path .. "." .. tostring(key)
    local ok, failedPath, left, right =
      matchShape(expectedValue, actual[key], childPath)
    if not ok then
      return false, failedPath, left, right
    end
  end

  return true
end

local function captureExecution(fn)
  return {
    xpcall(fn, function(err)
      return {
        message = tostring(err),
        traceback = debug.traceback and debug.traceback("", 2):sub(2) or "",
      }
    end),
  }
end

function Suite.new()
  return setmetatable({
    groups = {},
    results = {},
  }, Suite)
end

function Suite:describe(name, func)
  table.insert(self.groups, name)
  func()
  table.remove(self.groups)
end

function Suite:skip(reason)
  error({ __suiteSkip = true, reason = reason or "skipped" }, 0)
end

function Suite:itIf(condition, name, func, reason)
  if condition then
    return self:it(name, func)
  end

  return self:it(name, function()
    self:skip(reason or "condition not met")
  end)
end

function Suite:it(name, func)
  local path = table.concat(self.groups, " -> ") .. " -> " .. name
  local startedAt = os.clock()

  local function unhook()
    if debug.sethook then
      debug.sethook()
    end
  end

  local function terminateInfiniteLoop()
    unhook()
    error(
      string.format(
        "TLCTest: Infinite loop detected after %d instructions",
        INFINITE_LOOP_LIMIT
      ),
      0
    )
  end

  if debug.sethook then
    debug.sethook(terminateInfiniteLoop, "", INFINITE_LOOP_LIMIT)
  end

  local ok, result = xpcall(func, function(err)
    if type(err) == "table" and err.__suiteSkip then
      return err
    end

    return {
      message = tostring(err),
      traceback = debug.traceback and debug.traceback("", 2):sub(2) or "",
    }
  end)

  unhook()

  local durationMs = (os.clock() - startedAt) * 1000
  local entry = {
    path = path,
    durationMs = durationMs,
  }

  if ok then
    print(COLOR_PASS .. " " .. path)
    entry.status = "PASS"
    table.insert(self.results, entry)
    return
  end

  if type(result) == "table" and result.__suiteSkip then
    print(COLOR_SKIP .. " " .. path .. " (" .. result.reason .. ")")
    entry.status = "SKIP"
    entry.reason = result.reason
    table.insert(self.results, entry)
    return
  end

  print(COLOR_FAIL .. " " .. path)
  entry.status = "FAIL"
  entry.error = result
  table.insert(self.results, entry)
end

function Suite:compileAndRun(code)
  if _VERSION == "Lua 5.1" then
    -- TODO: When running on Lua 5.1, we probably should run it both in the VM
    -- and through `loadstring`.

    local func, err = loadstring(tlc.compile(code))
    if not func then
      error(err, 0)
    end
    return func()
  end

  return tlc.run(code)
end

function Suite:assertEqual(expected, actual, message)
  if expected == actual then
    return true
  end

  error(
    (message and (message .. " - ") or "")
      .. string.format(
        "Expected %s, got %s",
        serialize(expected),
        serialize(actual)
      ),
    2
  )
end

function Suite:assertDeepEqual(expected, actual, message)
  if deepEqual(expected, actual) then
    return true
  end

  error(
    (message and (message .. " - ") or "")
      .. string.format(
        "Expected %s, got %s",
        serialize(expected),
        serialize(actual)
      ),
    2
  )
end

function Suite:assertMatchesShape(expected, actual, message)
  local ok, path, left, right = matchShape(expected, actual, "$")
  if ok then
    return true
  end

  error(
    (message and (message .. " - ") or "")
      .. string.format(
        "Shape mismatch at %s: expected %s, got %s",
        path,
        serialize(left),
        serialize(right)
      ),
    2
  )
end

function Suite:assertError(func, pattern, message)
  local ok, err = xpcall(func, function(runtimeError)
    return tostring(runtimeError)
  end)

  if ok then
    error(message or "Expected an error, but the call succeeded", 2)
  end

  if pattern and not tostring(err):match(pattern) then
    error(
      (message and (message .. " - ") or "")
        .. string.format(
          "Expected error %q to match pattern %q",
          tostring(err),
          pattern
        ),
      2
    )
  end

  return err
end

function Suite:assertMatchesLua(code)
  local expected = captureExecution(function()
    local func, err = loadstring(code)
    if not func then
      error(err, 0)
    end
    return func()
  end)
  local actual = captureExecution(function()
    return self:compileAndRun(code)
  end)

  local expectedOk = table.remove(expected, 1)
  local actualOk = table.remove(actual, 1)

  if expectedOk ~= actualOk then
    local expectedStatus = expectedOk and "succeeded" or "failed"
    local actualStatus = actualOk and "succeeded" or "failed"
    error(
      "Execution status mismatch: standard Lua "
        .. expectedStatus
        .. ", TLC "
        .. actualStatus,
      2
    )
  elseif not expectedOk then
    local expectedError = expected[1] and expected[1].message or "<unknown>"
    local actualError = actual[1] and actual[1].message or "<unknown>"
    error(
      "Both executions errored unexpectedly. Standard Lua: "
        .. expectedError
        .. " | TLC: "
        .. actualError,
      2
    )
  end

  return self:assertDeepEqual(expected, actual)
end

function Suite:summary()
  local passed = 0
  local failed = 0
  local skipped = 0

  for _, result in ipairs(self.results) do
    if result.status == "PASS" then
      passed = passed + 1
    elseif result.status == "FAIL" then
      failed = failed + 1
    elseif result.status == "SKIP" then
      skipped = skipped + 1
    end
  end

  print("\n\27[1mTest Results:\27[0m")
  print(string.format("Passed:  \27[32m%d\27[0m", passed))
  print(string.format("Failed:  \27[31m%d\27[0m", failed))
  print(string.format("Skipped: \27[33m%d\27[0m", skipped))
  print(string.format("Total:   %d", passed + failed + skipped))

  if failed > 0 then
    print("\n\27[1mErrors:\27[0m")
    local errorIndex = 0
    for _, result in ipairs(self.results) do
      if result.status == "FAIL" then
        errorIndex = errorIndex + 1
        print(string.format("\n%d) \27[1m%s\27[0m", errorIndex, result.path))
        print(string.format("   \27[31m%s\27[0m", result.error.message))
        if result.error.traceback ~= "" then
          print(string.format("   \27[90m%s\27[0m", result.error.traceback))
        end
      end
    end
  end

  return (failed == 0 and 0) or 1
end

return Suite

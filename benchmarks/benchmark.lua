--[[
  TLC Performance Benchmark
  -------------------------
--]]

local tlc = require("./../tlc")

-- Configuration --
local ITERATIONS = 500
local FILES = {
  { path = "benchmarks/files/small.lua", name = "small" },
  { path = "benchmarks/files/medium.lua", name = "medium" },
  { path = "benchmarks/files/large.lua", name = "large" },
}

-- Utilities --
local function readFile(path)
  local success, fs = pcall(require, "@lute/fs")

  if io and io.open then
    local file = io.open(path, "r")
    if not file then
      error("Could not open: " .. path)
    end
    local content = file:read("*a")
    file:close()
    return content
  elseif success then
    local file = fs.open(path, "r")
    if not file then
      error("Could not open: " .. path)
    end
    local content = fs.read(file)
    fs.close(file)
    return content
  end

  error("No file system API available")
end

local function measure(func, iterations)
  func() -- warmup
  local start = os.clock()
  for _ = 1, iterations do
    func()
  end
  return (os.clock() - start) * 1000
end

local function fmtMs(ms)
  return string.format("%8.2f", ms)
end

-- Benchmark --
local function run()
  print("")
  print("TLC Benchmark | " .. _VERSION .. " | " .. ITERATIONS .. " iterations")
  print(string.rep("-", 72))
  print("file       tokenizer   parser    codegen   bytecode   total")
  print(string.rep("-", 72))

  local totals =
    { tokenizer = 0, parser = 0, codegen = 0, bytecode = 0, total = 0 }

  for _, f in ipairs(FILES) do
    local src = readFile(f.path)
    local tokens = tlc.tokenize(src)
    local ast = tlc.parseTokens(tokens)
    local proto = tlc.generate(ast)

    local t1 = measure(function()
      tlc.tokenize(src)
    end, ITERATIONS)
    local t2 = measure(function()
      tlc.parseTokens(tokens)
    end, ITERATIONS)
    local t3 = measure(function()
      tlc.generate(ast)
    end, ITERATIONS)
    local t4 = measure(function()
      tlc.emit(proto)
    end, ITERATIONS)
    local total = t1 + t2 + t3 + t4

    print(
      string.format(
        "%-10s %s  %s  %s  %s  %s",
        f.name,
        fmtMs(t1),
        fmtMs(t2),
        fmtMs(t3),
        fmtMs(t4),
        fmtMs(total)
      )
    )

    totals.tokenizer = totals.tokenizer + t1
    totals.parser = totals.parser + t2
    totals.codegen = totals.codegen + t3
    totals.bytecode = totals.bytecode + t4
    totals.total = totals.total + total
  end

  print(string.rep("-", 72))
  print(
    string.format(
      "%-10s %s  %s  %s  %s  %s",
      "TOTAL",
      fmtMs(totals.tokenizer),
      fmtMs(totals.parser),
      fmtMs(totals.codegen),
      fmtMs(totals.bytecode),
      fmtMs(totals.total)
    )
  )
  print("")
end

run()

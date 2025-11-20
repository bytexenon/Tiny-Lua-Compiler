--[[
  The Tiny Lua Compiler Performance Test Suite

  Lua 5.1 benchmark results (over 5000 iterations):
    Tokenizer:       175.9758 seconds (51.36%) - 28.41 iterations/second
    Parser:          43.9889 seconds (12.84%) - 113.66 iterations/second
    CodeGenerator:   48.5441 seconds (14.17%) - 103.00 iterations/second
    BytecodeEmitter: 74.1044 seconds (21.63%) - 67.47 iterations/second
    Total:           342.6132 seconds

  LuaJIT benchmark results (over 5000 iterations):
    Tokenizer:       20.8723 seconds (9.91%) - 239.55 iterations/second
    Parser:          11.6405 seconds (5.53%) - 429.54 iterations/second
    CodeGenerator:   158.4915 seconds (75.24%) - 31.55 iterations/second
    BytecodeEmitter: 19.6458 seconds (9.33%) - 254.51 iterations/second
    Total:           210.6501 seconds
--]]

--* Imports *--
local tlc = require("./../the-tiny-lua-compiler")

-- Constants --
local ITERATIONS = 500
local TLC_CODE   = io.open("./the-tiny-lua-compiler.lua", "r"):read("*a")

--* Functions *--
local function benchmark_tokenizer(code)
  local start = os.clock()
  for _ = 1, ITERATIONS do
    tlc.Tokenizer.new(code):tokenize()
  end
  return os.clock() - start
end

local function benchmark_parser(tokens)
  local start = os.clock()
  for _ = 1, ITERATIONS do
    tlc.Parser.new(tokens):parse()
  end
  return os.clock() - start
end

local function benchmark_codegenerator(ast)
  local start = os.clock()
  for _ = 1, ITERATIONS do
    tlc.CodeGenerator.new(ast):generate()
  end
  return os.clock() - start
end

local function benchmark_bytecodeemitter(proto)
  local start = os.clock()
  for _ = 1, ITERATIONS do
    tlc.BytecodeEmitter.new(proto):emit()
  end
  return os.clock() - start
end

local function benchmark()
  local tokens = tlc.Tokenizer.new(TLC_CODE):tokenize()
  local ast    = tlc.Parser.new(tokens):parse()
  local proto  = tlc.CodeGenerator.new(ast):generate()

  local tokenizer_elapsed     = benchmark_tokenizer(TLC_CODE)
  local parser_elapsed        = benchmark_parser(tokens)
  local codegenerator_elapsed = benchmark_codegenerator(ast)
  local bytecodeemitter_elapsed      = benchmark_bytecodeemitter(proto)

  local total_elapsed = tokenizer_elapsed + parser_elapsed + codegenerator_elapsed + bytecodeemitter_elapsed
  local tokenizer_elapsed_perc        = (tokenizer_elapsed / total_elapsed) * 100
  local parser_elapsed_perc           = (parser_elapsed / total_elapsed) * 100
  local codegenerator_elapsed_perc    = (codegenerator_elapsed / total_elapsed) * 100
  local bytecodeemitter_elapsed_perc  = (bytecodeemitter_elapsed / total_elapsed) * 100
  local tokenizer_iters_per_sec       = ITERATIONS / tokenizer_elapsed
  local parser_iters_per_sec          = ITERATIONS / parser_elapsed
  local codegenerator_iters_per_sec   = ITERATIONS / codegenerator_elapsed
  local bytecodeemitter_iters_per_sec = ITERATIONS / bytecodeemitter_elapsed

  print(string.format("Benchmark Results (over %d iterations):", ITERATIONS))
  print(string.format("Tokenizer:     %.4f seconds (%.2f%%) - %.2f iterations/second", tokenizer_elapsed,
    tokenizer_elapsed_perc, tokenizer_iters_per_sec))
  print(string.format("Parser:        %.4f seconds (%.2f%%) - %.2f iterations/second", parser_elapsed,
    parser_elapsed_perc, parser_iters_per_sec))
  print(string.format("CodeGenerator: %.4f seconds (%.2f%%) - %.2f iterations/second", codegenerator_elapsed,
    codegenerator_elapsed_perc, codegenerator_iters_per_sec))
  print(string.format("BytecodeEmitter: %.4f seconds (%.2f%%) - %.2f iterations/second", bytecodeemitter_elapsed,
    bytecodeemitter_elapsed_perc, bytecodeemitter_iters_per_sec))
  print(string.format("Total:         %.4f seconds", total_elapsed))
end

--* Main *--
benchmark()
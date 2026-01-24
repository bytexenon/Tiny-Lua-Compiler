local function fib(n)
  if n < 2 then
    return n
  else
    return fib(n - 1) + fib(n - 2)
  end
end

local n = tonumber(arg and arg[1]) or 20
fib(n)

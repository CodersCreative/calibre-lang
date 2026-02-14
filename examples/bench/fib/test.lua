function fib(n)
  if n < 2 then return n else return fib(n-1) + fib(n-2) end
end

local n = 30
local result = fib(n)
print(result)

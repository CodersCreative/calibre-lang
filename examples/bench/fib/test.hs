fib :: Int -> Int
fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
    let n = 28
    let result = fib n
    print result
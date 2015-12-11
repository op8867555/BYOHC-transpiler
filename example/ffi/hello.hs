foreign import ccall "hello.hello" hello :: Int -> String

main = putStrLn (hello 1)

data T where
     A :: T
     B :: T -> T

f = \x -> case x of B y -> (+) 1 (f y)
                    _ -> 0
main = putStrLn (case (f (B A)) of 0 -> "0"; 1 -> "1")


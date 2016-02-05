data Maybe :: * -> * where
     Nothing :: Maybe a
     Just :: a -> Maybe a

data Pair where
     Pair :: a -> b -> Pair a b

otherwise = True

t = \x -> case x of
               a | otherwise -> "a | otherwise"
               b | Just (Just (Pair "hi" 0)) <- x -> "hi"
               c | let x = "let test" -> x

main = putStrLn (t (Just (Just (Pair "hi" 0))))

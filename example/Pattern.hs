data Maybe :: * -> * where
     Nothing :: Maybe a
     Just :: a -> Maybe a

data Pair where
     Pair :: a -> b -> Pair a b

t = \x -> case x of
               Just Nothing -> "Just Nothing"
               Just (Just (Pair a 0)) -> a
               Nothing -> "Nothing"


main = putStrLn (t (Just (Just (Pair "hi" 0))))


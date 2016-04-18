data Maybe :: * -> * where
     Nothing :: Maybe a
     Just :: a -> Maybe a

data Pair where
     Pair :: a -> b -> Pair a b

undefined = undefined

t = \x -> case x of
               Just Nothing -> "Just Nothing"
               Just (Just (Pair a 0)) -> a
               Nothing -> "Nothing"

-- `irrefutable undefined` should return "hi"
irrefutable = \x -> case x of
                        ~(Pair a b) -> "hi"
-- `nonIrrefutable undefined` should fail
nonIrrefutable = \x -> case x of
                            (Pair a b) -> "fail anyway"


main = putStrLn (t (Just (Just (Pair "hi" 0))))


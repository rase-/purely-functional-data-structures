data List a = Empty | Cons a (List a) deriving (Show)

mhead :: List a -> a
mhead Empty = error "Empty list"
mhead (Cons x xs) = x

mtail :: List a -> List a
mtail Empty = Empty
mtail (Cons x xs) = xs

mconcat :: List a -> List a -> List a
mconcat Empty ys = ys
mconcat (Cons x xs) ys = Cons x (mconcat xs ys)

update :: List a -> Int -> a -> List a
update Empty i y = Empty
update (Cons x xs) 0 y = Cons y xs
update (Cons x xs) i y = Cons x (update xs (i - 1) y)

suffixes :: List a -> List (List a)
suffixes xs = Cons xs (suffixes' xs)
  where suffixes' Empty = Empty
        suffixes' (Cons x xs) = Cons xs (suffixes' xs)

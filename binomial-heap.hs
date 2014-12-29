type Rank = Int
data Heap a = Node Rank a [Heap a]

link :: (Ord a) => Heap a -> Heap a -> Heap a
link h1@(Node r1 x xs) h2@(Node r2 y ys)
  | r1 /= r2 = error "Can only link trees of same rank"
  | x < y = Node (r1 + 1) x (h2 : xs)
  | otherwise = Node (r1 + 1) y (h1 : ys)

rank :: Heap a -> Rank
rank (Node r _ _) = r

insTree :: Heap a -> Heap a -> Heap a
insTree h (Heap _ _ []) -> Heap [h]
insTree (Heap _ x xs) (Heap _ y ys) = 

insert :: a -> Heap a -> Heap a
insert x h = insTree (Node 0 x []) h

type Rank = Int
data Heap a = Empty | Node Rank a (Heap a) (Heap a) deriving (Show)

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Empty h = h
merge h Empty = h
merge h1@(Node _ x xs1 xs2) h2@(Node _ y ys1 ys2)
  | x <= y = makeT x xs1 (merge xs2 h2)
  | otherwise = makeT y ys1 (merge h1 ys2)

rank :: Heap a -> Rank
rank Empty = 0
rank (Node r _ _ _) = r

makeT :: (Ord a) => a -> Heap a -> Heap a -> Heap a
makeT x a b
  | rank a >= rank b = Node (rank b + 1) x a b
  | otherwise = Node (rank a + 1) x b a

insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = merge (Node 1 x Empty Empty) h

findMin :: (Ord a) => Heap a -> a
findMin Empty = error "Empty heap"
findMin (Node _ x _ _) = x

deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin Empty = error "Empty heap"
deleteMin (Node _ _ a b) = merge a b

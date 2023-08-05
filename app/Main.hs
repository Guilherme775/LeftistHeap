module Main where

data LeftistHeap a = Empty | Node {rank :: Int, value :: a, left :: LeftistHeap a, right :: LeftistHeap a}
  deriving (Show)

comparator :: Ord a => a -> a -> Bool
comparator x y = x < y

getRank :: LeftistHeap a -> Int
getRank Empty = 0
getRank h = rank h

makeNode :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeNode x a b
  | getRank a >= getRank b = Node {rank = getRank b + 1, value = x, left = a, right = b}
  | otherwise = Node {rank = getRank a + 1, value = x, left = b, right = a}

empty :: LeftistHeap a
empty = Empty

isEmpty :: LeftistHeap a -> Bool
isEmpty Empty = True
isEmpty _ = False

merge :: Ord a => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
merge h Empty = h
merge Empty h = h
merge h1@(Node {rank = _, value = x, left = a1, right = b1}) h2@(Node {rank = _, value = y, left = a2, right = b2})
  | comparator x y = makeNode x a1 (merge b1 h2)
  | otherwise = makeNode y a2 (merge h1 b2)

createNode :: a -> LeftistHeap a
createNode x = Node {rank = 1, value = x, left = Empty, right = Empty}

insert :: Ord a => LeftistHeap a -> a -> LeftistHeap a
insert h x = merge (createNode x) h

findMin :: LeftistHeap a -> Maybe a
findMin Empty = Nothing
findMin (Node {rank = _, value = x, left = _, right = _}) = Just x

deleteMin :: Ord a => LeftistHeap a -> LeftistHeap a
deleteMin Empty = Empty
deleteMin (Node {rank = _, value = _, left = a, right = b}) = merge a b

main :: IO ()
main = print "LeftistHeap yey"
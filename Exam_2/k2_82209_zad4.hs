main :: IO()
main = do
    print $ isPerfectlyBalanced t1 == True

data BTree a = Nil | Node a (BTree a) (BTree a)
    deriving(Show)

t1 = Node 'H' (Node 'a' (Node 'k' Nil Nil) (Node 'e' Nil Nil)) (Node 's' (Node 'l' Nil Nil) (Node 'l' Nil Nil))

getSize :: BTree a -> Int
getSize Nil = 0
getSize (Node _ left right) = 1 + getSize left + getSize right

getHeight :: BTree a -> Int
getHeight Nil = 0
getHeight (Node _ left right) = 1 + max (getHeight left) (getHeight right)

isPerfectlyBalanced :: BTree a -> Bool
isPerfectlyBalanced Nil = True -- it is covered by the second case but with not relation to other functions(speed up the program as a whole?)
isPerfectlyBalanced binaryTree = (getSize binaryTree) == 2^(getHeight binaryTree) - 1








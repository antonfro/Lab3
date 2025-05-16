{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a
  get,           -- Ord a => a -> AATree a -> Maybe a
  insert,        -- Ord a => a -> AATree a -> AATree a
  inorder,       -- AATree a -> [a]
  isSorted,
  remove,        -- Ord a => a -> AATree a -> AATree a
  size,          -- AATree a -> Int
  height,        -- AATree a -> Int
  checkTree,      -- Ord a => AATree a -> Bool
  create
 ) where

--------------------------------------------------------------------------------

-- AA search trees
data AATree a = Empty | Node Int (AATree a) a (AATree a) -- What a node looks like -> (Node (Level k) (LeftTree l) (Data d) (RightTree r))
  deriving (Eq, Show, Read)

emptyTree :: AATree a -- O(1)
emptyTree = Empty

get :: Ord a => a -> AATree a -> Maybe a -- O(log n)
get _ Empty = Nothing
get x (Node _ l d r)
        | x == d = Just d
        | x < d = get x l
        | x > d = get x r
        | otherwise = Nothing

split :: AATree a -> AATree a -- O(1)
split (Node tk tl td (Node rk rl rd (Node rrk rrl rrd rrr))) -- t = thisTree, r = rightTree (rr = rightRightTree)
        | tk == rrk = Node (rk+1) (Node tk tl td rl) rd (Node rrk rrl rrd rrr)
split t = t -- t = tree

skew  :: AATree a -> AATree a -- O(1)
skew (Node tk (Node lk ll ld lr) td tr)
        | tk == lk = Node lk ll ld (Node tk lr td tr)
skew t = t

insert :: Ord a => a -> AATree a -> AATree a -- O(n log (n))
insert x Empty = Node 1 Empty x Empty -- if empty tree
insert x (Node k l d r) -- finds the right spot
        | x < d = (split . skew) (Node k (insert x l) d r) -- rotate right if tk = lk
        | x > d = (split . skew) (Node k l d (insert x r)) -- rotate left if tk = rrk
        | otherwise = Node k l d r

-- own function for making a tree out of a list
create :: (Eq a, Ord a) => [a] -> AATree a -- O(n log(n))
create [] = Empty
create (a:as) = create2 as (insert a Empty)

create2 :: (Eq a, Ord a) => [a] -> AATree a -> AATree a -- helper funtion
create2 as t = foldl (flip insert) t as

inorder :: AATree a -> [a] -- O(n log(n))
inorder Empty = []
inorder (Node _ Empty d Empty) = [d]
inorder (Node _ l d r) = inorder l ++ [d] ++ inorder r

size :: AATree a -> Int -- O(n)
size Empty = 0
size (Node _ l _ r) = size l + 1 + size r

height :: AATree a -> Int -- O(n)
height Empty = 0
height (Node _ l _ r) = 1 + max (height l) (height r) -- 1 + the largest child

--------------------------------------------------------------------------------
-- Optional function
remove :: Ord a => a -> AATree a -> AATree a
remove = error "not "
{- We tried
remove :: Ord a => a -> AATree a -> AATree a -- 'Find node then delete max from l and put it there'
remove _ Empty = Empty
remove x (Node k l d r) -- Finds the node that will be deleted
        | x < d = Node k (remove x l) d r
        | x > d = Node k l d (remove x r)
        | x == d = remove2 (Node k l d r)
        where
            remove2 (Node k l d r)
                    | l == Empty && l == Empty = Empty
                    | l /= Empty && r == Empty = Node (lk+1) ll ld lr -- if left child exists
                    | l == Empty && r /= Empty = Node (rk+1) rl rd rr -- if right 
                    | otherwise = Node tk (remove (maxVal l) l) (maxVal l) r
remove _ t = t

maxVal :: Eq a => AATree a -> a
maxVal Empty = error "Empty tree" -- Kanske inte får returnera string ?
maxVal (Node _ _ _ r) = maxVal r 
maxVal (Node _ _ d Empty) = d 
-}

--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants

checkTree :: Ord a => AATree a -> Bool -- O(n^2)
checkTree root =
  isSorted (inorder root) &&
  all checkLevels (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x:nodes (leftSub x) ++ nodes (rightSub x)

-- True if the given list is ordered
isSorted :: Ord a => [a] -> Bool -- O(n)
isSorted [] = True
isSorted [_] = True -- if one element in list
isSorted (x:y:xs) = x < y && isSorted (y:xs) -- compares the two first elements

-- Check if the invariant is true for a single AA node
-- You may want to write this as a conjunction e.g.
--   checkLevels node =
--     leftChildOK node &&
--     rightChildOK node &&
--     rightGrandchildOK node
-- where each conjunct checks one aspect of the invariant

leftChildOK :: AATree a -> Bool -- O(1)
leftChildOK Empty = True -- if empty Node
leftChildOK (Node k Empty _ _) = k == 1 -- if its a leaf
leftChildOK (Node tk (Node lk _ _ _) _ _) = tk > lk

rightChildOK :: AATree a -> Bool -- (1)
rightChildOK Empty = True
rightChildOK (Node k _ _ Empty) = k == 1
rightChildOK (Node tk _ _ (Node rk _ _ _)) = tk >= rk

rightGrandchildOK :: AATree a -> Bool -- O(1)
rightGrandchildOK Empty = True
rightGrandchildOK (Node _ _ _ Empty) = True -- If there is no child
rightGrandchildOK (Node _ _ _ (Node _ _ _ Empty)) = True -- If there is no grandchild.
rightGrandchildOK (Node tk _ _ (Node _ _ _ (Node rrk _ _ _))) = tk > rrk

checkLevels :: AATree a -> Bool -- O(1)
checkLevels Empty = True
checkLevels (Node k Empty _ Empty) = k == 1
checkLevels node = leftChildOK node && rightChildOK node && rightGrandchildOK node

isEmpty :: AATree a -> Bool -- O(1)
isEmpty Empty = True
isEmpty (Node {}) = False -- any Node (Node _ _ _ _)

leftSub :: AATree a -> AATree a -- O(1)
leftSub Empty = Empty
leftSub (Node _ l _ _) = l

rightSub :: AATree a -> AATree a -- O(1)
rightSub Empty = Empty
rightSub (Node _ _ _ r) = r
--------------------------------------------------------------------------------
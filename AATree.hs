{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a
  get,           -- Ord a => a -> AATree a -> Maybe a
  insert,        -- Ord a => a -> AATree a -> AATree a
  inorder,       -- AATree a -> [a]
  remove,        -- Ord a => a -> AATree a -> AATree a
  size,          -- AATree a -> Int
  height,        -- AATree a -> Int
  checkTree      -- Ord a => AATree a -> Bool
 ) where

--------------------------------------------------------------------------------

-- AA search trees
data AATree a = Empty | Node Int (AATree a) a (AATree a) -- What a node looks like -> (Node (Level k) (LeftTree l) (Data d) (RightTree r))
  deriving (Eq, Show, Read)

emptyTree :: AATree a
emptyTree = Empty

get :: Ord a => a -> AATree a -> Maybe a
get _ Empty = Nothing
get x (Node _ l d r)
        | x == d = Just d
        | x < d = get x l
        | x > d = get x r

split :: AATree a -> AATree a
split (Node tk tl td (Node rk rl rd (Node rrk rrl rrd rrr))) -- t = this, r = right (rr = RightRightTree)
        | tk == rrk = Node (rk+1) (Node tk tl td rl) rd (Node rrk rrl rrd rrr)
split t = t -- t = tree

skew  :: AATree a -> AATree a
skew (Node tk (Node lk ll ld lr) td tr)
        | tk == lk = Node lk ll ld (Node tk lr td tr)
skew t = t

insert :: Ord a => a -> AATree a -> AATree a
insert x Empty = Node 1 Empty x Empty -- if empty tree
insert x (Node k l d r) -- finds the right spot
        | x == d = Node k l d r
        | x < d = (split . skew) (Node k (insert x l) d r)
        | x > d = (split . skew) (Node k l d (insert x r))

create :: (Eq a, Ord a) => [a] -> AATree a
create [] = Empty
create (a:as) = create2 as (insert a Empty)

create2 :: (Eq a, Ord a) => [a] -> AATree a -> AATree a
create2 [] t = t
create2 (a:as) (Node k l d r) = create2 as (insert a (Node k l d r))

inorder :: AATree a -> [a]
inorder Empty = []
inorder (Node _ Empty d Empty) = [d]
inorder (Node _ l d r) = inorder l ++ [d] ++ inorder r

size :: AATree a -> Int
size Empty = 0
size (Node _ l _ r) = size l + 1 + size r

height :: AATree a -> Int
height Empty = 0
height (Node k _ _ _) = k

--------------------------------------------------------------------------------
-- Optional function

remove :: Ord a => a -> AATree a -> AATree a -- 'Find node then delete max from l and put it there'
remove _ Empty = Empty
remove x (Node k l d r) -- Finds the node that will be deleted
        | x == d = remove2 (Node k l d r)
        | x < d = Node k (remove x l) d r
        | x > d = Node k l d (remove x r)
        where
            remove2 (Node tk (Node lk ll ld lr) _ (Node rk rl rd rr))
                    | l == Empty && r == Empty = Empty
                    | l /= Empty && r == Empty = Node (lk+1) ll ld lr 
                    | l == Empty && r /= Empty = Node (rk+1) rl rd rr
                    | otherwise = Node tk (remove (maxVal l) l) (maxVal l) r

maxVal :: Eq a => AATree a -> a
maxVal Empty = error "Empty tree"
maxVal (Node _ _ d r)
        | r == Empty = d
        | r /= Empty = maxVal r

--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants

checkTree :: Ord a => AATree a -> Bool
checkTree root =
  isSorted (inorder root) &&
  all checkLevels (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x:nodes (leftSub x) ++ nodes (rightSub x)

-- True if the given list is ordered
isSorted :: Ord a => [a] -> Bool
isSorted = error "isSorted not implemented"

-- Check if the invariant is true for a single AA node
-- You may want to write this as a conjunction e.g.
--   checkLevels node =
--     leftChildOK node &&
--     rightChildOK node &&
--     rightGrandchildOK node
-- where each conjunct checks one aspect of the invariant
checkLevels :: AATree a -> Bool
checkLevels = error "checkLevels not implemented"

isEmpty :: AATree a -> Bool
isEmpty = error "isEmpty not implemented"

leftSub :: AATree a -> AATree a
leftSub = error "leftSub not implemented"

rightSub :: AATree a -> AATree a
rightSub = error "rightSub not implemented"

--------------------------------------------------------------------------------

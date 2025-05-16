{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}

import AATree

--------------------------------------------------------------------------------

main :: IO ()
main = do
  contents <- getContents
  let tree = create $ words contents
  putStrLn ("Size: " ++ show (size tree))
  let heightT = height tree
  putStrLn ("Height: " ++ show heightT)
  let optH = ceiling (logBase 2 (fromIntegral (size tree+1))-1)
  putStrLn ("Optimal height: " ++ show optH)
  putStrLn ("Height / Optimal height: " ++ show (fromIntegral heightT / fromIntegral optH))
  putStrLn ("checkTree: " ++ show (checkTree tree))
  putStrLn ("First twenty words: " ++ show (unwords (take 20 (inorder tree))))

  -- split the data into words and build an AA tree
  -- use foldl

  -- calculate and print statistics
  -- use fromIntegral/ceiling/logBase

--------------------------------------------------------------------------------


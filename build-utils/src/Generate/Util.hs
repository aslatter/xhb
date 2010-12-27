
module Generate.Util where

import Data.Char

ensureUpper :: String -> String
ensureUpper [] = []
ensureUpper (x:xs) = toUpper x : xs


-- | filterAccum f xs = (filter f xs, filter (not . f) xs)
filterAccum :: (a -> Bool) -> [a] -> ([a],[a])
filterAccum f xs = go xs
 where
   go xs =
       let done = null xs           
           (y:ys) = xs
           passed = f y

           newPasses | done = []
                     | passed = y:passes
                     | otherwise = passes

           newFails | done = []
                    | passed = fails
                    | otherwise = y:fails

           (passes, fails) = go ys
        in (newPasses, newFails)

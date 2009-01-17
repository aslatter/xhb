
module Generate.Util where

import Data.Char

ensureUpper :: String -> String
ensureUpper [] = []
ensureUpper (x:xs) = toUpper x : xs


-- | filterAccum f xs = (filter f xs, filter (not . f) xs)
filterAccum :: (a -> Bool) -> [a] -> ([a],[a])
filterAccum _ [] = ([],[])
filterAccum f (x:xs) =
    let (passes, fails) = filterAccum f xs

        passed = f x

        newPasses | passed = x:passes
                  | otherwise = passes

        newFails | passed = fails
                 | otherwise = x:fails

    in (newPasses, newFails)

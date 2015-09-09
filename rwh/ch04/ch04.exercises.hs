import Data.List (foldl)
import Data.Char (digitToInt)

safeHead :: [a] -> Maybe a
safeHead lst = if null lst
               then Nothing
               else Just (head lst)

safeTail :: [a] -> Maybe [a]
safeTail lst = if null lst
               then Nothing
               else Just (tail lst)

safeLast :: [a] -> Maybe a
safeLast lst = if null lst
               then Nothing
               else Just (last lst)

safeInit :: [a] -> Maybe [a]
safeInit lst = if null lst
               then Nothing
               else Just (init lst)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith splitFun lst =
  let (pre, suf) = break splitFun lst
  in pre : [suf]

breakFun x = if x == 'a' then True else False

asInt_fold :: String -> Int
asInt_fold ('-':xs) = negate $ asInt_fold xs
asInt_fold xs = foldl step 0 xs
  where step acc n = acc * 10 + (digitToInt n)

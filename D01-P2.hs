main = do
    v <- readLn
    print $ sum $ takeWhile (> 0) $ iterate f v

f :: Int -> Int
f x = (x `div` 3) - 2

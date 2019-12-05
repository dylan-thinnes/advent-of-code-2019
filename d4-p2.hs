{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List

check s = pair s && monotonic s

monotonic cs = foldl (&&) True statuses
    where
    statuses = zipWith (<=) cs (tail cs)

pair = elem 2 . map length . group

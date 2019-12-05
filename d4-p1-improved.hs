import Data.Foldable

newtype Status = Status { unstatus :: (Bool, Bool) }

instance Semigroup Status where
    a@(Status (False,x)) <> _ = a
    (Status (x1,y1)) <> (Status (x2,y2)) = Status (x1 && x2, y1 || y2)
instance Monoid Status where
    mempty = Status (True,False)

collapse :: Status -> Bool
collapse = uncurry (&&) . unstatus

check cs = collapse $ fold statuses
    where
    statuses = zipWith f cs (tail cs)
    f x y = Status (y >= x, y == x)


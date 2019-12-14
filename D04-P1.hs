type Status = (Bool, Bool)

collapse :: Status -> Bool
collapse = uncurry (&&)

combine :: Status -> Status -> Status
combine a@(False,x) _ = a
combine (x1,y1) (x2,y2) = (x1 && x2, y1 || y2)

def :: Status
def = (True,False)

check cs = collapse $ foldl combine def statuses
    where
    statuses = zipWith f cs (tail cs)
    f x y = (y >= x, y == x)

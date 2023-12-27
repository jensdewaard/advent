module Common.Interval (Interval (..), Comparison (..), Common.Interval.compare, Common.Interval.length, distinct, singleton, fromList, fromPair, overlaps, union, includes, ub, lb, borders) where

data Interval = Empty | Interval Int Int deriving (Show, Eq, Ord)

data Comparison = LT -- ^ The first interval is completely less than the second.
    | LTE  -- ^ The first interval overlaps with the beginning of the second.
    | GT  -- ^ The first interval is completely greater than the second.
    | GTE  -- ^ The first interval overlaps with the end of the second.
    | IN   -- ^ The first interval is a subset of the second.
    | OUT  -- ^ The first interval is a superset of the second.

compare :: Interval -> Interval -> Comparison
compare i j
    | i `overlaps` j = LTE
    | j `overlaps` i = GTE
    | j `includes` i = IN
    | i `includes` j = OUT
    | ub i < lb j = Common.Interval.LT
    | ub j < lb i = Common.Interval.GT
    | otherwise = error ("unable to compare intervals " ++ show i ++ " " ++ show j)

singleton :: Int -> Interval
singleton i = Interval i i

fromPair :: (Int, Int) -> Interval
fromPair (x, y) = Interval (min x y) (max x y)

fromList :: [Int] -> Interval
fromList is = Interval (minimum is) (maximum is)

length :: Interval -> Int
length Empty = 0
length (Interval l u) = u - l + 1

lb :: Interval -> Int
lb Empty = error "lower bound of empty interval"
lb (Interval x _) = x

ub :: Interval -> Int
ub Empty = error "upper bound of empty interval"
ub (Interval _ y) = y

distinct :: Interval -> Interval -> Bool
distinct i j = not (overlaps i j || overlaps j i)

overlaps :: Interval -> Interval -> Bool
overlaps Empty _ = False
overlaps _ Empty = False
overlaps (Interval lbp ubp) (Interval lbq ubq) = lbp <= lbq && lbq <= ubp && ubp <= ubq

union :: Interval -> Interval -> Interval
union p Empty = p
union Empty q = q
union (Interval lbp ubp) (Interval lbq ubq) = fromPair (min lbp lbq, max ubp ubq)

includes :: Interval -> Interval -> Bool
includes _ Empty = False
includes Empty _ = False
includes (Interval lbp ubp) (Interval lbq ubq) = lbp <= lbq && ubq <= ubp

borders :: Interval -> Interval -> Bool
borders Empty _ = False
borders _ Empty = False
borders (Interval _ ubp) (Interval lbq _) = lbq - ubp == 1
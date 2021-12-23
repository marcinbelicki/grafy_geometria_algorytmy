import Data.List

data Point = P Double Double
data Horizontal = H Point Double
data Vertical = V Point Double
data Section = S Double Double
data Mark a = Not a | Yes a

value (Not a) = a
value (Yes a) = a

instance Show a => Show (Mark a) where
   show (Not y) = "Not " ++ show y
   show (Yes y) = "Yes " ++ show y

instance  Show Point where
   show (P x y) = "P " ++ show x ++ " " ++ show y



sortVertical (V (P a _) _)  (V (P b _) _) | a < b  = LT
                                          | a > b  = GT
                                          | otherwise = EQ

sortJust :: Ord a => Mark a -> Mark a -> Ordering
sortJust m1 m2 | a < b  = LT
               | a > b  = GT
               | otherwise = EQ
        where
    a = value m1
    b = value m2

yes (Yes _)  = True
yes _  = False

isInside a b d = yes $ sortBy sortJust [Not a, Yes d, Not b] !! 1


intersection (V (P xv yv) zv) (H (P xh yh) zh) | v && h = [[P xv yh,P yh zh]]
                                            | otherwise = []
                                                where
    v = isInside xv zv yh
    h = isInside xh zh xh


dc test end divide join p | test p = end p
                          | otherwise  =  join  $ map (dc test end divide join) $ divide p


halve :: [a] -> [[a]]
halve a = halve_help a [] 0 where
    half = div (length a) 2
    halve_help [] [] _ = [[],[]]
    halve_help first@(f:ft) s acc | acc < half = halve_help ft (f:s) $ acc + 1
                                  | otherwise  = [reverse s,first]


intersections :: Foldable t => t Horizontal -> Vertical -> ([Point], [[Point]], [Double])
intersections  h v@(V(P x _) _) = ([],concatMap (intersection v) h,[x])


divide a f = divide_help a [] [] where
    divide_help [] a b = (a,b)
    divide_help (punkt@[p,P x1 x2]:pt) a b | f x1 || f x2 =  divide_help pt (p:a) b
                                           | otherwise = divide_help pt a (punkt:b)


joinall a1 b1 a2 b2 x1 x2 = (a1++a2++a3++a4,b3++b4,[x1,x2]) where
    one = divide b1 $ isInside x1 x2
    two = divide b2 $ isInside x1 x2
    a3 = fst one
    b3 = snd one
    a4 = fst two
    b4 = snd two




first (a,_,_) = a
allintersections horizontals verticals = first $ dc test end divide join $ sortBy sortVertical verticals where
        test [_] = True
        test []  = True
        test _ = False
        end [v] = intersections horizontals v
        divide = halve
        join [(a1,b1,a),(a2,b2,b)] = joinall a1 b1 a2 b2 (head a) $ last b


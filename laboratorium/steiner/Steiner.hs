
module Steiner where
import Data.List


data Branch = P Double Double | Node Double Double Branch Branch


f before (P x y) = before ++ "P " ++ show x ++ " " ++ show y
f before (Node x y p q) = before ++ "Node " ++ show x ++ " " ++ show y ++ " {\n" ++ f (before ++ "  ") p ++  "\n" ++ f (before ++ "  ") q ++ "\n" ++ before ++ "}"

instance  Show Branch where
   show = f ""

value (P x y) = x + y
value (Node x y _ _) = x + y

x (P a _) = a
x (Node a _ _ _) = a

y (P _ b) = b
y (Node  _ b _ _) = b


order p q | vq < vp || (vq == vp && x p < x q) = LT
          | otherwise = GT where
              vq = value q
              vp = value p

join p q = Node (min (x p) (x q)) (min (y p) (y q)) p q

cost (P _ _) = 0
cost (Node _ _ p q) = sum [abs (x p - x q), abs (y p - y q), cost p, cost q]

orderedPair (Node _ _ p q)= order p q == LT

minimal q = minimumBy order $ concatMap (\x -> filter orderedPair $ map (join x) q) q



isNotSame (Node _ _ p q) r = compare p r && compare q r where
    compare a b = x a /= x b || y a /= y b



approxRSA zbior = helper (P 0 0:zbior) where
   helper [branch] = branch
   helper zbior = helper $ (:) minimalBranch $ filter (isNotSame minimalBranch) zbior where -- O(n * n ^ 2) = O(n ^ 3)
      minimalBranch = minimal zbior -- O(n ^ 2) - na początku posortować 


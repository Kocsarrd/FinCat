module FinCat where 

import Prelude hiding (id, product)
import Control.Monad (guard)

import Data.Set (Set)
import qualified Data.Set as Set

-- the data of a finite category
data Category ob arr = FinCat {
  objects :: [ob],
  arrows :: [arr],
  dom :: arr -> ob,
  cod :: arr -> ob,
  id :: ob -> arr,
  comp :: arr -> arr -> arr
}

hom :: Eq ob => Category ob arr -> ob -> ob -> [arr]
hom cC a b = [ f | f <- arrows cC, dom cC f == a, cod cC f == b ]

-- Validate if everything is well-defined and the category axioms are satisfied 
validate :: (Eq ob, Eq arr) => Category ob arr -> Bool
validate cC = and (unitl ++ unitr ++ assoc) 
  where 
    allarrows = (arrows cC) ++ [ id cC c | c <- objects cC ]
    unitl = [ (comp cC (id cC (cod cC f)) f) == f | f <- allarrows ]   
    unitr = [ (comp cC f (id cC (dom cC f))) == f | f <- allarrows ]  
    assoc = do
        f <- allarrows
        g <- allarrows
        h <- allarrows
        guard $ (cod cC h) == (dom cC g)
        guard $ (cod cC g) == (dom cC f)
        let (comp1,comp2) = (comp cC f (comp cC g h), comp cC (comp cC f g) h) 
        return $ (comp1 == comp2) && (dom cC comp1 == dom cC h) && (cod cC comp1 == cod cC f) 

-- product category
product :: Category ob1 arr1 -> Category ob2 arr2 -> Category (ob1,ob2) (arr1,arr2)
product cC cD = FinCat _objects _arrows _dom _cod _id _comp
  where
    _objects = [ (c,d) | c <- objects cC, d <- objects cD ]
    _arrows = [ (f,g) | f <- arrows cC, g <- arrows cD ]
    _dom (f,g) = (dom cC f, dom cD g)
    _cod (f,g) = (cod cC f, cod cD g)
    _id (c,d) = (id cC c, id cD d)
    _comp (f1,f2) (g1,g2) = (comp cC f1 g1, comp cD f2 g2) 

-- category of cospans
cospans :: (Eq ob, Eq arr) => Category ob arr -> ob -> ob -> Category (arr,arr) ((arr,arr),(arr,arr),arr) 
cospans cC c1 c2 = FinCat _objects _arrows _dom _cod _id _comp
  where
    _objects = [ (l,r) | l <- arrows cC, r <- arrows cC, dom cC l == c1, dom cC r == c2, cod cC l == cod cC r ]
    _arrows = do
        (l1,r1) <- _objects
        (l2,r2) <- _objects
        f <- hom cC (cod cC l1) (cod cC l2)
        guard $  ((comp cC f l1) == l2) && ((comp cC f r1) == r2) 
        return ((l1,r1),(l2,r2),f)
    _dom (s,_,_) = s
    _cod (_,t,_) = t
    _id p@(l,r) = (p,p,id cC (cod cC l))
    _comp (t2,u,f) (s,t,g) = (s,u,comp cC f g)

{- Connectedness -}

path :: (Eq ob, Ord ob) => Category ob arr -> Set ob -> ob -> Bool
path cC component target 
  | Set.member target component = True
  | otherwise = 
    let outgoing = [ cod cC f | f <- arrows cC, Set.member (dom cC f) component ] in
    let incoming = [ dom cC f | f <- arrows cC, Set.member (cod cC f) component ] in
    let frontier = Set.difference (Set.fromList (outgoing ++ incoming)) component in
    if Set.null frontier 
      then False
      else path cC (Set.union component frontier) target

connected :: (Eq ob, Ord ob) => Category ob arr -> Bool
connected cC = (length (objects cC)) > 0 && (and pairs_connected)
  where
    pairs_connected = [ path cC (Set.fromList [a]) b | a <- objects cC, b <- objects cC ]

{- Siftedness -}

sifted :: (Eq ob, Ord ob, Eq arr, Ord arr) => Category ob arr -> Bool
sifted cC = (length (objects cC)) > 0 && (and cospans_connected)
  where
    cospans_connected = [ connected (cospans cC a b) | a <- objects cC, b <- objects cC ]
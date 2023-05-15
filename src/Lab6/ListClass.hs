module ListClass where

import BoolClass
import qualified Data.List as List (foldr)
import MaybeClass
import MyPrelude
import NatClass
import PairClass
import Prelude (Show (..), (<>))

-- | The class of list-like types (types having a notion of
-- 'nil', 'cons'tructor, and aggregation --- 'foldr').
-- Instances should satisfy the following:
--
-- [Fold nil] @'foldr' f i 'nil' = i@
-- [Fold cons]  @'foldr' f i ('cons' x l) = f x ('foldr' f i l)@
data ListClass l a = ListClass {nil :: l a, cons :: a -> l a -> l a, foldr :: forall b. (a -> b -> b) -> b -> l a -> b}

list :: ListClass [] a
list =
  ListClass
    { nil = [],
      cons = (:),
      foldr = List.foldr
    }

-- | Append two lists
(++) :: ListClass l a -> l a -> l a -> l a
(++) l = flip (foldr l (cons l))

-- >>> (++) list (cons list 2 (cons list 3 (nil list))) (cons list 1 (cons list 4 (cons list 0 (nil list))))
-- [2,3,1,4,0]

-- | Returns the length of a list as a 'NatClass'
length :: ListClass l a -> l a -> CNat
length l = foldr l (const succ) zero

-- >>> length list (cons list 1 (cons list 4 (cons list 0 (nil list))))
-- C3

-- | Test whether the list is empty.
isNull :: ListClass l a -> l a -> CBool
isNull l = foldr l (const (const false)) true

-- >>> isNull list (cons list 1 (cons list 4 (cons list 0 (nil list))))
-- CFalse

-- | 'map' @f@ @xs@ is the list obtained by applying @f@ to each element of @xs@
map :: ListClass la a -> ListClass lb b -> (a -> b) -> la a -> lb b
map la lb f = foldr la (cons lb . f) (nil lb)

-- >>> map list list (mul 2) (cons list 1 (cons list 4 (cons list (0 :: CNat) (nil list))))
-- [C2,C8,C0]

-- | Applied to a predicate and a list, returns the list of those elements that satisfy the predicate
filter :: ListClass l a -> (a -> CBool) -> l a -> l a
filter la p = foldr la (\a l -> bool l (cons la a l) (p a)) (nil la)

-- >>> filter list (not . isZero :: CNat -> CBool) (cons list 1 (cons list 4 (cons list 0 (nil list))))
-- [C1,C4]

-- | Left-associative fold of a list.
-- @foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn@
foldl :: ListClass l a -> (b -> a -> b) -> b -> l a -> b
foldl l agg def xs = foldr l (\a f acc -> f (agg acc a)) id xs def

-- >>> foldl list exp 2 (cons list 1 (cons list 4 (cons list (3 :: CNat) (nil list))))
-- C4096

-- | Decompose a list into its head and tail ('nothing' for the empty list).
uncons :: ListClass l a -> l a -> CMaybe (CPair a (l a))
uncons l = foldr l (\a -> just . pair a . maybe (nil l) (uncurry (cons l))) nothing

-- >>> uncons list (cons list 1 (cons list 4 (cons list (0 :: CNat) (nil list))))
-- CJust <(C1,[C4,C0])>

-- | Extract the first element of a list into a 'MaybeClass' ('nothing' for the empty list)
head :: ListClass l a -> l a -> CMaybe a
head l = maybeFMap fst . uncons l

-- >>> head list (cons list 3 (cons list 4 (cons list (0 :: CNat) (nil list))))
-- CJust C3

-- | Extract the elements after the head of a list into a 'MaybeClass' ('nothing' for the empty list)
tail :: ListClass l a -> l a -> CMaybe (l a)
tail l = maybeFMap snd . uncons l

-- >>> tail list (cons list 3 (cons list 4 (cons list (0 :: CNat) (nil list))))
-- CJust [C4,C0]

-- | returns the elements of a list in reverse order.
reverse :: ListClass l a -> l a -> l a
reverse l = foldl l (flip (cons l)) (nil l)

-- >>> reverse list (cons list 3 (cons list 4 (cons list (0 :: CNat) (nil list))))
-- [C0,C4,C3]

-- | The 'sum' function computes the sum of a list of numbers.
sum :: ListClass l CNat -> l CNat -> CNat
sum l = foldr l add zero

-- >>> sum list (cons list 1 (cons list 4 (cons list (0 :: CNat) (nil list))))
-- C5

-- | The 'product' function computes the product of a list of numbers.
product :: ListClass l CNat -> l CNat -> CNat
product l = foldr l mul one

-- >>> product list (cons list 1 (cons list 4 (cons list (0 :: CNat) (nil list))))
-- C0

-- | 'maximum' returns the maximum value from a list of naturals ('zero' for the empty list),
maximum :: ListClass l CNat -> l CNat -> CNat
maximum l = foldr l max zero

-- >>> maximum list (cons list 1 (cons list 4 (cons list (0 :: CNat) (nil list))))
-- C4

-- | @'natlist' n@ generates a list containing the numbers from @1@ to @n@, in reverse order.
natToList :: ListClass l CNat -> CNat -> l CNat
natToList lnat = iter (\l -> maybe (cons lnat one (nil lnat)) (\p -> cons lnat (succ (fst p)) l) (uncons lnat l)) (nil lnat)

-- >>> natToList list (10 :: CNat)
-- [C10,C9,C8,C7,C6,C5,C4,C3,C2,C1]

newtype CList a = CList {getCList :: forall b. (a -> b -> b) -> b -> b}

clist :: ListClass CList a
clist =
  ListClass
    { foldr = \f i l -> getCList l f i,
      nil = CList (\f i -> i),
      cons = \x l -> CList (\f i -> f x (getCList l f i))
    }

-- | converting between different instances of 'ListClass'
fromListClass :: ListClass l1 a -> ListClass l2 a -> l1 a -> l2 a
fromListClass l1 l2 = map l1 l2 id

-- | 'Show' instance for 'CList' (via transformation into Haskell lists)
instance Show a => Show (CList a) where
  show cl = "{" <> show (fromListClass clist list cl) <> "}"

-- | computes the factorial of a number
factorial :: ListClass l CNat -> CNat -> CNat
factorial l = product l . natToList l

-- >>> factorial clist (5 :: CNat)
-- C120

-- >>> factorial list (5 :: CNat)
-- C120

-- >>> (++) clist (cons clist 2 (cons clist 3 (nil clist))) (cons clist 1 (cons clist 4 (cons clist 0 (nil clist))))
-- {[2,3,1,4,0]}

-- >>> length clist (cons clist 1 (cons clist 4 (cons clist 0 (nil clist))))
-- C3

-- >>> isNull clist (cons clist 1 (cons clist 4 (cons clist 0 (nil clist))))
-- CFalse

-- >>> map list clist (mul 2) [1, 4, 0 :: CNat]
-- {[C2,C8,C0]}

-- >>> filter clist (not . isZero :: CNat -> CBool) (cons clist 1 (cons clist 4 (cons clist 0 (nil clist))))
-- {[C1,C4]}

-- >>> foldl clist exp 2 (cons clist 1 (cons clist 4 (cons clist (3 :: CNat) (nil clist))))
-- C4096

-- >>> uncons clist (cons clist 1 (cons clist 4 (cons clist (0 :: CNat) (nil clist))))
-- CJust <(C1,{[C4,C0]})>

-- >>> head clist (cons clist 3 (cons clist 4 (cons clist (0 :: CNat) (nil clist))))
-- CJust C3

-- >>> tail clist (cons clist 3 (cons clist 4 (cons clist (0 :: CNat) (nil clist))))
-- CJust {[C4,C0]}

-- >>> reverse clist (cons clist 3 (cons clist 4 (cons clist (0 :: CNat) (nil clist))))
-- {[C0,C4,C3]}

-- >>> sum clist (cons clist 1 (cons clist 4 (cons clist (0 :: CNat) (nil clist))))
-- C5

-- >>> product clist (cons clist 1 (cons clist 4 (cons clist (0 :: CNat) (nil clist))))
-- C0

-- >>> maximum clist (cons clist 1 (cons clist 4 (cons clist (0 :: CNat) (nil clist))))
-- C4

-- >>> natToList clist (10 :: CNat)
-- {[C10,C9,C8,C7,C6,C5,C4,C3,C2,C1]}
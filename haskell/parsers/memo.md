import Data.Foldable

data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a) deriving (Show)

ghci> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b

ghci> :i Functor
type Functor :: (* -> *) -> Constraint
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
        -- Defined in ‘GHC.Base’
instance [safe] Functor Parser



instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

ghci> fmap (* 2) $ Branch (Branch (Leaf 5) (Leaf 4)) Empty
Branch (Branch (Leaf 10) (Leaf 8)) Empty


ghci> :t foldMap
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m


ghci> :i Foldable
type Foldable :: (* -> *) -> Constraint
class Foldable t where
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap' :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
  {-# MINIMAL foldMap | foldr #-}
        -- Defined in ‘Data.Foldable’
instance Foldable [] -- Defined in ‘Data

instance Foldable Tree where
   foldMap f Empty = mempty
   foldMap f (Leaf x) = f x
   foldMap f (Branch l r) = foldMap f l `mappend` foldMap f r

treeData :: Num a => Tree a -> a
treeData Empty = 0
treeData (Leaf x) = x
treeData (Branch l r) = (treeData l) + (treeData r)

instance Num a => Semigroup (Tree a) where
    (<>) x y = Leaf $ (treeData x) + (treeData y)

instance Num a => Monoid (Tree a) where
    mempty = Empty

foldMap (\a -> Leaf (2 * a)) $ Branch (Branch (Leaf 5) (Leaf 4)) Empty
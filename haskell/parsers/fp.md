# Rustで関数型プログラミング

Haskellでtree型のデータ構造のfmap, foldMapを実装し、Rustでも同様のことをやってみたいと思います。   

## fmap
### Haskell
まずHaskellでfmapの定義を確認すると、以下のようになっています。

```
ghci> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
```
`Functor f`からファンクターである必要があり、ファンクターの中身を移す関数とファンクターを受け取ると、ファンクターの中身が変更されるといった内容になっています。

Functorの型クラス定義を確認すると以下のようになっており、データ型をファンクターにしたい場合fmapを実装すれば良いことがわかります。
```
ghci> :i Functor
type Functor :: (* -> *) -> Constraint
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
        -- Defined in ‘GHC.Base’
```

以下のTreeをファンクターにする場合、
```
data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a) deriving (Show)
```

以下のようにfmapを実装します。
```
instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)
```

これでfmapの確認が行えます。
```
ghci> fmap (* 2) $ Branch (Branch (Leaf 5) (Leaf 4)) Empty
Branch (Branch (Leaf 10) (Leaf 8)) Empty
```

### Rust
RustでまずTreeを定義すると以下のようになります。
```
#[derive(Clone, Debug)]
enum Tree<A> {
    Empty,
    Leaf(A),
    Branch {
        left: Box<Tree<A>>,
        right: Box<Tree<A>>,
    },
}
```

それからFunctorですがRustは高カインド型に対応していないので、代わりに関連型を使ってfmapの変換前後の型を表現します。関連型は以下のように定義し
```
trait HKT<U> {
    type Current;
    type Target;
}
```
それから、Treeの変換前の中身と変換後を以下のように表現します。 `Target = Tree<B>;`ではなく`Target = B;`で表現できればよかったのですが、fmapの実装が書けない気がするのでこうしています。
```
impl<A, B> HKT<B> for Tree<A> {
    type Current = A;
    type Target = Tree<B>;
}

```
次に関連型を使ってfmapを定義し
```
trait Functor<A,B>: HKT<B> {
    fn fmap<F>(self, f: F) -> <Self as HKT<B>>::Target
    where
        F:  Copy + FnOnce(A) -> B;
}
```
Treeでの実装を書きます
```
impl<A,B> Functor<A,B> for Tree<A> {
    fn fmap<F>(self, f: F) -> <Self as HKT<B>>::Target 
    where
        F: Copy + FnOnce(A) -> B
    {
        match self {
            Tree::Empty => Tree::Empty::<B>,
            Tree::Leaf(a) => Tree::Leaf(f(a)),
            Tree::Branch { left, right } => Tree::Branch { left: Box::new(left.fmap(f)), right: Box::new(right.fmap(f)) }
        }
    }
}
```
これでTreeをfmapで変換できるようになります。
```
let tree = Tree::Branch {
    left: Box::new(Tree::Branch {
        left: Box::new(Tree::Leaf(5)),
        right: Box::new(Tree::Leaf(4)),
    }),
    right: Box::new(Tree::Empty)
};
        
let tree2 = tree.fmap(|i| 2 * i);
println!("{:?}", tree2);

-> Branch { left: Branch { left: Leaf(10), right: Leaf(8) }, right: Empty }
```

## foldMap
### Haskell

次にfoldMapの定義を確認すると以下のようになっています。
```
ghci> :t foldMap
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
```
今回はfoldMapでTree型のLeafのデータを2倍にした上で、各Leafのデータを足し合わせるといったものにしたいと思います。そのため、TreeをFoldableかつMonoidにしていきます。

まずFoldableですが、以下のように長いですがMINIMALではfoldMapまたはfoldrを実装すれば良いことがわかります。
```
ghci> :i Foldable
type Foldable :: (* -> *) -> Constraint
class Foldable t where
  Data.Foldable.fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  Data.Foldable.foldMap' :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  Data.Foldable.foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  Data.Foldable.foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  Data.Foldable.toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
  {-# MINIMAL foldMap | foldr #-}
        -- Defined in ‘Data.Foldable’
```

TreeにfoldMapを実装し、Foldableにします。
```
instance Foldable Tree where
   foldMap f Empty = mempty
   foldMap f (Leaf x) = f x
   foldMap f (Branch l r) = foldMap f l `mappend` foldMap f r
```
ただBranchのfoldMapでmappendを呼び出していますが、mappend未定義なので現時点ではfoldMapは呼び出すことができません。
mappendを呼び出す場合は、Monoidのインスタンスである必要があります。
```
ghci> :t mappend
mappend :: Monoid a => a -> a -> a
```

Monoidの型クラス定義を見てみるとSemigroupかつmemptyの実装が必要なことがわかり
```
ghci> :i Monoid
type Monoid :: * -> Constraint
class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  {-# MINIMAL mempty #-}
        -- Defined in ‘GHC.Base’
```
Semigroupでは(<>)を実装すれば良いことがわかります。mappendはデフォルトでSemigroupの(<>)が使われます。
```
ghci> :i Semigroup
type Semigroup :: * -> Constraint
class Semigroup a where
  (<>) :: a -> a -> a
  GHC.Base.sconcat :: GHC.Base.NonEmpty a -> a
  GHC.Base.stimes :: Integral b => b -> a -> a
  {-# MINIMAL (<>) #-}
        -- Defined in ‘GHC.Base’
```

TreeをSemigroupにして各Leafの集計を行う場合、以下のようになります。+演算子で集計するのでTreeの中身がNumである場合で限定しています。
```
treeData :: Num a => Tree a -> a
treeData Empty = 0
treeData (Leaf x) = x
treeData (Branch l r) = (treeData l) + (treeData r)

instance Num a => Semigroup (Tree a) where
    (<>) x y = Leaf $ (treeData x) + (treeData y)
```
それからmemptyを実装してMonoidにする場合は以下のようになります。
```
instance Num a => Monoid (Tree a) where
    mempty = Empty
```

これでfoldMapにより集計が行えていることがわかります。
```
ghci> foldMap (\a -> Leaf (2 * a)) $ Branch (Branch (Leaf 5) (Leaf 4)) Empty
Leaf 18
```

### Rust
それではRustの方でも同様のfoldMapを実装したいと思います。
まずSemigroupを実装します。
```
impl Tree<i32> {
    fn data(self) -> i32 {
        match self {
            Tree::Empty => 0,
            Tree::Leaf(a) => a,
            Tree::Branch { left, right } => left.data() + right.data()
        }
    }
}

trait Semigroup {
    fn mappend(self, other: Self) -> Self;
}


impl Semigroup for Tree<i32> {
    fn mappend(self, other: Self) -> Self {
        Tree::Leaf(self.data() + other.data())
    }
}
```
想定通りの集計が行われていることが確認できます。
```
let tree = Tree::Branch {
    left: Box::new(Tree::Branch {
        left: Box::new(Tree::Leaf(5)),
        right: Box::new(Tree::Leaf(4)),
    }),
    right: Box::new(Tree::Empty)
};
println!("{:?}", tree.mappend(Tree::Empty));
-> Leaf(9)
```
次にMonoidですがmappendはSemigroupで実装済みなのでmemptyを追加するだけで十分です。
```
trait Monoid: Semigroup {
    fn mempty(self) -> Self;
}

impl Monoid for Tree<i32> {
    fn mempty(self) -> Self {
        Tree::Empty
    }
}
```

それからFoldableのfoldMapを定義します。
```
trait Foldable<A,B>: HKT<B> + Monoid {
    fn fold_map<F>(self, f:F) -> <Self as HKT<B>>::Target
    where
        F: Copy + FnOnce(A) ->  <Self as HKT<B>>::Target;
}


impl Foldable<i32,i32> for Tree<i32> {
    fn fold_map<F>(self, f:F) -> Tree<i32>
    where
        F: Copy + FnOnce(i32) ->  Tree<i32> 
        {
            match self {
                Tree::Empty => self.mempty(),
                Tree::Leaf(a) => f(a),
                Tree::Branch { left, right } => left.fold_map(f).mappend(right.fold_map(f))
            }
        }
}
```

これでRust版でもFoldMapが実装できましたが、Treeの中身はi32で固定しているので数値型用のトレイトを用意してそちらを使ったほうが良さそうです。
結果は以下のようにHaskellと同様の集計が行えることが確認できます。
```
let tree = Tree::Branch {
    left: Box::new(Tree::Branch {
        left: Box::new(Tree::Leaf(5)),
        right: Box::new(Tree::Leaf(4)),
    }),
    right: Box::new(Tree::Empty)
};
println!("{:?}", tree.fold_map(|i| Tree::Leaf(2 * i)));
-> Leaf(18)
```
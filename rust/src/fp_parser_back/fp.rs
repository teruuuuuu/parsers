
use std::fmt::*;
// use num::*;

#[derive(Clone, Debug)]
enum Tree<A> {
    Empty,
    Leaf(A),
    Branch {
        left: Box<Tree<A>>,
        right: Box<Tree<A>>,
    },
}

#[test]
fn test_tree() {
    let tree = Tree::Branch {
            left: Box::new(Tree::Branch {
                left: Box::new(Tree::Leaf(5)),
                right: Box::new(Tree::Leaf(4)),
            }),
            right: Box::new(Tree::Empty)
        };
    println!("{:?}", tree);
}

trait HKT<U> {
    type Current;
    type Target;
}
impl<A, B> HKT<B> for Tree<A> {
    type Current = A;
    type Target = Tree<B>;
}

trait Functor<A,B>: HKT<B> {
    fn fmap<F>(self, f: F) -> <Self as HKT<B>>::Target
    where
        F:  Copy + FnOnce(A) -> B;
}

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

#[test]
fn test_fmap() {
    let tree = Tree::Branch {
            left: Box::new(Tree::Branch {
                left: Box::new(Tree::Leaf(5)),
                right: Box::new(Tree::Leaf(4)),
            }),
            right: Box::new(Tree::Empty)
        };
        
    let tree2 = tree.fmap(|i| 2 * i);
    println!("{:?}", tree2);
}

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

#[test]
fn test_semigroup() {
    let tree = Tree::Branch {
            left: Box::new(Tree::Branch {
                left: Box::new(Tree::Leaf(5)),
                right: Box::new(Tree::Leaf(4)),
            }),
            right: Box::new(Tree::Empty)
        };
    
    println!("{:?}", tree.mappend(Tree::Empty));
}

trait Monoid: Semigroup {
    fn mempty(self) -> Self;
}

impl Monoid for Tree<i32> {
    fn mempty(self) -> Self {
        Tree::Empty
    }
}

// foldMap :: Monoid m => (a -> m) -> t a -> m

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


#[test]
fn test_fold_map() {
    let tree = Tree::Branch {
            left: Box::new(Tree::Branch {
                left: Box::new(Tree::Leaf(5)),
                right: Box::new(Tree::Leaf(4)),
            }),
            right: Box::new(Tree::Empty)
        };
    
    println!("{:?}", tree.fold_map(|i| Tree::Leaf(2 * i)));
}

use std::rc::Rc;

pub trait Apply {
    type Elm;
    type M<B>;

    fn ap<B, F>(self, fs: Self::M<F>) -> Self::M<B>
    where
        F: Fn(&Self::Elm) -> B;
}

// ---

macro_rules! apply_numeric_impl {
    ($($t:ty)*) => ($(
        impl Apply for $t {
          type Elm = $t;
          type M<U> = U;

          fn ap<B, F>(self, fs: Self::M<F>) -> Self::M<B>
          where
            F: Fn(&Self::Elm) -> B,
            {
                fs(&self)
            }
        }
    )*)
}

apply_numeric_impl! { usize u8 u16 u32 u64 u128 isize i8 i16 i32 i64 i128 f32 f64 }

#[test]
fn test_apply() {
    // let a = vec![1];
    // let fs = vec![|k: i16|{k+1}];
    // let fs = |i: i16| -> i16 { i + 5};
    // let a = 5;
    // println!("{:?}", 5.ap(|b| 2 * b));

    fn fs<'a>(i: &'a i32) -> i32 {
        2 * i
    };
    println!("{:?}", 3.ap(fs));
    
}

#[macro_export]
macro_rules! vec2 {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    };
}

#[test]
fn test_macro() {
    let a = vec2![1,2,3];
    println!("{:?}", a);
}
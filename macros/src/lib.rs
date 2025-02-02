#[macro_export]
macro_rules! dedup {
    ($(($($args:tt)*))* {$cont:ident $($cont_args:tt)*}) => {
        $crate::dedup!(@inner {$cont $($cont_args)*} ($) [] $(($($args)*))*)
    };
    (@inner $cont:tt ($d:tt) [$(($($seen:tt)*))*] ($($first:tt)*) $(($($rest:tt)*))*) => {
        macro_rules! __dedup {
            $(
                (($($seen)*) $d(($d($d rest:tt)*))*) => {
                    __dedup!(@duplicate)
                };
            )*
            (@duplicate) => {
                $crate::dedup!(@inner $cont ($d) [$(($($seen)*))*] $(($($rest)*))*)
            };
            (($d($d first:tt)*) $d(($d($d rest:tt)*))*) => {
                $crate::dedup!(@inner $cont ($d) [$(($($seen)*))* ($d($d first)*)] $d(($d($d rest)*))*)
            };
        }
        __dedup!(($($first)*) $(($($rest)*))*)
    };
    (@inner {$cont:ident $($cont_args:tt)*} ($d:tt) [$(($($seen:tt)*))*]) => {
        $cont!($($cont_args)* $(($($seen)*))*)
    }
}

#[macro_export]
macro_rules! first {
    (($($first:tt)*) $(($($rest:tt)*))* {$cont:ident $($cont_args:tt)*}) => {
        $cont!($($cont_args)* $($first)*)
    };
}

#[macro_export]
macro_rules! count {
    ($(($($args:tt)*))* {$cont:ident $($cont_args:tt)*}) => {
        count!(@inner [0] $(($($args)*))* {$cont $($cont_args)*})
    };
    (@inner [$($acc:tt)*] ($($first:tt)*) $(($($rest:tt)*))* $cont:tt) => {
        $crate::count!(@inner [$($acc)* + 1] $(($($rest)*))* $cont)
    };
    (@inner [$($acc:tt)*] {$cont:ident $($cont_args:tt)*}) => {
        $cont!($($cont_args)* $($acc)*)
    }
}

#[macro_export]
macro_rules! id {
    ($($args:tt)*) => {
        $($args)*
    };
}

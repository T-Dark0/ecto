macro_rules! select {
    ( [$next:expr, $peek:expr, $kind:expr, $fail:expr] $parser:ident, $($(@$try:tt)? try)? |$tok:ident $(-> $map_tok:block)? $(,$expected:ident)?| $($rest:tt)* ) => {
        $crate::scope_tree::select::select! { @try |[[$next, $peek, $kind, $fail, $($map_tok)?], $parser, $tok, $($($try)? try)?, $($expected)?]| $($rest)* }
    };

    ( @try |[$lenses:tt, $parser:ident, $tok:ident, try, $($expected:ident)?]| $($rest:tt)* ) => {
        $crate::scope_tree::select::select! { @expected |[$lenses, $parser, $tok, try, $($expected)?]| $($rest)* }
    };
    ( @try |[$lenses:tt, $parser:ident, $tok:ident, , $($expected:ident)?]| $($rest:tt)* ) => {
        $crate::scope_tree::select::select! { @expected |[$lenses, $parser, $tok, fail, $($expected)?]| $($rest)* }
    };

    ( @expected |[$lenses:tt, $parser:ident, $tok:ident, $try:ident, $expected:ident]| $($rest:tt)* ) => {
        $crate::scope_tree::select::select! { @recurse |[$lenses, $parser, $tok, $try, $expected]| [] [] $($rest)* }
    };
    ( @expected |[$lenses:tt, $parser:ident, $tok:ident, $try:ident, ]| $($rest:tt)* ) => {
        $crate::scope_tree::select::select! { @recurse |[$lenses, $parser, $tok, $try, expected]| [] [] $($rest)* }
    };

    (
        @recurse |$vars:tt| [$($expected:tt)*] [$($init:tt)*]
        $fmode:ident else $(if $fguard:expr)? => $fbody:expr,
        $( $rmode:ident $($($rkind:ident)::*)|* $(if $rguard:expr)? => $rbody:expr),+ $(,)?
    ) => {
        $crate::scope_tree::select::select! { @recurse |$vars|
            [$($expected)*]
            [ $($init)* {$fmode _ $(if $fguard)? => $fbody,} ]
            $( $rmode $($($rkind)::*)|* $(if $rguard)? => $rbody ),+
        }
    };
    (
        @recurse |$vars:tt| [$($expected:tt)*] [$($init:tt)*]
        $fmode:ident $($($kind:ident)::*)|* $(if $fguard:expr)? => $fbody:expr,
        $( $rmode:ident $($($rkind:ident)::*)|* $(if $rguard:expr)? => $rbody:expr),+ $(,)?
    ) => {
        $crate::scope_tree::select::select! { @recurse |$vars|
            [$($expected)* $($($kind)::*,)*]
            [ $($init)* {$fmode $($($kind)::*)|* $(if $fguard)? => $fbody,} ]
            $( $rmode $($($rkind)::*)|* $(if $rguard)? => $rbody ),+
        }
    };
    (
        @recurse |$vars:tt| [$($expected:tt)*] [$($init:tt)*]
        $lmode:ident else => $lbody:expr $(,)?
    ) => {
        $crate::scope_tree::select::select! { @expand |$vars|
            [$($expected)*]
            [$($init)*]
            $lmode _ => $lbody
        }
    };

    (
        @expand |[[$next:expr, $peek:expr, $kind:expr, $fail:expr, $($map_tok:expr)?], $parser:ident, $tok:ident, $try:ident, $expected_name:ident]|
        [$($expected:tt)*]
        [$({ $imode:ident $ikind:pat $(if $iguard:expr)? => $ibody:expr, })*]
        $lmode:ident _ => $lbody:expr
    ) => {{
        #[allow(unused_variables)]
        {
            let $tok = $peek($parser);
            $(let $tok = $map_tok;)?
            let $expected_name = &[$($expected)*];
            match $kind($tok) {
                $(
                    $ikind $(if $iguard)? => {
                        $crate::scope_tree::select::select! { @match $imode: next => {$next($parser)} peek => {} };
                        $ibody
                    }
                )*
                _ => {
                    $crate::scope_tree::select::select! { @match $lmode: next => {$next($parser)} peek => {} };
                    $crate::scope_tree::select::select! { @match $try: try => {} fail => {$fail($parser, $expected_name, $tok)} };
                    $lbody
                }
            }
        }
    }};

    ( @match $id:ident : $($cond:ident => {$($body:tt)*})* ) => {
        macro_rules! __match {
            $( ($cond) => {$($body)*}; )*
        }
        __match!($id)
    }
}
pub(crate) use select;

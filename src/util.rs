pub mod expr_maker;
pub mod fmt_latex;
pub mod math;

#[macro_export]
macro_rules! bsl {
    [$($elt: expr),* $(,)?] => {
        Box::new([
            $($elt,)*
        ])
    };
}

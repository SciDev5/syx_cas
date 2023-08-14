use syxtensor::{
    expr::var::{self, VarValues},
    util::expr_maker::ExprMaker::{ConstInt, Var},
};

fn main() {
    let a = var::Var::new("a");

    let c = (((Var(a) + Var(a) + ConstInt(30)) / (Var(a) - ConstInt(3))).pow(ConstInt(3))
        - ConstInt(45)
        + Var(a)
        + Var(a) * Var(a)
        + Var(a).pow(ConstInt(2))
        + Var(a) * Var(a) * (ConstInt(4) / ConstInt(2)))
    .build();

    let vars = VarValues::from([(a, num_complex::Complex64::from(2.0))]);

    println!("{}", c);
    println!("{}", c.eval(&vars));
}

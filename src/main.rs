use syxtensor::{
    expr::{
        consts::Const,
        var::{self, VarValues},
    },
    util::expr_maker::ExprMaker::{ConstInt, Var},
};

fn main() {
    let a = var::Var::new("a");

    let c = (((Var(a) + Var(a) + ConstInt(30)) / (Var(a) - ConstInt(3))).pow(ConstInt(3))
        - ConstInt(45)
        + Var(a)
        + Var(a) * Var(a) * (ConstInt(4) / ConstInt(2)))
    .build();

    let vars = VarValues::from([(a, Const(2))]);

    println!("{}", c);
    println!("{}", c.eval(&vars));
}

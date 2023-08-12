use syxtensor::expr::{
    consts::{Const, ExConst},
    exponentiation::ExExponentiate,
    product::ExProduct,
    sum::ExSum,
    var::{ExVar, Var, VarValues},
    Expr, ExprScope,
};

fn main() {
    let scope = ExprScope::new();

    let var_a = Var::new("a");

    let a = ExConst::new(&scope, Const(3));
    let b = ExConst::new(&scope, Const(6));
    let a_ = ExVar::new(&scope, var_a);

    let a_plus_b = ExSum::new(
        &scope,
        vec![
            a.exprall(), // 3
            ExExponentiate::new(
                &scope,      // 216
                b.exprall(), // 6
                a.exprall(), // 6
            )
            .exprall(),
            ExProduct::new(
                &scope,
                vec![
                    // 4
                    a_.exprall(), // 2
                    a_.exprall(), // 2
                ],
            )
            .exprall(),
        ],
    ); // 223

    let mut vars = VarValues::new();
    vars.set(var_a, Const(2));

    println!("{:?}", a_plus_b.eval(&vars));
}

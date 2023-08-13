use syxtensor::expr::{
    consts::{Const, ExConst},
    division::ExDivide,
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
        // 223
        &scope,
        vec![
            a.exprall(), // 3
            ExExponentiate::new(
                // 216
                &scope,
                ExDivide::new(
                    // 6
                    &scope,
                    ExProduct::new(
                        // 12
                        &scope,
                        vec![
                            b.exprall(),  // 6
                            a_.exprall(), // 2
                        ],
                    )
                    .exprall(),
                    a_.exprall(), // 2
                )
                .exprall(),
                a.exprall(), // 3
            )
            .exprall(),
            ExProduct::new(
                // 4
                &scope,
                vec![
                    a_.exprall(), // 2
                    a_.exprall(), // 2
                    ExDivide::new(
                        // 1
                        &scope,
                        ExConst::new(&scope, Const(2)).exprall(), // 2
                        a_.exprall(),                             // 2
                    )
                    .exprall(),
                ],
            )
            .exprall(),
        ],
    );

    let mut vars = VarValues::new();
    vars.set(var_a, Const(2));

    println!("{}", a_plus_b.eval(&vars));
    println!("{}", a_plus_b.exprall());
}

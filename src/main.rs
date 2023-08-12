use syxtensor::expr::{var::{VarValues, ExVar, Var}, consts::{Const, ExConst}, sum::ExSum, ExprScope, Expr};

fn main() {
    let scope = ExprScope::new();
    
    let var_a = Var::new("a");

    let a = ExConst::new(&scope, Const(3));
    let b = ExConst::new(&scope, Const(6));
    let a_ = ExVar::new(&scope, var_a);

    let a_plus_b = ExSum::new(&scope, vec![
        a.exprall(),
        b.exprall(),
        a_.exprall(),
        a_.exprall(),

    ]);

    let mut vars = VarValues::new();
    vars.set(var_a, Const(2));

    println!("{:?}", a_plus_b.eval(&vars));
}

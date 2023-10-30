use syx::{
    consts::{I, ONE},
    expr::var,
    util::expr_maker::ExprMaker::{ConstInt, ConstRaw, Var},
};

fn main() {
    let x = var::Var::new("x", false);

    // y = x^(1+1i) + x / (x - 3)
    let y = (Var(x).pow(ConstRaw(ONE + I))
        + Var(x) / (Var(x) - ConstInt(3))
        + ((Var(x) * Var(x).ln()).exp()))
    .build();

    let dy_dx = y.derivative(x);

    println!("\n\ny = {}\n\\\\\ny^\\prime = {}\n\n", y, dy_dx);
}

// fn main() {
//     // let a = var::Var::new("a", false);

//     // let c = (((Var(a) + Var(a) + ConstInt(30)) / (Var(a) - ConstInt(3))).pow(ConstInt(3))
//     //     - ConstInt(45)
//     //     + Var(a)
//     //     + Var(a) * Var(a)
//     //     + Var(a).pow(ConstInt(2))
//     //     + Var(a) * Var(a) * (ConstInt(4) / ConstInt(2)))
//     // .build();

//     // let vars = VarValues::from([(a, num_complex::Complex64::from(2.0))]);

//     // println!("\\frac{{\\partial}}{{\\partial {}}} {}", a.name(), c);
//     // println!("= {}", c.derivative(a));
//     // println!("= {}", c.derivative(a).eval(&vars));

//     let x = var::Var::new("x", false);
//     // let x = (ConstInt(7) * Var(t).pow(ConstInt(2)) + ConstInt(4)*Var(t) + ConstInt(-3))
//     // let x = (ConstInt(4)*Var(t).pow(ConstInt(-2))+ConstInt(6)*Var(t).pow(syxtensor::util::expr_maker::ExprMaker::ConstRaw(syxtensor::consts::Const::Rational(Rational::new(1, 3)))))
//     // .build();
//     let y = ((ConstInt(6)*(ConstInt(1)+Var(x)).pow(ConstInt(-1)))).build();

//     let v = y.derivative(x);
//     // let a = v.derivative(t);

//     println!("{v}");

//     // let vars = VarValues::from([(y, num_complex::Complex64::from(3.0))]);

//     // dbg!(x.eval(&vars));
//     // dbg!(v.eval(&vars));
//     // dbg!(a.eval(&vars));

//     // const N: usize = 32;
//     // let mut d = x.clone();
//     // for i in 0 .. 20 {
//     //     d = d.derivative(t);
//     // }
//     // // let dx_dt = x.derivative(t).derivative(t);

//     // println!("\\frac{{d^{{{}}}}}{{dt^{{{}}}}} {} = {}", N, N, x, d);
// }

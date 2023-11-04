use syx::{
    consts::{I, ONE},
    expr::var,
    solve::equality::Equality,
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

    let y = var::Var::new("y", false);

    let x_ = Var(x);
    let y_ = Var(y);

    let c0 = (x_.clone().squared() + x_.clone() + y_.clone().exp() * ConstInt(5)).build();
    let c1 = (-x_.clone() + ConstInt(20)).build();

    let eq: Equality = Equality::new(c0.clone(), c1.clone());
    println!("{} = {}", c0, c1);
    let x_k = eq.solve_for(x).unwrap();
    let y_k = eq.solve_for(y).unwrap();
    println!(
        "x = \\left[ {} \\right]\n\\\\\ny = \\left[ {} \\right]",
        x_k.iter()
            .map(|v| format!("{}", v))
            .collect::<Vec<_>>()
            .join(", "),
        y_k.iter()
            .map(|v| format!("{}", v))
            .collect::<Vec<_>>()
            .join(", ")
    );

    // println!("\\\\ \n {}", x_k[0].clone().substitute(y, y_k[0].clone()));
    // println!("\\\\ \n {}", x_k[1].clone().substitute(y, y_k[0].clone()));
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

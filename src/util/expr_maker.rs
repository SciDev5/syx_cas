use crate::{
    consts::{Const, HALF},
    expr::{
        consts::ExConst,
        derivative::ExDerivative,
        division::ExDivide,
        exp::ExExp,
        ln::ExLn,
        pow::ExPow,
        product::ExProduct,
        sum::ExSum,
        var::{self, ExVar},
        Expr, ExprAll,
    },
};

use ExprMaker::*;

#[derive(Debug, Clone)]
pub enum ExprMaker {
    Raw(ExprAll),
    Var(var::Var),
    ConstInt(i128),
    ConstRaw(Const),
    Add(Box<ExprMaker>, Box<ExprMaker>),
    Mul(Box<ExprMaker>, Box<ExprMaker>),
    Div(Box<ExprMaker>, Box<ExprMaker>),
    Pow(Box<ExprMaker>, Box<ExprMaker>),
    Exp(Box<ExprMaker>),
    Ln(Box<ExprMaker>),
    Derivative(Box<ExprMaker>, var::Var),
}

impl std::ops::Add for ExprMaker {
    type Output = ExprMaker;
    fn add(self, rhs: Self) -> Self::Output {
        Add(Box::new(self), Box::new(rhs))
    }
}

impl std::ops::Add for &ExprMaker {
    type Output = ExprMaker;
    fn add(self, rhs: Self) -> Self::Output {
        self.clone() + rhs.clone()
    }
}
impl std::ops::Add<ExprMaker> for &ExprMaker {
    type Output = ExprMaker;
    fn add(self, rhs: ExprMaker) -> Self::Output {
        self.clone() + rhs
    }
}
impl std::ops::Add<&ExprMaker> for ExprMaker {
    type Output = ExprMaker;
    fn add(self, rhs: &ExprMaker) -> Self::Output {
        self + rhs.clone()
    }
}

impl std::ops::Sub for ExprMaker {
    type Output = ExprMaker;
    fn sub(self, rhs: Self) -> Self::Output {
        Add(Box::new(self), Box::new(rhs * ConstInt(-1)))
    }
}
impl std::ops::Sub for &ExprMaker {
    type Output = ExprMaker;
    fn sub(self, rhs: Self) -> Self::Output {
        self.clone() - rhs.clone()
    }
}
impl std::ops::Sub<ExprMaker> for &ExprMaker {
    type Output = ExprMaker;
    fn sub(self, rhs: ExprMaker) -> Self::Output {
        self.clone() - rhs
    }
}
impl std::ops::Sub<&ExprMaker> for ExprMaker {
    type Output = ExprMaker;
    fn sub(self, rhs: &ExprMaker) -> Self::Output {
        self - rhs.clone()
    }
}

impl std::ops::Neg for ExprMaker {
    type Output = ExprMaker;
    fn neg(self) -> Self::Output {
        self * ConstInt(-1)
    }
}
impl std::ops::Neg for &ExprMaker {
    type Output = ExprMaker;
    fn neg(self) -> Self::Output {
        -self.clone()
    }
}

impl std::ops::Mul for ExprMaker {
    type Output = ExprMaker;
    fn mul(self, rhs: Self) -> Self::Output {
        Mul(Box::new(self), Box::new(rhs))
    }
}
impl std::ops::Mul for &ExprMaker {
    type Output = ExprMaker;
    fn mul(self, rhs: Self) -> Self::Output {
        self.clone() * rhs.clone()
    }
}
impl std::ops::Mul<ExprMaker> for &ExprMaker {
    type Output = ExprMaker;
    fn mul(self, rhs: ExprMaker) -> Self::Output {
        self.clone() * rhs
    }
}
impl std::ops::Mul<&ExprMaker> for ExprMaker {
    type Output = ExprMaker;
    fn mul(self, rhs: &ExprMaker) -> Self::Output {
        self * rhs.clone()
    }
}
impl std::ops::Mul<ExprMaker> for i128 {
    type Output = ExprMaker;
    fn mul(self, rhs: ExprMaker) -> Self::Output {
        ConstInt(self) * rhs
    }
}
impl std::ops::Mul<&ExprMaker> for i128 {
    type Output = ExprMaker;
    fn mul(self, rhs: &ExprMaker) -> Self::Output {
        ConstInt(self) * rhs.clone()
    }
}

impl std::ops::Div for ExprMaker {
    type Output = ExprMaker;
    fn div(self, rhs: Self) -> Self::Output {
        Div(Box::new(self), Box::new(rhs))
    }
}
impl std::ops::Div for &ExprMaker {
    type Output = ExprMaker;
    fn div(self, rhs: Self) -> Self::Output {
        self.clone() / rhs.clone()
    }
}

impl ExprMaker {
    pub fn build(self) -> ExprAll {
        match self {
            Raw(ex) => ex,
            Add(l, r) => ExSum::new(vec![l.build(), r.build()]).exprall(),
            Mul(l, r) => ExProduct::new(vec![l.build(), r.build()]).exprall(),
            Div(l, r) => ExDivide::new(l.build(), r.build()).exprall(),
            Pow(l, r) => ExPow::new(l.build(), r.build()).exprall(),
            Exp(v) => ExExp::new(v.build()).exprall(),
            Ln(v) => ExLn::new(v.build()).exprall(),
            Derivative(ex, v) => ExDerivative::new(ex.build(), v, 1).exprall(),
            Var(v) => ExVar::new(v).exprall(),
            ConstInt(v) => ExConst::new(Const::Int(v)).exprall(),
            ConstRaw(c) => ExConst::new(c).exprall(),
        }
    }

    pub fn pow(self, other: Self) -> Self {
        Pow(Box::new(self), Box::new(other))
    }
    pub fn squared(self) -> Self {
        Pow(Box::new(self), Box::new(ExprMaker::ConstInt(2)))
    }
    pub fn cubed(self) -> Self {
        Pow(Box::new(self), Box::new(ExprMaker::ConstInt(3)))
    }
    pub fn ref_squared(&self) -> Self {
        self.clone().squared()
    }
    pub fn ref_cubed(&self) -> Self {
        self.clone().cubed()
    }
    pub fn sqrt(self) -> Self {
        Pow(Box::new(self), Box::new(ExprMaker::ConstRaw(HALF)))
    }
    pub fn exp(self) -> Self {
        Exp(Box::new(self))
    }
    pub fn ln(self) -> Self {
        Ln(Box::new(self))
    }

    pub fn partial_deriv(self, var: var::Var) -> ExprMaker {
        Derivative(Box::new(self), var)
    }
}

#[cfg(test)]
mod test {
    use num_complex::ComplexFloat;

    use crate::{expr::var, util::expr_maker::ExprMaker::*};

    #[test]
    fn test_expr_maker() {
        let a = var::Var::new("a", false);
        let b = var::Var::new("b", false);
        let c = (((Var(a) + Var(b) + ConstInt(30)) / (Var(a) - ConstInt(3))).pow(ConstInt(3))
            - ConstInt(45)
            + Var(a)
            + Var(a) * Var(a) * (ConstInt(4) / ConstInt(2)))
        .build();
        // ((a + b + 30) / (a - 3)) ^ 3 - 45 + a + a * a * (4/2) = -2239

        let vars = var::VarValues::from([
            (a, num_complex::Complex64::from(1.0)),
            (b, num_complex::Complex64::from(-5.0)),
        ]);

        assert!(
            (c.eval(&vars) - num_complex::Complex64::new(-2239.0, 0.0))
                .abs()
                .re()
                < 1e-10
        );
    }
}

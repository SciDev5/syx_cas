use crate::expr::{
    consts::{Const, ExConst},
    division::ExDivide,
    exponentiation::ExExponentiate,
    product::ExProduct,
    sum::ExSum,
    var::{self, ExVar},
    Expr, ExprAll,
};

use ExprMaker::*;

#[derive(Debug, Clone)]
pub enum ExprMaker {
    Var(var::Var),
    ConstInt(i32),
    Add(Box<ExprMaker>, Box<ExprMaker>),
    Mul(Box<ExprMaker>, Box<ExprMaker>),
    Div(Box<ExprMaker>, Box<ExprMaker>),
    Pow(Box<ExprMaker>, Box<ExprMaker>),
}

impl std::ops::Add for ExprMaker {
    type Output = ExprMaker;
    fn add(self, rhs: Self) -> Self::Output {
        Add(Box::new(self), Box::new(rhs))
    }
}

impl std::ops::Sub for ExprMaker {
    type Output = ExprMaker;
    fn sub(self, rhs: Self) -> Self::Output {
        Add(Box::new(self), Box::new(rhs * ConstInt(-1)))
    }
}

impl std::ops::Mul for ExprMaker {
    type Output = ExprMaker;
    fn mul(self, rhs: Self) -> Self::Output {
        Mul(Box::new(self), Box::new(rhs))
    }
}

impl std::ops::Div for ExprMaker {
    type Output = ExprMaker;
    fn div(self, rhs: Self) -> Self::Output {
        Div(Box::new(self), Box::new(rhs))
    }
}

impl ExprMaker {
    pub fn build(self) -> ExprAll {
        match self {
            Add(l, r) => ExSum::new(vec![l.build(), r.build()]).exprall(),
            Mul(l, r) => ExProduct::new(vec![l.build(), r.build()]).exprall(),
            Div(l, r) => ExDivide::new(l.build(), r.build()).exprall(),
            Pow(l, r) => ExExponentiate::new(l.build(), r.build()).exprall(),
            Var(v) => ExVar::new(v).exprall(),
            ConstInt(v) => ExConst::new(Const(v)).exprall(),
        }
    }

    pub fn pow(self, other: Self) -> Self {
        Pow(Box::new(self), Box::new(other))
    }
}

#[cfg(test)]
mod test {
    use crate::{
        expr::{consts::Const, var},
        util::expr_maker::ExprMaker::*,
    };

    #[test]
    fn t() {
        let a = var::Var::new("a");
        let b = var::Var::new("b");
        let c = (((Var(a) + Var(b) + ConstInt(30)) / (Var(a) - ConstInt(3))).pow(ConstInt(3))
            - ConstInt(45)
            + Var(a)
            + Var(a) * Var(a) * (ConstInt(4) / ConstInt(2)))
        .build();
        // ((a + b + 30) / (a - 3)) ^ 3 - 45 + a + a * a * (4/2) = -2239

        let vars = var::VarValues::from([(a, Const(1)), (b, Const(-5))]);

        assert_eq!(c.eval(&vars), Const(-2239),);
    }
}

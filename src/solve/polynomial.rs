use std::{collections::HashMap, fmt::Display};

use crate::{
    consts::{Const, NEG_ONE, ONE, ZERO},
    expr::{
        associative_commutative::ExprAssociativeCommuttative,
        consts::ExConst,
        pow::ExPow,
        product::ExProduct,
        sum::ExSum,
        var::{ExVar, Var},
        Expr, ExprAll,
    },
    util::expr_maker::ExprMaker::*,
};

#[derive(Debug, Clone)]
pub struct Polynomial {
    terms: HashMap<i128, ExprAll>,
}
impl Display for Polynomial {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, k) in &self.terms {
            write!(f, "+({})t^{{{}}}", k, i)?;
        }
        Ok(())
    }
}
impl Polynomial {
    pub fn zero() -> Self {
        Self {
            terms: HashMap::new(),
        }
    }
    pub fn one() -> Self {
        Self::new_of_const(ExConst::new(ONE).exprall())
    }
    pub fn new_of_const(c: ExprAll) -> Self {
        Self {
            terms: [(0, c)].into_iter().collect(),
        }
    }
    pub fn new_of_param() -> Self {
        Self {
            terms: [(1, ExConst::new(ONE).exprall())].into_iter().collect(),
        }
    }

    pub fn zeros(mut self) -> Vec<ExprAll> {
        self.clear_zero_coefficients();
        if self.terms.len() == 0 {
            todo!("return INFINITE SOLUTIONS (placeholder values: real)");
        }
        let leading_n = *self.terms.keys().max().unwrap();
        let trailing_n = *self.terms.keys().min().unwrap();

        let order = leading_n - trailing_n;
        let mut solutions = match order {
            0 => vec![], // no solutions:  c = 0, c != 0  ->  never
            1 => {
                // one solution, linear:  bx + c = 0  ->  x = -c/b
                let b = Raw(self.terms.get(&(trailing_n + 1)).unwrap().clone());
                let c = Raw(self.terms.get(&trailing_n).unwrap().clone());
                vec![(-c / b).build()]
            }
            2 => {
                // two solutions, quadratic:  a x^2 + b x + c = 0  ->  x = (-b <+-> sqrt(b^2 - 4 a c))/(2 a)
                let a = Raw(self.terms.get(&(trailing_n + 2)).unwrap().clone());
                let b = Raw(
                    self.terms
                        .get(&(trailing_n + 1))
                        .map(|it| it.clone())
                        .unwrap_or(ExConst::new(ZERO).exprall()),
                );
                let c = Raw(self.terms.get(&trailing_n).unwrap().clone());

                vec![
                    ((-b.clone()
                        + (b.clone().squared() - ConstInt(4) * a.clone() * c.clone())
                            .sqrt())
                        / (ConstInt(2) * a.clone()))
                    .build(),
                    ((-b.clone()
                        - (b.clone().squared() - ConstInt(4) * a.clone() * c.clone())
                            .sqrt())
                        / (ConstInt(2) * a.clone()))
                    .build(),
                ]
            }
            _ => vec![], // TODO
        };

        if trailing_n > 0 {
            solutions.push(ExConst::new(ZERO).exprall())
        }
        if trailing_n < 0 {
            // TODO add condition that x != 0
        }

        solutions
    }

    pub fn leading(&self) -> Option<(i128, ExprAll)> {
        let i_max = self.terms.keys().max();
        if let Some(i_max) = i_max {
            Some((
                *i_max,
                self.terms
                    .get(i_max)
                    .map_or(ExConst::new(ZERO).exprall(), |it| it.clone()),
            ))
        } else {
            None
        }
    }

    pub fn to_monomial(mut self) -> Option<(i128, ExprAll)> {
        self.clear_zero_coefficients();
        if self.terms.len() == 1 {
            Some(self.terms.drain().next().unwrap())
        } else {
            None
        }
    }
    pub fn to_const(self) -> Option<Const> {
        let (pow, expr) = self.to_monomial()?;
        if pow != 0 {
            None
        } else if let ExprAll::Const(c) = expr {
            Some(c.1)
        } else {
            None
        }
    }

    pub fn clear_zero_coefficients(&mut self) {
        for i in self.terms.keys().map(|v| *v).collect::<Vec<_>>() {
            if is_explicitly_zero(self.terms.get(&i).unwrap()) {
                self.terms.remove(&i);
            }
        }
    }

    pub fn exprall(mut self, var: Var) -> ExprAll {
        ExSum::new(
            self.terms
                .drain()
                .into_iter()
                .map(|(pow, expr)| {
                    ExProduct::new(vec![
                        expr,
                        ExPow::new(
                            ExVar::new(var).exprall(),
                            ExConst::new(crate::consts::Const::Int(pow)).exprall(),
                        )
                        .exprall(),
                    ])
                    .exprall()
                })
                .collect(),
        )
        .exprall()
    }

    /**
     * Returns the expr as a polynomial representation, if possible.
     */
    pub fn from_exprall(expr: ExprAll, var: Var) -> Option<Polynomial> {
        // dbg!(&expr);
        match expr {
            ExprAll::Const(c) => Some(Polynomial::new_of_const(c.exprall())),
            ExprAll::Var(v) => Some(if v.var == var {
                Polynomial::new_of_param()
            } else {
                Polynomial::new_of_const(v.exprall())
            }),
            ExprAll::Sum(v) => v
                .children()
                .get_expralls()
                .into_iter()
                .map(|v| Self::from_exprall(v, var))
                .sum(),
            ExprAll::Product(v) => v
                .children()
                .get_expralls()
                .into_iter()
                .map(|v| Self::from_exprall(v, var))
                .product(),
            ExprAll::Derivative(_) => todo!("handle polynomials with residual derivatives in them"),
            ExprAll::Ln(v) => {
                if v.has_explicit_dependence(var) {
                    None
                } else {
                    Some(Self::new_of_const(v.exprall()))
                }
            }
            ExprAll::Exp(v) => {
                if v.has_explicit_dependence(var) {
                    None
                } else {
                    Some(Self::new_of_const(v.exprall()))
                }
            }
            ExprAll::Divide(v) => Self::from_exprall(
                ExPow::new(v.exprall(), ExConst::new(NEG_ONE).exprall()).exprall(),
                var,
            ),
            ExprAll::Pow(v) => {
                let base = Self::from_exprall(v.base().clone(), var)?;
                let exponent = Self::from_exprall(v.exponent().clone(), var)?;

                let base = base.to_monomial()?;
                let exponent = if let Const::Int(n) = exponent.to_const()? {
                    n
                } else {
                    return None;
                };

                let mut terms = HashMap::new();
                terms.insert(
                    exponent * base.0,
                    ExPow::new(base.1, ExConst::new(Const::Int(exponent)).exprall()).exprall(),
                );
                Some(Polynomial { terms })
            }
        }
    }

    // pub fn div(self, rhs: Self) -> (Self, Option<(Self, Self)>) {
    //     let num = self.0
    // }
}
impl std::ops::Add for Polynomial {
    type Output = Polynomial;
    fn add(mut self, rhs: Self) -> Self::Output {
        self += rhs;
        self
    }
}
impl std::ops::AddAssign for Polynomial {
    fn add_assign(&mut self, rhs: Self) {
        for (n, c) in rhs.terms {
            self.terms
                .entry(n)
                .and_modify(|v| *v = ExSum::new(vec![v.clone(), c.clone()]).exprall())
                .or_insert(c);
        }
    }
}
impl std::iter::Sum for Polynomial {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut accumulator = Polynomial::zero();
        for p in iter {
            accumulator += p;
        }
        accumulator
    }
}
impl std::ops::Mul for &Polynomial {
    type Output = Polynomial;
    fn mul(self, rhs: Self) -> Self::Output {
        let mut output: HashMap<_, ExprAll> = HashMap::new();

        for (na, a) in &self.terms {
            for (nb, b) in &rhs.terms {
                let c = ExProduct::new(vec![a.clone(), b.clone()]).exprall();
                output
                    .entry(na + nb)
                    .and_modify(|v| *v = ExSum::new(vec![v.clone(), c.clone()]).exprall())
                    .or_insert(c);
            }
        }

        Polynomial { terms: output }
    }
}
impl std::ops::MulAssign for Polynomial {
    fn mul_assign(&mut self, rhs: Self) {
        *self = (&*self) * (&rhs)
    }
}
impl std::iter::Product for Polynomial {
    fn product<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut accumulator = Polynomial::one();
        for p in iter {
            accumulator *= p;
        }
        accumulator
    }
}
impl std::ops::Mul<ExprAll> for Polynomial {
    type Output = Polynomial;
    fn mul(mut self, rhs: ExprAll) -> Self::Output {
        self *= rhs;
        self
    }
}
impl std::ops::MulAssign<ExprAll> for Polynomial {
    fn mul_assign(&mut self, rhs: ExprAll) {
        for v in self.terms.values_mut() {
            *v = ExProduct::new(vec![v.clone(), rhs.clone()]).exprall();
        }
    }
}

fn is_explicitly_zero(expr: &ExprAll) -> bool {
    match expr {
        ExprAll::Const(c) => c.1.is_zero(),
        ExprAll::Derivative(v) => !v.has_explicit_dependence(v.var),
        ExprAll::Divide(v) => {
            is_explicitly_zero(v.numerator()) && is_explicitly_zero(v.denominator())
        }
        ExprAll::Exp(_v) => false,
        ExprAll::Ln(v) => is_explicitly_one(v.argument()),
        ExprAll::Pow(v) => is_explicitly_zero(v.base()) && !is_explicitly_zero(v.exponent()),
        ExprAll::Product(v) => v.children().get_expralls().iter().all(is_explicitly_zero),
        ExprAll::Sum(v) => v.children().get_expralls().iter().all(is_explicitly_zero),
        ExprAll::Var(_) => false,
    }
}
fn is_explicitly_one(expr: &ExprAll) -> bool {
    match expr {
        ExprAll::Const(c) => c.1.is_one(),
        ExprAll::Derivative(_v) => false,
        ExprAll::Divide(_v) => false, // should automatically cancel
        ExprAll::Exp(v) => is_explicitly_zero(&v.exprall()),
        ExprAll::Ln(_v) => is_explicitly_zero(expr),
        ExprAll::Pow(v) => {
            is_explicitly_zero(v.exponent()) && !is_explicitly_zero(v.base())
                || is_explicitly_one(v.base()) && !is_explicitly_zero(v.exponent())
        }
        ExprAll::Product(_v) => false, // should automatically reduce
        ExprAll::Sum(_v) => false,     // should automatically reduce
        ExprAll::Var(_) => false,
    }
}

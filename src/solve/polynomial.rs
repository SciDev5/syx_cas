use std::{collections::HashMap, fmt::Display};

use crate::{
    consts::{Const, NEG_ONE, ONE, ZERO},
    expr::{
        self,
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

    pub fn zeros(mut self) -> Option<Vec<ExprAll>> {
        self.clear_zero_coefficients();
        if self.terms.len() == 0 {
            todo!("return INFINITE SOLUTIONS (placeholder values: real)");
        }
        let leading_n = *self.terms.keys().max().unwrap();
        let trailing_n = *self.terms.keys().min().unwrap();

        macro_rules! term {
            ($i: expr) => {
                Raw(self
                    .terms
                    .get(&(trailing_n + $i))
                    .map(|it| it.clone())
                    .unwrap_or(ExConst::new(ZERO).exprall()))
            };
        }
        macro_rules! has_term {
            ($i: expr) => {
                self.terms.get(&(trailing_n+$i)).is_some()
            };
        }

        
        let order = leading_n - trailing_n;
        let mut solutions = match order {
            0 => {
                // no solutions dependent on x:  c = 0  ->  never  // TODO distinguish this from failure
                if leading_n > 0 {
                    vec![] // let the default zero roots thing do its thing.
                } else {
                    return None; // else fail out now, the zero roots will be empty
                }
            }
            1 => {
                // one solution, linear:  bx + c = 0  ->  x = -c/b
                let b = term!(1);
                let c = term!(0);
                vec![(-c / b).build()]
            }
            2 => {
                // two solutions, quadratic:  a x^2 + b x + c = 0  ->  x = (-b <+-> sqrt(b^2 - 4 a c))/(2 a)
                let a = term!(2);
                let b = term!(1);
                let c = term!(0);

                vec![
                    ((-&b + (b.ref_squared() - 4 * &a * &c).sqrt()) / (2 * &a)).build(),
                    ((-&b - (b.ref_squared() - 4 * &a * &c).sqrt()) / (2 * &a)).build(),
                ]
            }
            4 => {
                if !has_term!(3) && !has_term!(1) {
                    let a = term!(4);
                    let b = term!(2);
                    let c = term!(0);

                    let r0 = (-&b + (b.ref_squared() - 4 * &a * &c).sqrt()) / (2 * &a);
                    let r1 = (-&b - (b.ref_squared() - 4 * &a * &c).sqrt()) / (2 * &a);

                    [
                        r0.clone().sqrt(),
                        -r0.clone().sqrt(),
                        r1.clone().sqrt(),
                        -r1.clone().sqrt(),
                    ].into_iter().map(|v| v.build()).collect()
                } else {
                    let a = term!(4);
                    let b = term!(3);
                    let c = term!(2);
                    let d = term!(1);
                    let e = term!(0);
    
                    let half = ConstInt(1) / ConstInt(2);
    
                    let delta_0 = Raw((c.ref_squared() - 3 * &b * &d + 12 * &a * &e).build());
                    let delta_1 = Raw((2 * c.ref_cubed() - 9 * &b * &c * &d
                        + 27 * b.ref_squared() * &e
                        + 27 * &a * d.ref_squared()
                        - 72 * &a * &c * &e)
                        .build());
    
                    let p = Raw(((8 * &a * &c - 3 * b.ref_squared()) / (8 * a.ref_squared())).build());
                    let q = Raw(
                        ((b.ref_cubed() - 4 * &a * &b * &c + 8 * a.ref_squared() * &d)
                            / (8 * a.ref_cubed()))
                        .build(),
                    );
    
                    let qq = ((&delta_1 + (delta_1.ref_squared() - 4 * delta_0.ref_cubed()).sqrt())
                        / ConstInt(2))
                    .pow(ConstInt(1) / ConstInt(3));
                    let ss = (-2 * &p / ConstInt(3) + (&qq + &delta_0 / &qq) / (3 * a.clone())).sqrt()
                        / ConstInt(2);
                    vec![
                        (-&b / (4 * &a) - &ss
                            + &half * (-4 * ss.ref_squared() - 2 * &p + &q / &ss).sqrt())
                        .build(),
                        (-&b / (4 * &a)
                            - &ss
                            - &half * (-4 * ss.ref_squared() - 2 * &p + &q / &ss).sqrt())
                        .build(),
                        (-&b / (4 * &a)
                            + &ss
                            + &half * (-4 * ss.ref_squared() - 2 * &p - &q / &ss).sqrt())
                        .build(),
                        (-&b / (4 * &a) + &ss
                            - &half * (-4 * ss.ref_squared() - 2 * &p - &q / &ss).sqrt())
                        .build(),
                    ]
                }
            }
            _ => return None, // TODO
        };

        if trailing_n > 0 {
            solutions.push(ExConst::new(ZERO).exprall())
        }
        if trailing_n < 0 {
            // TODO add condition that x != 0
        }

        Some(solutions)
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
            if expr::is_explicitly_zero(self.terms.get(&i).unwrap()) {
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
            ExprAll::Divide(v) => Self::from_exprall(
                ExProduct::new(vec![
                    v.numerator().clone(),
                    ExPow::new(v.denominator().clone(), ExConst::new(NEG_ONE).exprall()).exprall(),
                ])
                .exprall(),
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
            expr => {
                if expr::has_explicit_dependence(&expr, var) {
                    None
                } else {
                    Some(Self::new_of_const(expr))
                }
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

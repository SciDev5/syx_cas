use std::{collections::hash_map::DefaultHasher, hash::Hasher, rc::Rc};

use crate::consts::{NEG_ONE, ONE, TWO, ZERO};

use super::{
    consts::ExConst, pow::ExPow, product::ExProduct, sum::ExSum, var::VarValues, Expr, ExprAll, Id,
};

#[derive(Debug)]
pub struct ExDivide(Id, ExprAll, ExprAll);
impl ExDivide {
    pub fn new(numerator: ExprAll, denominator: ExprAll) -> Rc<Self> {
        let content_hash = {
            let mut h = DefaultHasher::new();
            h.write_u64(numerator.get_hash());
            h.write_u64(denominator.get_hash());
            h.finish()
        };
        let id = Id { content_hash };
        Rc::new(Self(id, numerator, denominator))
    }
    pub fn numerator(&self) -> &ExprAll {
        &self.1
    }
    pub fn denominator(&self) -> &ExprAll {
        &self.2
    }
}
impl Expr for ExDivide {
    fn eval(&self, vars: &VarValues) -> num_complex::Complex64 {
        let numerator = self.1.eval(vars);
        let denominator = self.2.eval(vars);

        numerator / denominator
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        if let (ExprAll::Const(num), ExprAll::Const(den)) = (self.numerator(), self.denominator()) {
            return ExprAll::Const(ExConst::new(num.1 / den.1));
        } else if self.numerator() == self.denominator() {
            return ExprAll::Const(ExConst::new(ONE));
        } else if let ExprAll::Const(den) = self.denominator() {
            if den.1.is_one() {
                return self.numerator().clone();
            }
        } else if let ExprAll::Const(num) = self.numerator() {
            if num.1.is_zero() {
                return ExprAll::Const(ExConst::new(ZERO));
            }
        }
        ExprAll::Divide(self.clone())
    }
    fn derivative(self: &Rc<Self>, var: super::var::Var) -> ExprAll {
        let u = self.numerator();
        let v = self.denominator();
        let u_ = u.derivative(var);
        let v_ = v.derivative(var);
        ExDivide::new(
            // (u / v)' = (u' v - u v') / (v ^ 2)
            ExSum::new(vec![
                ExProduct::new(vec![u_.clone(), v.clone()]).exprall(),
                ExProduct::new(vec![ExConst::new(NEG_ONE).exprall(), u.clone(), v_.clone()])
                    .exprall(),
            ])
            .exprall(),
            ExPow::new(v.clone(), ExConst::new(TWO).exprall()).exprall(),
        )
        .exprall()
    }
    fn child_exprs(self: &Rc<Self>) -> Vec<ExprAll> {
        vec![self.numerator().clone(), self.denominator().clone()]
    }
    fn transform_children<F: Fn(&ExprAll) -> ExprAll>(self: &Rc<Self>, f: F) -> Rc<Self> {
        ExDivide::new(f(self.numerator()), f(self.denominator()))
    }
    fn id(&self) -> Id {
        self.0
    }
}
impl PartialEq for ExDivide {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

#[cfg(test)]
mod test {
    use crate::expr::{
        division::ExDivide,
        var::{ExVar, Var},
        Expr,
    };

    #[test]
    fn hash_equality() {
        let vars = [
            ExVar::new(Var::new("a", false)),
            ExVar::new(Var::new("b", false)),
        ];

        let divs = [
            ExDivide::new(vars[0].exprall(), vars[1].exprall()),
            ExDivide::new(vars[0].exprall(), vars[0].exprall()),
            ExDivide::new(vars[1].exprall(), vars[0].exprall()),
        ];

        assert_ne!(divs[0], divs[1]);
        assert_ne!(divs[0], divs[2]);
    }
}

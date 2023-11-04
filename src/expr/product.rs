use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::consts::{Const, ONE, ZERO};

use super::{
    associative_commutative::{ChildrenAssociativeCommutative, ExprAssociativeCommuttative},
    consts::ExConst,
    pow::ExPow,
    sum::ExSum,
    var::{Var, VarValues},
    Expr, ExprAll, Id,
};

#[derive(Debug)]
pub struct ExProduct(Id, ChildrenAssociativeCommutative);
impl ExProduct {
    pub fn new(children: Vec<ExprAll>) -> Rc<Self> {
        // TODO: explain this code I have no idea what this does
        let children = ChildrenAssociativeCommutative::new::<Self>(children)
            .combine_like::<Self, ExprAll, _, _, _>(
                |a, b| ExSum::new(vec![a.clone(), b.clone()]).exprall(),
                |ex| match ex {
                    ExprAll::Pow(v) => (v.exponent().clone(), v.base().clone()),
                    ex => (ExConst::new(ONE).exprall(), ex.clone()),
                },
                |(k, ex)| ExPow::new(ex, k).exprall(),
            );
        let content_hash = {
            let mut h = DefaultHasher::new();
            children.hash(&mut h);
            h.finish()
        };
        let id = Id { content_hash };
        Rc::new(Self(id, children))
    }
}
impl Expr for ExProduct {
    fn eval(&self, vars: &VarValues) -> num_complex::Complex64 {
        self.1
            .get_expralls()
            .into_iter()
            .map(|v| v.eval(vars))
            .product()
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        let children = self.1.get_expralls_filtering(Const::is_one);
        if self.1.is_only_consts() {
            // (k) = k
            ExprAll::Const(self.1.get_consts())
        } else if self.1.get_consts().1.is_zero() {
            // k * 0 = 0
            ExprAll::Const(ExConst::new(ZERO))
        } else if children.len() == 1 {
            // x = x
            children[0].clone()
        } else if self.children().get_nonconsts().len() == 1 {
            let consts = self.children().get_consts();
            let child = &self.children().get_nonconsts()[0];
            if let ExprAll::Sum(sum) = child {
                // k(a+b) = ka + kb  // constants only
                ExSum::new(
                    sum.children()
                        .get_expralls()
                        .iter()
                        .map(|ex| ExProduct::new(vec![consts.exprall(), ex.clone()]).exprall())
                        .collect(),
                )
                .exprall()
            } else {
                ExprAll::Product(self.clone())
            }
        } else {
            ExprAll::Product(self.clone())
        }
    }
    fn derivative(self: &Rc<Self>, var: Var) -> ExprAll {
        // extended product rule ({const} a b c)' = {const}a'bc + {const}ab'c + {const}abc'
        let consts = self.children().get_consts();
        let non_consts = self.children().get_nonconsts();

        let mut terms = Vec::with_capacity(non_consts.len());
        for i in 0..non_consts.len() {
            let mut v: Vec<_> = [consts.exprall()]
                .into_iter()
                .chain(non_consts.iter().map(|v| v.clone()))
                .collect();
            v[i + 1] = non_consts[i].derivative(var);

            terms.push(ExProduct::new(v).exprall());
        }
        ExSum::new(terms).exprall()
    }
    fn has_explicit_dependence(self: &Rc<Self>, var: Var) -> bool {
        self.children()
            .get_nonconsts()
            .iter()
            .any(|it| it.has_explicit_dependence(var))
    }
    fn substitute(self: &Rc<Self>, var: Var, expr: ExprAll) -> ExprAll {
        ExProduct::new(
            self.children()
                .get_expralls()
                .iter()
                .map(|it| it.substitute(var, expr.clone()))
                .collect(),
        )
        .exprall()
    }
    fn id(&self) -> Id {
        self.0
    }
}
impl ExprAssociativeCommuttative for ExProduct {
    fn reduce_consts<I: Iterator<Item = Const>>(consts: I) -> Const {
        consts.product()
    }
    fn associates_with(child: ExprAll) -> Result<Vec<ExprAll>, ExprAll> {
        match child {
            ExprAll::Product(v) => Ok(v.1.get_expralls()),
            child => Err(child),
        }
    }
    fn children(&self) -> &ChildrenAssociativeCommutative {
        &self.1
    }
}
impl PartialEq for ExProduct {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

#[cfg(test)]
mod test {
    use crate::{
        consts::{Const, ONE},
        expr::{consts::ExConst, sum::ExSum, ExprAll},
    };

    #[test]
    fn hash_equality() {
        let consts = [
            ExConst::new(ONE),
            ExConst::new(ONE),
            ExConst::new(Const::Int(5)),
        ];

        let sums = [
            ExSum::new(vec![
                ExprAll::Const(consts[0].clone()),
                ExprAll::Const(consts[0].clone()),
            ]),
            ExSum::new(vec![
                ExprAll::Const(consts[0].clone()),
                ExprAll::Const(consts[1].clone()),
            ]),
            ExSum::new(vec![
                ExprAll::Const(consts[2].clone()),
                ExprAll::Const(consts[1].clone()),
            ]),
            ExSum::new(vec![
                ExprAll::Const(consts[0].clone()),
                ExprAll::Const(consts[2].clone()),
            ]),
            ExSum::new(vec![
                ExprAll::Const(consts[0].clone()),
                ExprAll::Const(consts[2].clone()),
                ExprAll::Const(consts[2].clone()),
            ]),
        ];

        assert_eq!(sums[0], sums[1]);
        assert_eq!(sums[2], sums[3]);

        assert_ne!(sums[0], sums[2]);
        assert_ne!(sums[0], sums[3]);
        assert_ne!(sums[1], sums[3]);

        assert_ne!(sums[3], sums[4]);
    }
}

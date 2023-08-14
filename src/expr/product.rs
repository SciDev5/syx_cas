use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::consts::Const;

use super::{
    associative_commutative::{ChildrenAssociativeCommutative, ExprAssociativeCommuttative},
    consts::ExConst,
    exponentiation::ExExponentiate,
    sum::ExSum,
    var::VarValues,
    Expr, ExprAll, Id,
};

#[derive(Debug)]
pub struct ExProduct(Id, ChildrenAssociativeCommutative);
impl ExProduct {
    pub fn new(children: Vec<ExprAll>) -> Rc<Self> {
        let children = ChildrenAssociativeCommutative::new::<Self>(children)
            .combine_like::<Self, ExprAll, _, _, _>(
                |a, b| ExSum::new(vec![a.clone(), b.clone()]).exprall(),
                |ex| match ex {
                    ExprAll::Exponent(v) => (v.exponent().clone(), v.base().clone()),
                    ex => (ExConst::new(Const::Int(1)).exprall(), ex.clone()),
                },
                |(k, ex)| ExExponentiate::new(ex, k).exprall(),
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
            ExprAll::Const(ExConst::new(Const::Int(0)))
        } else if children.len() == 1 {
            // x = x
            children[0].clone()
        } else {
            ExprAll::Product(self.clone())
        }
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
        consts::Const,
        expr::{consts::ExConst, sum::ExSum, ExprAll},
    };

    #[test]
    fn hash_equality() {
        let consts = [
            ExConst::new(Const::Int(1)),
            ExConst::new(Const::Int(1)),
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

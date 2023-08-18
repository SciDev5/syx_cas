use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::consts::Const;

use super::{
    associative_commutative::{ChildrenAssociativeCommutative, ExprAssociativeCommuttative},
    consts::ExConst,
    product::ExProduct,
    var::{Var, VarValues},
    Expr, ExprAll, Id,
};

#[derive(Debug)]
pub struct ExSum(Id, ChildrenAssociativeCommutative);
impl ExSum {
    pub fn new(children: Vec<ExprAll>) -> Rc<Self> {
        let children = ChildrenAssociativeCommutative::new::<Self>(children)
            .combine_like::<Self, Const, _, _, _>(
                |a, b| *a + *b,
                |ex| match ex {
                    ExprAll::Product(v) => (
                        v.children().get_consts().1,
                        ExProduct::new(v.children().get_expralls_filtering(|_| true)).exprall(),
                    ),
                    ex => (Const::Int(1), ex.clone()),
                },
                |(k, ex)| ExProduct::new(vec![ExConst::new(k).exprall(), ex]).exprall(),
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
impl Expr for ExSum {
    fn eval(&self, vars: &VarValues) -> num_complex::Complex64 {
        self.1
            .get_expralls()
            .into_iter()
            .map(|v| v.eval(vars))
            .sum()
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        let children = self.1.get_expralls_filtering(Const::is_zero);

        if self.1.is_only_consts() {
            // (k) = k
            ExprAll::Const(self.1.get_consts())
        } else if children.len() == 1 {
            // (x) = x
            children[0].clone()
        } else {
            ExprAll::Sum(self.clone())
        }
    }
    fn derivative(self: &Rc<Self>, var: Var) -> ExprAll {
        // sum rule (a + b + c + {const})' = a' + b' + c'
        ExSum::new(
            self.children()
                .get_nonconsts()
                .iter()
                .map(|v| v.derivative(var))
                .collect(),
        )
        .exprall()
    }
    fn id(&self) -> Id {
        self.0
    }
}
impl ExprAssociativeCommuttative for ExSum {
    fn reduce_consts<I: Iterator<Item = Const>>(consts: I) -> Const {
        consts.sum()
    }
    fn associates_with(child: ExprAll) -> Result<Vec<ExprAll>, ExprAll> {
        match child {
            ExprAll::Sum(sum) => Ok(sum.1.get_expralls()),
            child => Err(child),
        }
    }
    fn children(&self) -> &ChildrenAssociativeCommutative {
        &self.1
    }
}
impl PartialEq for ExSum {
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

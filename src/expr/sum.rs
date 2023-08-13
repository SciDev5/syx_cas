use std::{
    cell::RefCell,
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    rc::Rc,
};

use super::{
    associative_commutative::{ChildrenAssociativeCommutative, ExprAssociativeCommuttative},
    consts::Const,
    var::VarValues,
    Expr, ExprAll, ExprScope, Id,
};

#[derive(Debug)]
pub struct ExSum(Id, ChildrenAssociativeCommutative);
impl ExSum {
    pub fn new(scope: &Rc<RefCell<ExprScope>>, children: Vec<ExprAll>) -> Rc<Self> {
        let children = ChildrenAssociativeCommutative::new::<Self>(scope.clone(), children);
        let content_hash = {
            let mut h = DefaultHasher::new();
            children.hash(&mut h);
            h.finish()
        };
        let id = Id { content_hash };
        scope
            .borrow_mut()
            .sums
            .entry(id)
            .or_insert_with(|| Rc::new(Self(id, children)))
            .clone()
    }
}
impl Expr for ExSum {
    fn eval(&self, vars: &VarValues) -> Const {
        self.1
            .get_expralls()
            .into_iter()
            .map(|v| v.eval(vars))
            .sum()
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        ExprAll::Sum(self.clone())
    }
    fn id(&self) -> Id {
        self.0
    }
    fn exprfmtprecedence(self: &Rc<Self>) -> crate::util::fmt_latex::ExprFmtPrecedence {
        crate::util::fmt_latex::ExprFmtPrecedence::AS
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
    use crate::expr::{
        consts::{Const, ExConst},
        sum::ExSum,
        ExprAll, ExprScope,
    };

    #[test]
    fn hash_equality() {
        let scope = ExprScope::new();

        let consts = [
            ExConst::new(&scope, Const(1)),
            ExConst::new(&scope, Const(1)),
            ExConst::new(&scope, Const(5)),
        ];

        let sums = [
            ExSum::new(
                &scope,
                vec![
                    ExprAll::Const(consts[0].clone()),
                    ExprAll::Const(consts[0].clone()),
                ],
            ),
            ExSum::new(
                &scope,
                vec![
                    ExprAll::Const(consts[0].clone()),
                    ExprAll::Const(consts[1].clone()),
                ],
            ),
            ExSum::new(
                &scope,
                vec![
                    ExprAll::Const(consts[2].clone()),
                    ExprAll::Const(consts[1].clone()),
                ],
            ),
            ExSum::new(
                &scope,
                vec![
                    ExprAll::Const(consts[0].clone()),
                    ExprAll::Const(consts[2].clone()),
                ],
            ),
            ExSum::new(
                &scope,
                vec![
                    ExprAll::Const(consts[0].clone()),
                    ExprAll::Const(consts[2].clone()),
                    ExprAll::Const(consts[2].clone()),
                ],
            ),
        ];

        assert_eq!(sums[0], sums[1]);
        assert_eq!(sums[2], sums[3]);

        assert_ne!(sums[0], sums[2]);
        assert_ne!(sums[0], sums[3]);
        assert_ne!(sums[1], sums[3]);

        assert_ne!(sums[3], sums[4]);
    }
}

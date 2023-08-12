use std::{
    cell::RefCell,
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    rc::Rc,
};

use super::{var::VarValues, Expr, ExprAll, ExprScope, Id};

#[derive(Debug, Clone, Copy, Hash)]
pub struct Const(pub i32);
impl Const {
    pub fn pow(self, other: Self) -> Const {
        Const(self.0.pow(other.0 as u32))
    }
}
impl std::ops::Add for Const {
    type Output = Const;
    fn add(mut self, rhs: Self) -> Self::Output {
        self.0 += rhs.0;
        self
    }
}
impl std::ops::Mul for Const {
    type Output = Const;
    fn mul(mut self, rhs: Self) -> Self::Output {
        self.0 *= rhs.0;
        self
    }
}
impl std::ops::Div for Const {
    type Output = Const;
    fn div(mut self, rhs: Self) -> Self::Output {
        self.0 /= rhs.0;
        self
    }
}
impl std::iter::Sum<Const> for Const {
    fn sum<I: Iterator<Item = Const>>(iter: I) -> Self {
        let mut sum = Const(0);
        for ent in iter {
            sum = sum + ent;
        }
        sum
    }
}
impl std::iter::Product<Const> for Const {
    fn product<I: Iterator<Item = Const>>(iter: I) -> Self {
        let mut prod = Const(1);
        for ent in iter {
            prod = prod * ent;
        }
        prod
    }
}

#[derive(Debug)]
pub struct ExConst(Id, pub Const);
impl ExConst {
    pub fn new(scope: &Rc<RefCell<ExprScope>>, c: Const) -> Rc<Self> {
        let content_hash = {
            let mut h = DefaultHasher::new();
            c.hash(&mut h);
            h.finish()
        };
        let id = Id { content_hash };
        scope
            .borrow_mut()
            .consts
            .entry(id)
            .or_insert_with(|| Rc::new(Self(id, c)))
            .clone()
    }
}
impl Expr for ExConst {
    fn eval(&self, _vars: &VarValues) -> Const {
        self.1
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        ExprAll::Const(self.clone())
    }
    fn id(&self) -> Id {
        self.0
    }
}
impl PartialEq for ExConst {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

#[cfg(test)]
mod test {
    use crate::expr::{
        consts::{Const, ExConst},
        ExprScope,
    };

    #[test]
    fn hash_equality() {
        let scope = ExprScope::new();

        let consts = [
            ExConst::new(&scope, Const(1)),
            ExConst::new(&scope, Const(1)),
            ExConst::new(&scope, Const(5)),
        ];

        assert_eq!(consts[0], consts[1]);
        assert_ne!(consts[0], consts[2]);
        assert_ne!(consts[1], consts[2]);
    }
}

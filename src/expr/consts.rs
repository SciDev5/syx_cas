use std::{
    collections::hash_map::DefaultHasher,
    fmt::Display,
    hash::{Hash, Hasher},
    rc::Rc,
};

use super::{var::VarValues, Expr, ExprAll, Id};

#[derive(Debug, Clone, Copy, Hash)]
pub struct Const(pub i32);
impl Const {
    pub fn pow(self, other: Self) -> Const {
        Const(self.0.pow(other.0 as u32))
    }
    pub fn is_zero(&self) -> bool {
        self.0 == 0
    }
    pub fn is_one(&self) -> bool {
        self.0 == 1
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
impl PartialEq for Const {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl Display for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub struct ExConst(Id, pub Const);
impl ExConst {
    pub fn new(c: Const) -> Rc<Self> {
        let content_hash = {
            let mut h = DefaultHasher::new();
            c.hash(&mut h);
            h.finish()
        };
        let id = Id { content_hash };
        Rc::new(Self(id, c))
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
    use crate::expr::consts::{Const, ExConst};

    #[test]
    fn hash_equality() {
        let consts = [
            ExConst::new(Const(1)),
            ExConst::new(Const(1)),
            ExConst::new(Const(5)),
        ];

        assert_eq!(consts[0], consts[1]);
        assert_ne!(consts[0], consts[2]);
        assert_ne!(consts[1], consts[2]);
    }
}

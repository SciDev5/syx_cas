use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::consts::{Const, ZERO};

use super::{
    var::{Var, VarValues},
    Expr, ExprAll, Id,
};

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
    fn eval(&self, _vars: &VarValues) -> num_complex::Complex64 {
        self.1.complex64()
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        ExprAll::Const(self.clone())
    }
    fn derivative(self: &Rc<Self>, _var: Var) -> ExprAll {
        Self::new(ZERO).exprall()
    }
    fn child_exprs(self: &Rc<Self>) -> Vec<ExprAll> {
        vec![]
    }
    fn transform_children<F: Fn(&ExprAll) -> ExprAll>(self: &Rc<Self>, _f: F) -> Rc<Self> {
        self.clone()
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
    use crate::{
        consts::ONE,
        expr::consts::{Const, ExConst},
    };

    #[test]
    fn hash_equality() {
        let consts = [
            ExConst::new(ONE),
            ExConst::new(ONE),
            ExConst::new(Const::Int(5)),
        ];

        assert_eq!(consts[0], consts[1]);
        assert_ne!(consts[0], consts[2]);
        assert_ne!(consts[1], consts[2]);
    }
}

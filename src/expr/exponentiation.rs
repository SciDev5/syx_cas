use std::{collections::hash_map::DefaultHasher, hash::Hasher, rc::Rc};

use super::{consts::Const, var::VarValues, Expr, ExprAll, Id};

#[derive(Debug)]
pub struct ExExponentiate(Id, ExprAll, ExprAll);
impl ExExponentiate {
    pub fn new(base: ExprAll, exponent: ExprAll) -> Rc<Self> {
        let content_hash = {
            let mut h = DefaultHasher::new();
            h.write_u64(base.get_hash());
            h.write_u64(exponent.get_hash());
            h.finish()
        };
        let id = Id { content_hash };
        Rc::new(Self(id, base, exponent))
    }
    pub fn base(&self) -> &ExprAll {
        &self.1
    }
    pub fn exponent(&self) -> &ExprAll {
        &self.2
    }
}
impl Expr for ExExponentiate {
    fn eval(&self, vars: &VarValues) -> Const {
        let base = self.1.eval(vars);
        let exponent = self.2.eval(vars);

        base.pow(exponent)
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        ExprAll::Exponent(self.clone())
    }
    fn id(&self) -> Id {
        self.0
    }
}
impl PartialEq for ExExponentiate {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

#[cfg(test)]
mod test {
    use crate::expr::{
        exponentiation::ExExponentiate,
        var::{ExVar, Var},
        Expr,
    };

    #[test]
    fn hash_equality() {
        let vars = [ExVar::new(Var::new("a")), ExVar::new(Var::new("b"))];

        let exps = [
            ExExponentiate::new(vars[0].exprall(), vars[1].exprall()),
            ExExponentiate::new(vars[0].exprall(), vars[0].exprall()),
            ExExponentiate::new(vars[1].exprall(), vars[0].exprall()),
        ];

        assert_ne!(exps[0], exps[1]);
        assert_ne!(exps[0], exps[2]);
    }
}

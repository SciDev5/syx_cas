use std::{cell::RefCell, collections::hash_map::DefaultHasher, hash::Hasher, rc::Rc};

use super::{consts::Const, var::VarValues, Expr, ExprAll, ExprScope, Id};

#[derive(Debug)]
pub struct ExDivide(Id, ExprAll, ExprAll);
impl ExDivide {
    pub fn new(
        scope: &Rc<RefCell<ExprScope>>,
        numerator: ExprAll,
        denominator: ExprAll,
    ) -> Rc<Self> {
        let content_hash = {
            let mut h = DefaultHasher::new();
            h.write_u64(numerator.get_hash());
            h.write_u64(denominator.get_hash());
            h.finish()
        };
        let id = Id { content_hash };
        scope
            .borrow_mut()
            .divisions
            .entry(id)
            .or_insert_with(|| Rc::new(Self(id, numerator, denominator)))
            .clone()
    }
}
impl Expr for ExDivide {
    fn eval(&self, vars: &VarValues) -> Const {
        let numerator = self.1.eval(vars);
        let denominator = self.2.eval(vars);

        numerator / denominator
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        ExprAll::Divide(self.clone())
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
        Expr, ExprScope,
    };

    #[test]
    fn hash_equality() {
        let scope = ExprScope::new();

        let vars = [
            ExVar::new(&scope, Var::new("a")),
            ExVar::new(&scope, Var::new("b")),
        ];

        let divs = [
            ExDivide::new(&scope, vars[0].exprall(), vars[1].exprall()),
            ExDivide::new(&scope, vars[0].exprall(), vars[0].exprall()),
            ExDivide::new(&scope, vars[1].exprall(), vars[0].exprall()),
        ];

        assert_ne!(divs[0], divs[1]);
        assert_ne!(divs[0], divs[2]);
    }
}

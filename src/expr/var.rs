use std::{
    cell::RefCell,
    collections::HashMap,
    hash::{Hash, Hasher},
    rc::Rc,
};

use super::{consts::Const, Expr, ExprAll, ExprScope, Id};

#[derive(Debug, Clone, Copy, Eq)]
pub struct Var(&'static str, u64);
impl Var {
    pub fn new(name: &'static str) -> Self {
        Var(name, rand::random())
    }
}
impl PartialEq for Var {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}
impl Hash for Var {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(0xeb42_aa23_948b_300f);
        state.write_u64(self.1);
    }
}
pub struct VarValues(HashMap<Var, Const>);
impl VarValues {
    pub fn set(&mut self, var: Var, val: Const) {
        self.0.insert(var, val);
    }
    pub fn new() -> Self {
        Self(HashMap::new())
    }
}

#[derive(Debug)]
pub struct ExVar(Id, Var);
impl ExVar {
    pub fn new(scope: &Rc<RefCell<ExprScope>>, var: Var) -> Rc<Self> {
        let content_hash = var.1;
        let id = Id { content_hash };
        scope
            .borrow_mut()
            .vars
            .entry(id)
            .or_insert_with(|| Rc::new(Self(id, var)))
            .clone()
    }
    pub fn var_name(&self) -> &'static str {
        self.1 .0
    }
}
impl Expr for ExVar {
    fn eval(&self, vars: &VarValues) -> Const {
        *vars.0.get(&self.1).expect("eval variable unassigned")
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        ExprAll::Var(self.clone())
    }
    fn id(&self) -> Id {
        self.0
    }
    fn exprfmtprecedence(self: &Rc<Self>) -> crate::util::fmt_latex::ExprFmtPrecedence {
        crate::util::fmt_latex::ExprFmtPrecedence::V
    }
}
impl PartialEq for ExVar {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

#[cfg(test)]
mod test {
    use crate::expr::{
        var::{ExVar, Var},
        ExprScope,
    };

    #[test]
    fn hash_equality() {
        let scope = ExprScope::new();

        let a = Var::new("a");
        let b = Var::new("b");
        let vars = [
            ExVar::new(&scope, a),
            ExVar::new(&scope, a),
            ExVar::new(&scope, b),
        ];

        assert_eq!(vars[0], vars[1]);
        assert_ne!(vars[0], vars[2]);
        assert_ne!(vars[1], vars[2]);
    }
}

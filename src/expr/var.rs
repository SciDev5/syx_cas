use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
    rc::Rc,
};

use super::{consts::Const, Expr, ExprAll, Id};

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
pub type VarValues = HashMap<Var, Const>;

#[derive(Debug)]
pub struct ExVar(Id, Var);
impl ExVar {
    pub fn new(var: Var) -> Rc<Self> {
        let content_hash = var.1;
        let id = Id { content_hash };
        Rc::new(Self(id, var))
    }
    pub fn var_name(&self) -> &'static str {
        self.1 .0
    }
}
impl Expr for ExVar {
    fn eval(&self, vars: &VarValues) -> Const {
        *vars.get(&self.1).expect("eval variable unassigned")
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        ExprAll::Var(self.clone())
    }
    fn id(&self) -> Id {
        self.0
    }
}
impl PartialEq for ExVar {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

#[cfg(test)]
mod test {
    use crate::expr::var::{ExVar, Var};

    #[test]
    fn hash_equality() {
        let a = Var::new("a");
        let b = Var::new("b");
        let vars = [ExVar::new(a), ExVar::new(a), ExVar::new(b)];

        assert_eq!(vars[0], vars[1]);
        assert_ne!(vars[0], vars[2]);
        assert_ne!(vars[1], vars[2]);
    }
}

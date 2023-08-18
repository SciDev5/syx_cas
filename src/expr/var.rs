use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::consts::Const;

use super::{consts::ExConst, derivative::ExDerivative, Expr, ExprAll, Id};

#[derive(Debug, Clone, Copy, Eq)]
pub struct Var {
    name: &'static str,
    id: u64,
    is_dependent: bool,
}
impl Var {
    pub fn new(name: &'static str, is_dependent: bool) -> Self {
        Var {
            name,
            id: rand::random(),
            is_dependent,
        }
    }
    pub fn name(&self) -> &'static str {
        self.name
    }
    pub fn is_dependent(&self) -> bool {
        self.is_dependent
    }
    pub fn is_independent(&self) -> bool {
        !self.is_dependent
    }
}
impl PartialEq for Var {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Hash for Var {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(0xeb42_aa23_948b_300f);
        state.write_u64(self.id);
    }
}
pub type VarValues = HashMap<Var, num_complex::Complex64>;

#[derive(Debug)]
pub struct ExVar {
    id: Id,
    pub var: Var,
}
impl ExVar {
    pub fn new(var: Var) -> Rc<Self> {
        let content_hash = var.id;
        let id = Id { content_hash };
        Rc::new(Self { id, var })
    }
}
impl Expr for ExVar {
    fn eval(&self, vars: &VarValues) -> num_complex::Complex64 {
        *vars.get(&self.var).expect("eval variable unassigned")
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        ExprAll::Var(self.clone())
    }
    fn derivative(self: &Rc<Self>, var: Var) -> ExprAll {
        if var == self.var {
            ExConst::new(Const::Int(1)).exprall()
        } else if self.var.is_dependent() {
            ExDerivative::new(self.exprall(), var, 1).exprall()
        } else {
            ExConst::new(Const::Int(0)).exprall()
        }
    }
    fn id(&self) -> Id {
        self.id
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
        let a = Var::new("a", false);
        let b = Var::new("b", false);
        let vars = [ExVar::new(a), ExVar::new(a), ExVar::new(b)];

        assert_eq!(vars[0], vars[1]);
        assert_ne!(vars[0], vars[2]);
        assert_ne!(vars[1], vars[2]);
    }
}

use std::{collections::hash_map::DefaultHasher, hash::Hasher, rc::Rc};

use crate::consts::ONE;

use super::{
    consts::ExConst,
    product::ExProduct,
    var::{Var, VarValues},
    Expr, ExprAll, Id,
};

#[derive(Debug)]
pub struct ExExp(Id, ExprAll);
impl ExExp {
    pub fn new(v: ExprAll) -> Rc<Self> {
        let content_hash = {
            let mut h = DefaultHasher::new();
            h.write_u64(v.get_hash());
            h.finish()
        };
        let id = Id { content_hash };
        Rc::new(Self(id, v))
    }
    pub fn exponent(&self) -> &ExprAll {
        &self.1
    }
}
impl Expr for ExExp {
    fn eval(&self, vars: &VarValues) -> num_complex::Complex64 {
        self.1.eval(vars).exp()
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        match self.exponent() {
            ExprAll::Const(exp) => {
                if exp.1.is_zero() {
                    return ExConst::new(ONE).exprall();
                }
            }
            ExprAll::Ln(ln) => {
                // exp(ln(f(x))) = f(x) no matter what, even for complex valued f(x)
                return ln.argument().clone();
            }
            _ => {}
        }
        ExprAll::Exp(self.clone())
    }
    fn derivative(self: &Rc<Self>, var: Var) -> ExprAll {
        ExProduct::new(vec![self.exprall(), self.exponent().derivative(var)]).exprall()
    }
    fn child_exprs(self: &Rc<Self>) -> Vec<ExprAll> {
        vec![self.exponent().clone()]
    }
    fn transform_children<F: Fn(&ExprAll) -> ExprAll>(self: &Rc<Self>, f: F) -> Rc<Self> {
        ExExp::new(f(self.exponent()))
    }
    fn id(&self) -> Id {
        self.0
    }
}
impl PartialEq for ExExp {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

#[cfg(test)]
mod test {
    use crate::expr::{
        exp::ExExp,
        var::{ExVar, Var},
        Expr,
    };

    #[test]
    fn hash_equality() {
        let vars = [
            ExVar::new(Var::new("a", false)),
            ExVar::new(Var::new("b", false)),
        ];

        let exps = [ExExp::new(vars[0].exprall()), ExExp::new(vars[1].exprall())];
        assert_ne!(exps[0], exps[1]);
        assert_eq!(exps[0], exps[0]);
    }
}

use std::{collections::hash_map::DefaultHasher, hash::Hasher, rc::Rc};

use crate::consts::ZERO;

use super::{
    consts::ExConst,
    division::ExDivide,
    var::{Var, VarValues},
    Expr, ExprAll, Id,
};

#[derive(Debug)]
pub struct ExLn(Id, ExprAll);
impl ExLn {
    pub fn new(v: ExprAll) -> Rc<Self> {
        let content_hash = {
            let mut h = DefaultHasher::new();
            h.write_u64(v.get_hash());
            h.finish()
        };
        let id = Id { content_hash };
        Rc::new(Self(id, v))
    }
    pub fn argument(&self) -> &ExprAll {
        &self.1
    }
}
impl Expr for ExLn {
    fn eval(&self, vars: &VarValues) -> num_complex::Complex64 {
        self.1.eval(vars).ln()
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        match self.argument() {
            ExprAll::Const(arg) => {
                if arg.1.is_one() {
                    return ExConst::new(ZERO).exprall();
                }
            }
            // NOTE: $\ln(\exp(x)) = x + 2 \pi i n$ for all integer $n$, and so cannot be simply reduced
            _ => {}
        }
        ExprAll::Ln(self.clone())
    }
    fn derivative(self: &Rc<Self>, var: Var) -> ExprAll {
        ExDivide::new(
            self.argument().clone().derivative(var),
            self.argument().clone(),
        )
        .exprall()
    }
    fn child_exprs(self: &Rc<Self>) -> Vec<ExprAll> {
        vec![self.argument().clone()]
    }
    fn transform_children<F: Fn(&ExprAll) -> ExprAll>(self: &Rc<Self>, f: F) -> Rc<Self> {
        ExLn::new(f(self.argument()))
    }
    fn id(&self) -> Id {
        self.0
    }
}
impl PartialEq for ExLn {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

#[cfg(test)]
mod test {
    use crate::expr::{
        ln::ExLn,
        var::{ExVar, Var},
        Expr,
    };

    #[test]
    fn hash_equality() {
        let vars = [
            ExVar::new(Var::new("a", false)),
            ExVar::new(Var::new("b", false)),
        ];

        let exps = [ExLn::new(vars[0].exprall()), ExLn::new(vars[1].exprall())];
        assert_ne!(exps[0], exps[1]);
        assert_eq!(exps[0], exps[0]);
    }
}

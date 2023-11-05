use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::consts::{ONE, ZERO};

use super::{
    consts::ExConst,
    var::{Var, VarValues},
    Expr, ExprAll, Id,
};

#[derive(Debug)]
pub struct ExDerivative {
    id: Id,
    pub expr: ExprAll,
    pub var: Var,
    pub order: u32,
}
impl ExDerivative {
    pub fn new(expr: ExprAll, var: Var, order: u32) -> Rc<Self> {
        let content_hash = {
            let mut h = DefaultHasher::new();
            h.write_u64(expr.get_hash());
            var.hash(&mut h);
            h.finish()
        };
        let id = Id { content_hash };
        Rc::new(Self {
            id,
            expr,
            order,
            var,
        })
    }
}
impl Expr for ExDerivative {
    fn eval(&self, vars: &VarValues) -> num_complex::Complex64 {
        // FIXME assumes real independent variables (eg. no complex analysis)
        const DX_RE: f64 = 1e-11;
        const DX: num_complex::Complex64 = num_complex::Complex64::new(DX_RE, 0.0);

        let mut pos = vars.clone();
        let mut neg = vars.clone();

        *(pos.get_mut(&self.var).expect("eval variable unassigned")) += DX / 2.0;
        *(neg.get_mut(&self.var).expect("eval variable unassigned")) -= DX / 2.0;

        (self.expr.eval(&pos) - self.expr.eval(&neg)) / DX_RE
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        if let ExprAll::Const(_) = &self.expr {
            // `d/dx C = 0` for const `C`
            return ExConst::new(ZERO).exprall();
        }
        if let ExprAll::Var(v) = &self.expr {
            if v.var.is_independent() && v.var != self.var {
                // different independent variable
                // `dx/dy = 0` if `x` and `y` are both independent
                return ExConst::new(ZERO).exprall();
            }
            if v.var == self.var {
                // `dx/dx = 1` obviously lmao
                return ExConst::new(ONE).exprall();
            }
        }
        ExprAll::Derivative(self.clone())
    }
    fn derivative(self: &Rc<Self>, var: Var) -> ExprAll {
        (if var == self.var {
            // increase order
            ExDerivative::new(self.expr.clone(), var, self.order + 1)
        } else if let ExprAll::Var(_) = &self.expr {
            // keep d/dy df/dx unchanged
            ExDerivative::new(self.exprall(), var, 1)
        } else {
            // commutability of derivatives
            ExDerivative::new(self.expr.derivative(var), self.var, self.order)
        })
        .exprall()
    }
    fn child_exprs(self: &Rc<Self>) -> Vec<ExprAll> {
        vec![self.expr.clone()]
    }
    fn transform_children<F: Fn(&ExprAll) -> ExprAll>(self: &Rc<Self>, f: F) -> Rc<Self> {
        ExDerivative::new(f(&self.expr), self.var, self.order)
    }
    fn id(&self) -> super::Id {
        self.id
    }
}

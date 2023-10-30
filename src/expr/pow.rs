use std::{collections::hash_map::DefaultHasher, hash::Hasher, rc::Rc};

use crate::{
    consts::{NEG_ONE, ONE, ZERO},
    util::expr_maker::ExprMaker,
};

use super::{
    consts::ExConst,
    var::{Var, VarValues},
    Expr, ExprAll, Id,
};

#[derive(Debug)]
pub struct ExPow(Id, ExprAll, ExprAll);
impl ExPow {
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
impl Expr for ExPow {
    fn eval(&self, vars: &VarValues) -> num_complex::Complex64 {
        let base = self.1.eval(vars);
        let exponent = self.2.eval(vars);

        base.powc(exponent)
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        if let ExprAll::Const(exp) = self.exponent() {
            if exp.1.is_one() {
                return self.base().clone();
            }
            if let ExprAll::Const(base) = self.base() {
                if exp.1.is_zero() && !base.1.is_zero() {
                    return ExConst::new(ONE).exprall();
                }
            } else {
                if exp.1.is_zero() {
                    // x ^ 0 = 1 and x != 0
                    // TODO enforce x != 0
                    return ExConst::new(ONE).exprall();
                }
            }
        } else if let ExprAll::Const(base) = self.base() {
            if base.1.is_zero() {
                // 0 ^ x = 0 and x > 0
                // TODO enforce x > 0
                return ExConst::new(ZERO).exprall();
            }
            if base.1.is_one() {
                // 1 ^ x = 1 (always, even in the limit as $x \to \pm\infty$ because it's a const here)
                return ExConst::new(ONE).exprall();
            }
        }
        ExprAll::Pow(self.clone())
    }
    fn derivative(self: &Rc<Self>, var: Var) -> ExprAll {
        if let ExprAll::Const(_base) = self.base() {
            if let ExprAll::Const(_exp) = self.exponent() {
                // {const}^{const}
                ExConst::new(ZERO).exprall()
            } else {
                // {const}^{f(x)}
                // (
                //     Ln(ExprMaker::ConstRaw(base.1)) // Can't
                //     * ExprMaker::Raw(self.exprall())
                //     * ExprMaker::Raw(self.exponent().derivative(var))
                // ).build()
                todo!("derivative depends on having functions like ln made but like i havent defined those yet");
            }
        } else {
            if let ExprAll::Const(exp) = self.exponent() {
                // {f(x)}^{const}
                (ExprMaker::ConstRaw(exp.1)
                    * ExprMaker::Raw(self.base().clone()).pow(ExprMaker::ConstRaw(exp.1 + NEG_ONE))
                    * ExprMaker::Raw(self.base().derivative(var)))
                .build()
            } else {
                // TODO d/dx f(x)^g(x)
                todo!("this derivative is complicated and im too lazy to evaluate it right now (also it depends on having functions like ln made alr)");
            }
        }
    }
    fn id(&self) -> Id {
        self.0
    }
}
impl PartialEq for ExPow {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

#[cfg(test)]
mod test {
    use crate::expr::{
        pow::ExPow,
        var::{ExVar, Var},
        Expr,
    };

    #[test]
    fn hash_equality() {
        let vars = [
            ExVar::new(Var::new("a", false)),
            ExVar::new(Var::new("b", false)),
        ];

        let exps = [
            ExPow::new(vars[0].exprall(), vars[1].exprall()),
            ExPow::new(vars[0].exprall(), vars[0].exprall()),
            ExPow::new(vars[1].exprall(), vars[0].exprall()),
        ];

        assert_ne!(exps[0], exps[1]);
        assert_ne!(exps[0], exps[2]);
    }
}
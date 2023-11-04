use std::{hash, rc::Rc};


use self::{
    consts::ExConst,
    derivative::ExDerivative,
    division::ExDivide,
    exp::ExExp,
    ln::ExLn,
    pow::ExPow,
    product::ExProduct,
    sum::ExSum,
    var::{ExVar, Var, VarValues},
};

pub mod associative_commutative;
pub mod consts;
pub mod derivative;
pub mod division;
pub mod exp;
pub mod ln;
pub mod pow;
pub mod product;
pub mod sum;
pub mod var;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Id {
    pub content_hash: u64,
}

pub trait Expr {
    fn eval(&self, vars: &VarValues) -> num_complex::Complex64;
    fn id(&self) -> Id;
    /** Get the trivially simplified (ex. 0*k -> 0 but (a+3)(b+2) remains the same) ExprAll representing this Expr. */
    fn exprall(self: &Rc<Self>) -> ExprAll;
    fn derivative(self: &Rc<Self>, var: Var) -> ExprAll;
    /** Returns true if this expression is written in terms of `var`. NOTE: May return true even if `var` will cancel out (ex: $x-x$ -> true). */
    fn has_explicit_dependence(self: &Rc<Self>, var: Var) -> bool;
    fn substitute(self: &Rc<Self>, var: Var, expr: ExprAll) -> ExprAll;
}

#[derive(Debug, Clone)]
pub enum ExprAll {
    Const(Rc<ExConst>),
    Sum(Rc<ExSum>),
    Product(Rc<ExProduct>),
    Pow(Rc<ExPow>),
    Exp(Rc<ExExp>),
    Ln(Rc<ExLn>),
    Divide(Rc<ExDivide>),
    Var(Rc<ExVar>),
    Derivative(Rc<ExDerivative>),
}
impl ExprAll {
    pub fn eval(&self, vars: &VarValues) -> num_complex::Complex64 {
        match self {
            Self::Const(v) => v.eval(vars),
            Self::Var(v) => v.eval(vars),
            Self::Sum(v) => v.eval(vars),
            Self::Product(v) => v.eval(vars),
            Self::Pow(v) => v.eval(vars),
            Self::Exp(v) => v.eval(vars),
            Self::Ln(v) => v.eval(vars),
            Self::Divide(v) => v.eval(vars),
            Self::Derivative(v) => v.eval(vars),
        }
    }
    fn get_hash(&self) -> u64 {
        match self {
            Self::Const(v) => v.id().content_hash ^ 0xd674_0330_30c0_0c40, // hardcoded noise to avoid hash collisions
            Self::Var(v) => v.id().content_hash ^ 0x36a7_4d41_2258_3d0e,
            Self::Sum(v) => v.id().content_hash ^ 0x0992_3158_b088_c199,
            Self::Product(v) => v.id().content_hash ^ 0xadc8_cd4a_57ea_2881,
            Self::Pow(v) => v.id().content_hash ^ 0x676c_ec63_7c5e_d41a,
            Self::Exp(v) => v.id().content_hash ^ 0x0434_070d_4711_b7be,
            Self::Ln(v) => v.id().content_hash ^ 0x3092_228d_687c_c9ab,
            Self::Divide(v) => v.id().content_hash ^ 0xc684_0800_00c5_b600,
            Self::Derivative(v) => v.id().content_hash ^ 0xb29c_4558_6bbd_b2e6,
            // 0xc3ab_d70d_e223_0489
            // 0x6cb0_36ed_0116_0b13
            // 0x4ac6_cb03_d793_4d05
            // 0x0a00_7007_983d_5738
        }
    }
    pub fn derivative(&self, var: Var) -> ExprAll {
        match self {
            Self::Const(v) => v.derivative(var),
            Self::Var(v) => v.derivative(var),
            Self::Sum(v) => v.derivative(var),
            Self::Product(v) => v.derivative(var),
            Self::Pow(v) => v.derivative(var),
            Self::Exp(v) => v.derivative(var),
            Self::Ln(v) => v.derivative(var),
            Self::Divide(v) => v.derivative(var),
            Self::Derivative(v) => v.derivative(var),
        }
    }
    pub fn has_explicit_dependence(&self, var: Var) -> bool {
        match self {
            Self::Const(v) => v.has_explicit_dependence(var),
            Self::Var(v) => v.has_explicit_dependence(var),
            Self::Sum(v) => v.has_explicit_dependence(var),
            Self::Product(v) => v.has_explicit_dependence(var),
            Self::Pow(v) => v.has_explicit_dependence(var),
            Self::Exp(v) => v.has_explicit_dependence(var),
            Self::Ln(v) => v.has_explicit_dependence(var),
            Self::Divide(v) => v.has_explicit_dependence(var),
            Self::Derivative(v) => v.has_explicit_dependence(var),
        }
    }
    pub fn substitute(&self, var: Var, expr: ExprAll) -> ExprAll {
        match self {
            Self::Const(v) => v.substitute(var, expr),
            Self::Var(v) => v.substitute(var, expr),
            Self::Sum(v) => v.substitute(var, expr),
            Self::Product(v) => v.substitute(var, expr),
            Self::Pow(v) => v.substitute(var, expr),
            Self::Exp(v) => v.substitute(var, expr),
            Self::Ln(v) => v.substitute(var, expr),
            Self::Divide(v) => v.substitute(var, expr),
            Self::Derivative(v) => v.substitute(var, expr),
        }
    }
}
// Note that this implementation is checking for "matching" equality, eg, it doesn't consider variable values or advanced identities
impl PartialEq for ExprAll {
    fn eq(&self, other: &Self) -> bool {
        self.get_hash() == other.get_hash()
    }
}
impl Eq for ExprAll {}
impl hash::Hash for ExprAll {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.get_hash())
    }
}

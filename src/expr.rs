use std::rc::Rc;

use self::{
    consts::{Const, ExConst},
    division::ExDivide,
    exponentiation::ExExponentiate,
    product::ExProduct,
    sum::ExSum,
    var::{ExVar, VarValues},
};

pub mod associative_commutative;
pub mod consts;
pub mod division;
pub mod exponentiation;
pub mod product;
pub mod sum;
pub mod var;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Id {
    pub content_hash: u64,
}

pub trait Expr {
    fn eval(&self, vars: &VarValues) -> Const;
    fn id(&self) -> Id;
    /** Derivation may include simple reductions if possible. */
    fn exprall(self: &Rc<Self>) -> ExprAll;
}

#[derive(Debug, Clone)]
pub enum ExprAll {
    Const(Rc<ExConst>),
    Sum(Rc<ExSum>),
    Product(Rc<ExProduct>),
    Exponent(Rc<ExExponentiate>),
    Divide(Rc<ExDivide>),
    Var(Rc<ExVar>),
}
impl ExprAll {
    pub fn eval(&self, vars: &VarValues) -> Const {
        match self {
            Self::Const(v) => v.eval(vars),
            Self::Var(v) => v.eval(vars),
            Self::Sum(v) => v.eval(vars),
            Self::Product(v) => v.eval(vars),
            Self::Exponent(v) => v.eval(vars),
            Self::Divide(v) => v.eval(vars),
        }
    }
    fn get_hash(&self) -> u64 {
        match self {
            Self::Const(v) => v.id().content_hash ^ 0xd674_0330_30c0_0c40, // hardcoded noise to avoid hash collisions
            Self::Var(v) => v.id().content_hash ^ 0x36a7_4d41_2258_3d0e,
            Self::Sum(v) => v.id().content_hash ^ 0x0992_3158_b088_c199,
            Self::Product(v) => v.id().content_hash ^ 0xadc8_cd4a_57ea_2881,
            Self::Exponent(v) => v.id().content_hash ^ 0x676c_ec63_7c5e_d41a,
            Self::Divide(v) => v.id().content_hash ^ 0xc684_0800_00c5_b600,
            // 0xb29c_4558_6bbd_b2e6
            // 0x0434_070d_4711_b7be
            // 0x3092_228d_687c_c9ab
            // 0xc3ab_d70d_e223_0489
            // 0x6cb0_36ed_0116_0b13
            // 0x4ac6_cb03_d793_4d05
            // 0x0a00_7007_983d_5738
        }
    }
}

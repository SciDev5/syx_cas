use crate::{expr::{ExprAll, consts::ExConst, Expr}, consts::{ZERO, ONE, NEG_ONE}};

use super::tensor::TensorOpSafetyLevel;


macro_rules! impl_linear_identity_values {
    (-1 => $minus_one: expr, $(0 => $zero: expr,)? 1 => $one: expr $(,)?) => {
        $(fn zero() -> Self {
            $zero
        })?
        fn one() -> Self {
            $one
        }
        fn minus_one() -> Self {
            $minus_one
        }
    };
}

pub trait Linear:
    std::ops::Add<Output = Self>
    + std::ops::Sub<Output = Self>
    + std::ops::AddAssign
    + std::iter::Sum
    + std::ops::Mul<Output = Self>
    + Clone
    + Default
{
    fn hermitian_conj(self) -> Self;
    fn commutator(self, rhs: Self) -> Self {
        self.clone() * rhs.clone() - rhs.clone() * self.clone()
    }

    fn op_safety_add(&self, rhs: &Self) -> TensorOpSafetyLevel;
    fn op_safety_mul(&self, rhs: &Self) -> TensorOpSafetyLevel;
    
    fn zero() -> Self {
        Default::default()
    }
    fn one() -> Self;
    fn minus_one() -> Self;
}
impl Linear for f64 {
    fn hermitian_conj(self) -> Self {
        self
    }
    fn commutator(self, _other: Self) -> Self {
        0.0
    }
    fn op_safety_add(&self, _rhs: &Self) -> TensorOpSafetyLevel {
        TensorOpSafetyLevel::Safe
    }
    fn op_safety_mul(&self, _rhs: &Self) -> TensorOpSafetyLevel {
        TensorOpSafetyLevel::Safe
    }
    impl_linear_identity_values!{
        -1 => -1.0,
        0 => 0.0,
        1 => 1.0,
    }
}

impl Linear for ExprAll {
    fn hermitian_conj(self) -> Self {
        // TODO hermitian conjugate
        println!("just used temporary invalid hermitian conjugate");
        self
    }
    fn op_safety_add(&self, _rhs: &Self) -> TensorOpSafetyLevel {
        TensorOpSafetyLevel::Safe
    }
    fn op_safety_mul(&self, _rhs: &Self) -> TensorOpSafetyLevel {
        TensorOpSafetyLevel::Safe
    }
    impl_linear_identity_values!{
        -1 => ExConst::new(NEG_ONE).exprall(),
        0 => ExConst::new(ZERO).exprall(),
        1 => ExConst::new(ONE).exprall(),
    }
}

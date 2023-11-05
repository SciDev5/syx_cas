use std::{hash, rc::Rc};

use self::{
    associative_commutative::ExprAssociativeCommuttative,
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

    fn child_exprs(self: &Rc<Self>) -> Vec<ExprAll>;
    fn transform_children<F: Fn(&ExprAll) -> ExprAll>(self: &Rc<Self>, f: F) -> Rc<Self>;
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
    /** Returns true if this expression is written in terms of `var`. NOTE: May return true even if `var` will cancel out (ex: $x-x$ -> true). */
    pub fn has_explicit_dependence(&self, var: Var) -> bool {
        has_explicit_dependence(&self, var)
    }
    /** Recursively replace all instances of `var` with a replacement `ExprAll`. */
    pub fn substitute(&self, var: Var, expr: &ExprAll) -> ExprAll {
        substitute(&self, var, expr)
    }
    pub fn transform_children<F: Fn(&ExprAll) -> ExprAll>(&self, f: F) -> ExprAll {
        match self {
            Self::Const(v) => v.transform_children(f).exprall(),
            Self::Var(v) => v.transform_children(f).exprall(),
            Self::Sum(v) => v.transform_children(f).exprall(),
            Self::Product(v) => v.transform_children(f).exprall(),
            Self::Pow(v) => v.transform_children(f).exprall(),
            Self::Exp(v) => v.transform_children(f).exprall(),
            Self::Ln(v) => v.transform_children(f).exprall(),
            Self::Divide(v) => v.transform_children(f).exprall(),
            Self::Derivative(v) => v.transform_children(f).exprall(),
        }
    }
    pub fn children(&self) -> Vec<ExprAll> {
        match self {
            Self::Const(v) => v.child_exprs(),
            Self::Var(v) => v.child_exprs(),
            Self::Sum(v) => v.child_exprs(),
            Self::Product(v) => v.child_exprs(),
            Self::Pow(v) => v.child_exprs(),
            Self::Exp(v) => v.child_exprs(),
            Self::Ln(v) => v.child_exprs(),
            Self::Divide(v) => v.child_exprs(),
            Self::Derivative(v) => v.child_exprs(),
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

pub fn is_explicitly_zero(expr: &ExprAll) -> bool {
    match expr {
        ExprAll::Const(c) => c.1.is_zero(),
        ExprAll::Derivative(v) => !has_explicit_dependence(&v.expr, v.var),
        ExprAll::Divide(v) => {
            is_explicitly_zero(v.numerator()) && is_explicitly_zero(v.denominator())
        }
        ExprAll::Exp(_v) => false,
        ExprAll::Ln(v) => is_explicitly_one(v.argument()),
        ExprAll::Pow(v) => is_explicitly_zero(v.base()) && !is_explicitly_zero(v.exponent()),
        ExprAll::Product(v) => v.children().get_expralls().iter().all(is_explicitly_zero),
        ExprAll::Sum(v) => v.children().get_expralls().iter().all(is_explicitly_zero),
        ExprAll::Var(_) => false,
    }
}
pub fn is_explicitly_one(expr: &ExprAll) -> bool {
    match expr {
        ExprAll::Const(c) => c.1.is_one(),
        ExprAll::Derivative(_v) => false,
        ExprAll::Divide(_v) => false, // should automatically cancel
        ExprAll::Exp(v) => is_explicitly_zero(&v.exprall()),
        ExprAll::Ln(_v) => is_explicitly_zero(expr),
        ExprAll::Pow(v) => {
            is_explicitly_zero(v.exponent()) && !is_explicitly_zero(v.base())
                || is_explicitly_one(v.base()) && !is_explicitly_zero(v.exponent())
        }
        ExprAll::Product(_v) => false, // should automatically reduce
        ExprAll::Sum(_v) => false,     // should automatically reduce
        ExprAll::Var(_) => false,
    }
}

/** Returns true if this expression is written in terms of `var`. NOTE: May return true even if `var` will cancel out (ex: $x-x$ -> true). */
pub fn has_explicit_dependence(expr: &ExprAll, var: Var) -> bool {
    match expr {
        ExprAll::Var(v) => v.var == var,
        expr => expr
            .children()
            .into_iter()
            .any(|it| has_explicit_dependence(&it, var)),
    }
}
/** Recursively replace all instances of `var` with a replacement `ExprAll`. */
pub fn substitute(expr: &ExprAll, var: Var, replacement: &ExprAll) -> ExprAll {
    match expr {
        ExprAll::Var(v) => {
            if v.var == var {
                replacement.clone()
            } else {
                v.exprall()
            }
        }
        expr => expr.transform_children(|it| substitute(it, var, replacement)),
    }
}

/**
 * Distribute products of sums into sums of products recursively.
 * 
 * Respects noncommutability.
 */
pub fn distribute_sum_product(expr: &ExprAll) -> ExprAll {
    match expr {
        ExprAll::Product(product) => {
            let product = product.transform_children(distribute_sum_product);

            if product
                .children()
                .get_nonconsts()
                .iter()
                .all(|v| !matches!(v, ExprAll::Sum(_)))
            {
                product.exprall()
            } else {
                let mut sum_of_terms = vec![vec![product.children().get_consts().exprall()]];
                let mut next_sum_of_terms = vec![];
                for product_child in product.children().get_nonconsts().iter() {
                    if let ExprAll::Sum(sum) = product_child {
                        for sum_child in &sum
                            .children()
                            .get_expralls_filtering(|const_term| const_term.is_zero())
                        {
                            for term in &sum_of_terms {
                                let mut term = term.clone();
                                term.push(sum_child.clone());
                                next_sum_of_terms.push(term);
                            }
                        }

                        std::mem::swap(&mut sum_of_terms, &mut next_sum_of_terms);
                        next_sum_of_terms.clear();
                    } else {
                        for term in &mut sum_of_terms {
                            term.push(product_child.clone());
                        }
                    }
                }
                ExSum::new(
                    sum_of_terms
                        .into_iter()
                        .map(|term| ExProduct::new(term).exprall())
                        .collect(),
                )
                .exprall()
            }
        }
        // else just return unchanged,
        expr => expr.transform_children(distribute_sum_product),
    }
}

use crate::{
    expr::{
        associative_commutative::{ChildrenAssociativeCommutative, ExprAssociativeCommuttative},
        product::ExProduct,
        sum::ExSum,
        var::Var,
        Expr, ExprAll, self,
    },
    util::expr_maker::ExprMaker,
};

use super::polynomial::Polynomial;

#[derive(Debug, Clone)]
pub struct Equality(ExprAll);

impl Equality {
    pub fn new_expr_eq_0(v: ExprAll) -> Self {
        Self(v)
    }
    pub fn new(lhs: ExprAll, rhs: ExprAll) -> Self {
        Self::new_expr_eq_0((ExprMaker::Raw(rhs) - ExprMaker::Raw(lhs)).build())
    }

    pub fn expr_that_equals_zero(&self) -> &ExprAll {
        &self.0
    }
    
    pub fn solve_for(&self, var: Var) -> Option<SolutionList> {
        let mut exprs = vec![self.0.clone()];
        let mut exprs_next = vec![];

        // None
        loop {
            // -- BEGINNING OF MACROS --
            macro_rules! attempt_solve {
                ( $exprs: expr , $var: expr ; [$($solver: ident),* $(,)?]) => {
                    {$(
                        let mut v = None;
                        for expr in &($exprs) {
                            if let Some(solutions) = $solver.solve(expr, $var) {
                                v = Some(solutions);
                                break;
                            }
                        }
                        if v.is_some() {
                            break v;
                        }
                    )*}
                };
            }
            macro_rules! attempt_reduce {
                ($exprs: expr => $exprs_next: expr , $var: expr ; [$($solve_reducer: ident),* $(,)?]) => {
                    $(
                        for expr in &($exprs) {
                            $exprs_next.append(&mut $solve_reducer.reduce(expr, $var))
                        }
                    )*
                };
            }
            // -- END OF MACROS --

            // attempt to solve
            attempt_solve!(exprs, var; [
                PolynomialSolver,
            ]);
            // attempt to reduce the complexity of the problem
            attempt_reduce!(exprs => exprs_next, var; [
                FnAndConstSolveReducer,
                DistributeSolveReducer,
            ]);

            if exprs_next.is_empty() {
                break None; // no more ways we know to figure out how to definitively solve this problem
            }

            std::mem::swap(&mut exprs, &mut exprs_next);
            exprs_next.clear();
        }
    }
}

type SolutionList = Vec<ExprAll>;

trait SingleVarSolver {
    /**
     * Takes an expr which equals zero and attempts to find
     * solutions for `var` of `expr` = 0.
     */
    fn solve(&self, expr: &ExprAll, var: Var) -> Option<SolutionList>;
}
trait SingleVarSolveReducer {
    /** Takes an expr which equals zero and tries to reduce it to
     * another easier to handle expr that still equals zero. */
    fn reduce(&self, expr: &ExprAll, var: Var) -> Vec<ExprAll>;
}

struct PolynomialSolver;
impl SingleVarSolver for PolynomialSolver {
    fn solve(&self, expr: &ExprAll, var: Var) -> Option<SolutionList> {
        let polynomial = Polynomial::from_exprall(expr.clone(), var)?;
        polynomial.zeros()
    }
}

struct FnAndConstSolveReducer;
impl SingleVarSolveReducer for FnAndConstSolveReducer {
    fn reduce(&self, expr: &ExprAll, var: Var) -> Vec<ExprAll> {
        let mut const_part = vec![];
        let mut var_part = vec![];

        match expr {
            ExprAll::Sum(v) => {
                let (mut var_part_, mut const_part_) = children_by_dependency(v.children(), var);
                var_part.append(&mut var_part_);
                const_part.append(&mut const_part_);
            }
            v => {
                if v.has_explicit_dependence(var) {
                    var_part.push(v.clone())
                } else {
                    const_part.push(v.clone())
                }
            }
        };

        let const_part = ExprMaker::Raw(ExSum::new(const_part).exprall());

        match &var_part[..] {
            [ExprAll::Product(v)] => {
                // various items are products, see inside
                let (prod_var_part, prod_const_part) = children_by_dependency(v.children(), var);
                let prod_const_part = ExprMaker::Raw(ExProduct::new(prod_const_part).exprall());
                match &prod_var_part[..] {
                    [v] => vec![
                        // b f(x) + c = 0  ->  f(x) + c/b = 0    ;; TODO b != 0
                        (ExprMaker::Raw(v.clone()) + const_part / prod_const_part).build(),
                    ],
                    _ => vec![],
                }
            }
            [ExprAll::Exp(v)] => vec![
                // exp(f(x)) + c = 0  ->  f(x) - ln(-c) = 0   ;; TODO (+ 2 pi i n) offset
                (ExprMaker::Raw(v.exponent().clone()) - (-const_part).ln()).build(),
            ],
            [ExprAll::Ln(v)] => vec![
                // ln(f(x)) + c = 0  ->  f(x) - exp(-c) = 0
                (ExprMaker::Raw(v.argument().clone()) - (-const_part).exp()).build(),
            ],
            _ => vec![],
        }
    }
}

struct DistributeSolveReducer;
impl SingleVarSolveReducer for DistributeSolveReducer {
    fn reduce(&self, expr: &ExprAll, _var: Var) -> Vec<ExprAll> {
        let distributed = expr::distribute_sum_product(expr);
        if &distributed == expr {
            vec![]
        } else {
            vec![distributed]
        }
    }
}

/** Partitions expr child nodes by whether they are dependent on `var`.
 *
 * Returns (dependents, constants)
 */
fn children_by_dependency(
    node: &ChildrenAssociativeCommutative,
    var: Var,
) -> (Vec<ExprAll>, Vec<ExprAll>) {
    node.get_expralls()
        .iter()
        .map(|v| v.clone())
        .partition(|v| v.has_explicit_dependence(var))
}

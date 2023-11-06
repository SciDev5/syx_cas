/*
 * Start condition:
 * - multiple variables
 * - multiple equalities
 * Goal:
 * - find sets of expressions of the form ( [ {<var> = <expr>} foreach var <var> ] )[]
 *
 *
 * Useful things:
 * - once you've solved for n-1 / n vars, the last one is automatically solved
 *
 */

use std::collections::HashMap;

use crate::expr::{var::Var, ExprAll};

use super::equality::Equality;

type SystemSolution = HashMap<Var, ExprAll>;

pub fn solve_system(equalties: &[Equality], vars: &[Var]) -> Option<Vec<SystemSolution>> {
    if equalties.len() < vars.len() {
        // underconstrained, there is no way there are fully independent solutions.
        return Some(vec![]);
    }
    if equalties.len() > vars.len() {
        panic!("too many equalities, this should technically be valid with overconstraining eliminating excesses but I don't want to deal with that right now.");
    }
    if vars.len() == 1 {
        // base case, solve for single var.
        let var = vars[0];

        debug_assert_eq!(vars.len(), equalties.len());
        return Some(
            equalties[0]
                .solve_for(var)?
                .into_iter()
                .map(|v| HashMap::from([(var, v)]))
                .collect(),
        );
    }

    // eliminate n-1 of the vars using n-1 of the equations
    debug_assert_eq!(vars.len(), equalties.len());
    let n = vars.len();
    for eq_i in 0..n {
        'outer: for var_i in 0..n {
            let (residual_var, subcall_vars) = extract(&vars, var_i);
            let (residual_eq, subcall_eqs) = extract(&equalties, eq_i);

            let sub_solutions =
                if let Some(sub_solutions) = solve_system(&subcall_eqs, &subcall_vars) {
                    sub_solutions
                } else {
                    // failed to determine solutions with these values, try another.
                    continue;
                };

            // sub_solutions have <residual_var> dependency
            let mut solutions = Vec::new();
            for sub_solution in sub_solutions {
                let mut residual_var_eq_zero = residual_eq.expr_that_equals_zero().clone();
                // substitute solved var values out of residual_var_value
                for var in &subcall_vars {
                    residual_var_eq_zero = residual_var_eq_zero.substitute(*var, sub_solution.get(var).expect("solve_system failed to provide solved value for one of its eliminated vars"));
                }
                let residual_var_values = if let Some(solution) =
                    Equality::new_expr_eq_0(residual_var_eq_zero).solve_for(residual_var)
                {
                    solution
                } else {
                    continue 'outer;
                };
                for residual_var_value in residual_var_values {
                    // substitute <residual_var> out of solution
                    let mut solution = sub_solution.clone();
                    for solution_var_value in solution.values_mut() {
                        *solution_var_value =
                            solution_var_value.substitute(residual_var, &residual_var_value);
                    }
                    // add <residual_var>'s value to solution
                    solution.entry(residual_var).or_insert(residual_var_value);

                    solutions.push(solution);
                }
            }

            return Some(solutions);
        }
    }

    None // all permutations of equalities and vars are not solvable by the current system.
}

fn extract<T: Clone>(vec: &[T], i: usize) -> (T, Vec<T>) {
    let extracted = vec[i].clone();
    let remaining = vec
        .into_iter()
        .enumerate()
        .filter_map(|(j, it)| if i != j { Some(it.clone()) } else { None })
        .collect();
    (extracted, remaining)
}

#[cfg(test)]
mod test {
    use crate::{
        consts::{Const, Rational},
        expr::var::Var,
        solve::equality::Equality,
        util::expr_maker::ExprMaker::*,
    };

    use super::solve_system;

    #[test]
    fn linear_system() {
        let x = Var::new("x", false);
        let y = Var::new("y", false);
        let z = Var::new("z", false);

        let eqa = Equality::new(
            (Var(x) + Var(y) * ConstInt(2) + Var(z) * ConstInt(3)).build(),
            (ConstInt(2)).build(),
        );
        let eqb = Equality::new(
            (Var(x) + Var(y) * ConstInt(4) + Var(z) * ConstInt(4)).build(),
            (ConstInt(0)).build(),
        );
        let eqc = Equality::new((Var(x) + Var(y) + Var(z)).build(), (ConstInt(1)).build());

        let solutions =
            solve_system(&[eqa, eqb, eqc], &[x, y, z]).expect("this should be solvable");

        assert_eq!(
            solutions.len(),
            1,
            "this is a linear system, should have exactly one solution"
        );

        assert_eq!(
            solutions[0].get(&x),
            Some(&(ConstRaw(Const::Rational(Rational::new(4, 3)))).build())
        );
        assert_eq!(
            solutions[0].get(&y),
            Some(&(ConstRaw(Const::Rational(Rational::new(-5, 3)))).build())
        );
        assert_eq!(
            solutions[0].get(&z),
            Some(&(ConstRaw(Const::Rational(Rational::new(4, 3)))).build())
        );
    }

    #[test]
    fn circle_line_system() {
        let x = Var::new("x", false);
        let y = Var::new("y", false);

        let circle = Equality::new(
            (Var(x).squared() + Var(y).squared()).build(),
            (ConstInt(2)).build(),
        );
        let line = Equality::new(Var(x).build(), Var(y).build());

        let solutions = solve_system(&[circle, line], &[x, y]).expect("this should be solvable");

        assert_eq!(solutions.len(), 2);

        let pos_one = (ConstRaw(Const::Int(1))).build();
        let neg_one = (ConstRaw(Const::Int(-1))).build();

        let (i_pos, i_neg) = if solutions[0].get(&x) == Some(&(ConstRaw(Const::Int(1))).build()) {
            (0, 1)
        } else {
            (1, 0)
        };
        assert_eq!(solutions[i_neg].get(&x), Some(&neg_one));
        assert_eq!(solutions[i_neg].get(&y), Some(&neg_one));
        assert_eq!(solutions[i_pos].get(&x), Some(&pos_one));
        assert_eq!(solutions[i_pos].get(&y), Some(&pos_one));
    }
}

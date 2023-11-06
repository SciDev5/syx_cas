// puro was here

use std::fmt;

use crate::{
    consts::Const,
    expr::{associative_commutative::ExprAssociativeCommuttative, ExprAll, Expr},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExprFmtPrecedence {
    V = 4,
    P = 3,
    E = 2,
    MD = 1,
    AS = 0,
}

pub fn expr_precedence(expr: &ExprAll) -> ExprFmtPrecedence {
    match expr {
        ExprAll::Const(c) => {
            if c.1.is_complex() {
                ExprFmtPrecedence::AS
            } else {
                ExprFmtPrecedence::V
            }
        }
        ExprAll::Var(_) => ExprFmtPrecedence::V,
        ExprAll::Sum(_) => ExprFmtPrecedence::AS,
        ExprAll::Product(_) => ExprFmtPrecedence::MD,
        ExprAll::Pow(_) => ExprFmtPrecedence::E,
        ExprAll::Exp(_) => ExprFmtPrecedence::E,
        ExprAll::Ln(_) => ExprFmtPrecedence::P,
        ExprAll::Divide(_) => ExprFmtPrecedence::MD,
        ExprAll::Derivative(_) => ExprFmtPrecedence::MD,
    }
}

fn write_child(
    f: &mut fmt::Formatter<'_>,
    outer_precedence: ExprFmtPrecedence,
    child: &ExprAll,
    separator_pre_paren: &'static str,
) -> fmt::Result {
    if expr_precedence(child) < outer_precedence {
        write!(f, "{}\\left( {} \\right)", separator_pre_paren, child)
    } else if let (ExprFmtPrecedence::E, ExprAll::Const(child)) = (outer_precedence, child) {
        write!(f, "\\left( {} \\right)", child.exprall())
    } else {
        write!(f, "{}", child)
    }
}
fn write_children(
    f: &mut fmt::Formatter<'_>,
    outer_precedence: ExprFmtPrecedence,
    children: Vec<ExprAll>,
    separator: &'static str,
    separator_pre_paren: &'static str,
) -> fmt::Result {
    write_child(f, outer_precedence, &children[0], "")?; // puts consts first
    for child in &children[1..] {
        write!(f, "{}", separator)?;
        write_child(f, outer_precedence, child, separator_pre_paren)?;
    }
    Ok(())
}
impl fmt::Display for ExprAll {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprAll::Var(v) => write!(f, "{}", v.var.name()),
            ExprAll::Const(v) => write!(f, "{}", v.1),
            ExprAll::Sum(v) => write_children(
                f,
                ExprFmtPrecedence::AS,
                v.children().get_expralls_filtering(Const::is_zero),
                " + ",
                "",
            ),
            ExprAll::Product(v) => write_children(
                f,
                ExprFmtPrecedence::MD,
                v.children().get_expralls_filtering(Const::is_one),
                " ",
                "\\cdot ",
            ),
            ExprAll::Divide(v) => {
                // Notice: because \frac acts like grouping, expressions (precedence AD) like `a + b` don't need grouping,
                // therefore the effective outer_precedence inside a \frac is AS or less.
                write!(f, "\\frac{{ ")?;
                write_child(f, ExprFmtPrecedence::AS, v.numerator(), "")?;
                write!(f, " }}{{ ")?;
                write_child(f, ExprFmtPrecedence::AS, v.denominator(), "")?;
                write!(f, " }}")?;
                Ok(())
            }
            ExprAll::Pow(v) => {
                // For info on overwriting of outer_precedence, see the `ExprAll::Divide` branch.
                write!(f, "{{ ")?;
                write_child(f, ExprFmtPrecedence::E, v.base(), "")?;
                write!(f, " }} ^ {{ ")?;
                write_child(f, ExprFmtPrecedence::MD, v.exponent(), "")?;
                write!(f, " }}")?;
                Ok(())
            }
            ExprAll::Exp(v) => {
                // For info on overwriting of outer_precedence, see the `ExprAll::Divide` branch.
                write!(f, "e ^ {{ ")?;
                write_child(f, ExprFmtPrecedence::MD, v.exponent(), "")?;
                write!(f, " }}")?;
                Ok(())
            }
            ExprAll::Ln(v) => {
                // For info on overwriting of outer_precedence, see the `ExprAll::Divide` branch.
                write!(f, "\\ln ( ")?;
                write_child(f, ExprFmtPrecedence::AS, v.argument(), "")?;
                write!(f, " )")?;
                Ok(())
            }
            ExprAll::Derivative(v) => {
                if let ExprAll::Var(child_var) = &v.expr {
                    if v.order == 1 {
                        write!(
                            f,
                            "\\frac{{\\partial {}}}{{\\partial {}}} ",
                            child_var.var.name(),
                            v.var.name()
                        )?;
                    } else {
                        write!(
                            f,
                            "\\frac{{\\partial ^ {} {}}}{{\\partial {} ^ {}}} ",
                            v.order,
                            child_var.var.name(),
                            v.var.name(),
                            v.order
                        )?;
                    }
                } else {
                    if v.order == 1 {
                        write!(f, "\\frac{{\\partial}}{{\\partial {}}} ", v.var.name())?;
                    } else {
                        write!(
                            f,
                            "\\frac{{\\partial ^ {}}}{{\\partial {} ^ {}}} ",
                            v.order,
                            v.var.name(),
                            v.order
                        )?;
                    }
                    write_child(f, ExprFmtPrecedence::MD, &v.expr, "")?;
                }
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::util::fmt_latex::ExprFmtPrecedence::*;

    #[test]
    fn precedence_works_with_ord() {
        assert!(P == P); // () matches ()
        assert!(MD > AS); // * precedes +
        assert!(AS < E); // + follows ^
    }
}

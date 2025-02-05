/*

expr ::= term { op_inf? term }
term ::= { op_pre } term_inner { op_pos }
term_inner ::= ident | literal_int | literal_frac | '(' expr ')' | call
call ::= '{' ident { expr ',' } expr? '}'
literal_int ::= int
literal_frac ::= '{' int '/' int '}'
op_pre ::= [~]
op_pos ::= [*]
op_inf ::= [+-/^]

stmt ::= '$' ident expr                               // solve for
       | '$' '(' { ident } ')' expr { ',' expr }     // solve for (system)
       | '#' { ident '=' expr ';' } expr            // eval
       | ident "=>" stmt                           // assign


*/

use std::{collections::HashMap, io::BufRead, rc::Rc};

use nom::{
    branch::alt,
    bytes::{
        complete::{take_while, take_while1},
        tag,
    },
    character::complete::one_of,
    combinator::opt,
    multi::many,
    sequence::delimited,
    IResult, Parser,
};

use crate::{
    consts::{Const, Rational},
    expr::{
        consts::ExConst,
        division::ExDivide,
        exp::ExExp,
        ln::ExLn,
        pow::ExPow,
        var::{ExVar, Var},
        ExprAll,
    },
    solve::equality::Equality,
};

type Src<'a> = &'a str;

#[derive(Debug, Clone)]
enum ASTStmt {
    Solve {
        var: Rc<str>,
        expr: ASTExpr,
    },
    // SolveSystem {
    //     vars: Rc<[Rc<str>]>,
    //     exprs: Rc<[ASTExpr]>,
    // },
    Eval {
        bindings: Rc<[(Rc<str>, ASTExpr)]>,
        expr: ASTExpr,
    },
    Alias {
        name: Rc<str>,
        expr: Rc<ASTStmt>,
    },
}
#[derive(Debug, Clone)]
struct ASTExpr {
    term0: ASTTerm,
    terms: Rc<[(ASTOpInfix, ASTTerm)]>,
}
#[derive(Debug, Clone, Copy)]
enum ASTOpInfix {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}
impl ASTOpInfix {
    fn precedence(self) -> u8 {
        match self {
            Self::Add => 3,
            Self::Sub => 3,
            Self::Mul => 6,
            Self::Div => 6,
            Self::Pow => 9,
        }
    }

    fn left_associative(self) -> bool {
        match self {
            Self::Pow => false, // a^b^c  -> a^(b^c)
            _ => true,          // a*b*c -> (a*b)*c
        }
    }
    fn before(self, other: Self) -> bool {
        match self.precedence().cmp(&other.precedence()) {
            std::cmp::Ordering::Greater => true,
            std::cmp::Ordering::Less => false,
            std::cmp::Ordering::Equal => self.left_associative(),
        }
    }
}
#[derive(Debug, Clone, Copy)]
enum ASTOpOutfix {
    Neg,
    Conj,
}
#[derive(Debug, Clone)]
struct ASTTerm {
    ops_pre: Rc<[ASTOpOutfix]>,
    inner: ASTTermInner,
    ops_pos: Rc<[ASTOpOutfix]>,
}
#[derive(Debug, Clone)]
enum ASTTermInner {
    Ident(Rc<str>),
    LiteralInt(i128),
    LiteralFrac(i128, i128),
    Call(Rc<str>, Rc<[ASTExpr]>),
    Expr(Rc<ASTExpr>),
}

fn sep(src: Src) -> IResult<Src, ()> {
    take_while(|ch: char| ch.is_whitespace())
        .map(|_| ())
        .parse(src)
}
fn int(src: Src) -> IResult<Src, i128> {
    take_while(|ch: char| ch.is_ascii_digit())
        .map_opt(|chars: Src| i128::from_str_radix(chars, 10).ok())
        .parse(src)
}
fn ident(src: Src) -> IResult<Src, Rc<str>> {
    (take_while1(|ch: char| ch.is_ascii_alphanumeric() || ch == '_'))
        .map_opt(|s: Src| {
            if s.chars().next().unwrap().is_ascii_alphabetic() {
                Some(s.into())
            } else {
                None
            }
        })
        .parse(src)
}

fn expr(src: Src) -> IResult<Src, ASTExpr> {
    fn op_infix(src: Src) -> IResult<Src, ASTOpInfix> {
        opt(one_of("+-/^"))
            .map(|ch| match ch {
                Some('+') => ASTOpInfix::Add,
                Some('-') => ASTOpInfix::Sub,
                None => ASTOpInfix::Mul,
                Some('/') => ASTOpInfix::Div,
                Some('^') => ASTOpInfix::Pow,
                _ => unreachable!(),
            })
            .parse(src)
    }
    fn term(src: Src) -> IResult<Src, ASTTerm> {
        fn op_prefix(src: Src) -> IResult<Src, ASTOpOutfix> {
            one_of("~")
                .map(|ch| match ch {
                    '~' => ASTOpOutfix::Neg,
                    _ => unreachable!(),
                })
                .parse(src)
        }
        fn op_postfix(src: Src) -> IResult<Src, ASTOpOutfix> {
            one_of("*")
                .map(|ch| match ch {
                    '*' => ASTOpOutfix::Conj,
                    _ => unreachable!(),
                })
                .parse(src)
        }
        fn term_inner(src: Src) -> IResult<Src, ASTTermInner> {
            fn term_inner_ident(src: Src) -> IResult<Src, ASTTermInner> {
                ident
                    .map(|ident| ASTTermInner::Ident(ident.into()))
                    .parse(src)
            }
            fn term_inner_int(src: Src) -> IResult<Src, ASTTermInner> {
                int.map(|value| ASTTermInner::LiteralInt(value)).parse(src)
            }
            fn term_inner_bracketed(src: Src) -> IResult<Src, ASTTermInner> {
                delimited(
                    tag("{"),
                    alt((
                        (sep, int, sep, tag("/"), sep, int, sep)
                            .map(|(_, num, _, _, _, den, _)| ASTTermInner::LiteralFrac(num, den)),
                        (
                            sep,
                            ident,
                            many(.., (sep, expr, sep, tag(",")).map(|(_, expr, _, _)| expr))
                                .map(|v: Vec<_>| v),
                            opt((sep, expr).map(|(_, expr)| expr)),
                            sep,
                        )
                            .map(|(_, ident, exprs, expr_last, _)| {
                                ASTTermInner::Call(
                                    ident,
                                    exprs.into_iter().chain(expr_last.into_iter()).collect(),
                                )
                            }),
                    )),
                    tag("}"),
                )
                .parse(src)
            }
            fn term_inner_paren(src: Src) -> IResult<Src, ASTTermInner> {
                delimited(
                    tag("("),
                    (sep, expr, sep).map(|(_, expr, _)| ASTTermInner::Expr(Rc::new(expr))),
                    tag(")"),
                )
                .parse(src)
            }

            if let Ok(v) = term_inner_ident(src) {
                return Ok(v);
            }
            if let Ok(v) = term_inner_paren(src) {
                return Ok(v);
            }
            if let Ok(v) = term_inner_bracketed(src) {
                return Ok(v);
            }
            return term_inner_int(src);
        }
        (
            many(.., (op_prefix, sep).map(|(v, _)| v)).map(|v: Vec<_>| v.into()),
            term_inner,
            many(.., (sep, op_postfix).map(|(_, v)| v)).map(|v: Vec<_>| v.into()),
        )
            .map(|(ops_pre, inner, ops_pos)| ASTTerm {
                ops_pre,
                inner,
                ops_pos,
            })
            .parse(src)
    }

    (
        term,
        many(
            ..,
            (sep, op_infix, sep, term).map(|(_, op, _, term)| (op, term)),
        ),
    )
        .map(|(term0, terms): (_, Vec<_>)| ASTExpr {
            term0,
            terms: terms.into(),
        })
        .parse(src)
}

fn stmt(src: Src) -> IResult<Src, ASTStmt> {
    (
        sep,
        alt((
            // (
            //     tag("$"), sep, delimited(tag("("), many(.., ident).map(|v:Vec<_>|v), tag(")")),sep, expr, many(.., (sep,tag(","),sep, expr))
            // ).map(|()|)
            (tag("$"), sep, ident, sep, expr)
                .map(|(_, _, var, _, expr)| ASTStmt::Solve { var, expr }),
            (
                tag("#"),
                sep,
                many(
                    ..,
                    (ident, sep, tag("="), sep, expr, sep, tag(";"), sep)
                        .map(|(var, _, _, _, expr, _, _, _)| (var, expr)),
                )
                .map(|v: Vec<_>| v),
                expr,
            )
                .map(|(_, _, bindings, expr)| ASTStmt::Eval {
                    bindings: bindings.into(),
                    expr: expr,
                }),
            (expr).map(|expr| ASTStmt::Eval {
                bindings: [].into(),
                expr,
            }),
            (ident, sep, tag("=>"), sep, stmt).map(|(name, _, _, _, stmt)| ASTStmt::Alias {
                name,
                expr: Rc::new(stmt),
            }),
        )),
        sep,
    )
        .map(|(_, v, _)| v)
        .parse(src)
}

#[derive(Debug, Clone)]
enum VarOrAlias {
    Var(Var),
    Alias(ExprAll),
}

impl ASTExpr {
    fn convert(self, defs: &HashMap<Rc<str>, VarOrAlias>) -> ExprAll {
        let mut output: Vec<ExprAll> = Vec::from([self.term0.convert(defs)]);
        let mut input = self.terms.into_iter().map(|(op, expr)| (*op, expr.clone()));
        let mut op_stack: Vec<ASTOpInfix> = Vec::new();

        while let Some((op, expr)) = input.next() {
            while op_stack.last().is_some_and(|it| it.before(op)) {
                let b = output.pop().unwrap();
                let a = output.pop().unwrap();
                output.push(match op_stack.pop().unwrap() {
                    ASTOpInfix::Add => a + b,
                    ASTOpInfix::Sub => a - b,
                    ASTOpInfix::Mul => a * b,
                    ASTOpInfix::Div => ExprAll::Divide(ExDivide::new(a, b)),
                    ASTOpInfix::Pow => ExprAll::Pow(ExPow::new(a, b)),
                });
            }
            op_stack.push(op);
            output.push(expr.convert(defs));
        }
        while let Some(op) = op_stack.pop() {
            let b = output.pop().unwrap();
            let a = output.pop().unwrap();
            output.push(match op {
                ASTOpInfix::Add => a + b,
                ASTOpInfix::Sub => a - b,
                ASTOpInfix::Mul => a * b,
                ASTOpInfix::Div => ExprAll::Divide(ExDivide::new(a, b)),
                ASTOpInfix::Pow => ExprAll::Pow(ExPow::new(a, b)),
            });
        }
        assert_eq!(output.len(), 1);

        output.pop().unwrap()
    }
}
impl ASTTerm {
    fn convert(self, defs: &HashMap<Rc<str>, VarOrAlias>) -> ExprAll {
        let mut value = self.inner.convert(defs);
        for op in self.ops_pre.iter().chain(self.ops_pos.iter()).copied() {
            value = match op {
                ASTOpOutfix::Neg => -value,
                ASTOpOutfix::Conj => todo!("conjugate is not yet implemented"),
            }
        }
        value
    }
}
impl ASTTermInner {
    fn convert(self, defs: &HashMap<Rc<str>, VarOrAlias>) -> ExprAll {
        match self {
            Self::Ident(ident) => match defs.get(&ident) {
                None => todo!("graceful failure: missing var"),
                Some(VarOrAlias::Alias(expr)) => expr.clone(),
                Some(VarOrAlias::Var(var)) => ExprAll::Var(ExVar::new(var.clone())),
            },
            Self::LiteralInt(v) => ExprAll::Const(ExConst::new(Const::Int(v))),
            Self::LiteralFrac(n, d) => ExprAll::Const(ExConst::new(Const::Rational(
                Rational::new(n * d.signum(), d.unsigned_abs()),
            ))),
            Self::Call(name, args) => match (name.as_ref(), args.as_ref()) {
                ("ln", [arg]) => ExprAll::Ln(ExLn::new(arg.clone().convert(defs))),
                ("exp", [arg]) => ExprAll::Exp(ExExp::new(arg.clone().convert(defs))),
                ("ln" | "expr", _) => todo!("graceful failure: incorrect number of args"),
                _ => todo!("graceful failure: unknown command"),
            },
            Self::Expr(expr) => expr.as_ref().clone().convert(defs),
        }
    }
}

const LIBRARY_OF_BABEL: &'static str = include_str!("./babel.txt");

fn exec_stmt(stmt: ASTStmt, defs: &mut HashMap<Rc<str>, VarOrAlias>) -> Option<ExprAll> {
    match stmt {
        ASTStmt::Eval { bindings, expr } => {
            let mut defs_inner = defs.clone();
            for (name, expr) in bindings.iter().cloned() {
                defs_inner.insert(name, VarOrAlias::Alias(expr.convert(defs)));
            }

            let expr = expr.convert(&defs_inner);

            if !expr.has_vars() {
                let value = expr.eval(&HashMap::new());
                println!("= {}", value);
            } else {
                println!("= {}", &expr);
            }

            Some(expr)
        }
        ASTStmt::Alias { name, expr } => match exec_stmt(expr.as_ref().clone(), defs) {
            Some(v) => {
                println!("aliased {} = {}", name.as_ref(), &v);
                defs.insert(name, VarOrAlias::Alias(v));
                None
            }
            None => todo!("graceful failure: alias"),
        },
        ASTStmt::Solve { var, expr } => {
            let (var, defs) = match defs.get(&var) {
                Some(VarOrAlias::Var(v)) => (v.clone(), defs.clone()),
                _ => {
                    let mut x = defs.clone();
                    let v = Var::new(
                        {
                            let i = LIBRARY_OF_BABEL
                                .find(var.as_ref())
                                .expect("could not find string in library of babel");
                            &LIBRARY_OF_BABEL[i..i + var.len()]
                        },
                        false,
                    );
                    x.insert(var, VarOrAlias::Var(v.clone()));
                    (v.clone(), x)
                }
            };
            let sols = Equality::new_expr_eq_0(expr.convert(&defs)).solve_for(var.clone());
            if let Some(sols) = sols {
                if sols.len() == 1 {
                    let sol = sols.first().unwrap().clone();
                    if sol.has_vars() {
                        println!("{} = {}", var.name(), &sol);
                    } else {
                        println!(
                            "{} = {}\n    = {}",
                            var.name(),
                            &sol,
                            sol.eval(&HashMap::new())
                        );
                    }
                    Some(sol)
                } else {
                    println!("{} = [", var.name());
                    for sol in sols {
                        if sol.has_vars() {
                            println!("  {}", sol)
                        } else {
                            println!("  {}\n    = {}", sol, sol.eval(&HashMap::new()))
                        }
                    }
                    println!("]");
                    None
                }
            } else {
                println!("cannot solve");
                None
            }
        }
    }
}
fn repl_do(str: &str, defs: &mut HashMap<Rc<str>, VarOrAlias>) {
    let parsed = match stmt(str) {
        Ok((rest, v)) if rest.trim().is_empty() => v,
        _ => {
            println!("Syntax error");
            return;
        }
    };
    let _ = exec_stmt(parsed, defs);
}
pub fn repl() {
    let stdin = std::io::stdin();
    let mut defs = HashMap::new();
    let mut iter = stdin.lock().lines();
    while let Some(Ok(line)) = iter.next() {
        repl_do(line.as_str(), &mut defs);
    }
}

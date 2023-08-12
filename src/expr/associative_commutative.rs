use std::{rc::Rc, cell::RefCell, collections::hash_map::DefaultHasher, hash::{Hash, Hasher}};

use super::{ExprAll, consts::{Const, ExConst}, ExprScope, sum::ExSum, var::ExVar, Expr, product::ExProduct};


pub trait ExprAssociativeCommuttative {
    fn reduce_consts<I: Iterator<Item = Const>>(consts: I) -> Const;
    fn associates_with(child: ExprAll) -> Result<Vec<ExprAll>, ExprAll>;
    fn children(&self) -> &ChildrenAssociativeCommutative;
}
#[derive(Debug)]
pub struct ChildrenAssociativeCommutative {
    consts_reduced: Rc<ExConst>,
    vars: Box<[Rc<ExVar>]>,
    sums: Box<[Rc<ExSum>]>,
    products: Box<[Rc<ExProduct>]>,
    precalculated_hash: u64,
    precalculated_hash_except_consts: u64,
}
impl ChildrenAssociativeCommutative {
    pub fn get_expralls(&self) -> Vec<ExprAll> {
        [
            self.consts_reduced.exprall(),
        ].into_iter().chain(
            self.vars.iter().map(Expr::exprall)
        ).chain(
            self.sums.iter().map(Expr::exprall)
        ).chain(
            self.products.iter().map(Expr::exprall)
        ).collect()
    }
    pub fn new<T: ExprAssociativeCommuttative>(
        scope: Rc<RefCell<ExprScope>>,
        raw: Vec<ExprAll>,
    ) -> Self {
        let mut consts = vec![];
        let mut vars = vec![];
        let mut sums = vec![];
        let mut products = vec![];
        for ex in raw.into_iter().flat_map(|it| {
            match T::associates_with(it) {
                Ok(sub_children) => sub_children,
                Err(it) => vec![it],
            }
        }) {
            match ex {
                ExprAll::Const(v) => consts.push(v),
                ExprAll::Var(v) => vars.push(v),
                ExprAll::Sum(v) => sums.push(v),
                ExprAll::Product(v) => products.push(v),
            }
        }
        let consts_reduced = ExConst::new(&scope, 
            T::reduce_consts(consts.into_iter().map(|v| v.1))
        );

        let (precalculated_hash, precalculated_hash_except_consts) = Self::precalculate_hashes(&consts_reduced, &vars, &sums, &products);

        Self {
            consts_reduced,
            vars: vars.into(),
            sums: sums.into(),
            products: products.into(),
            precalculated_hash,
            precalculated_hash_except_consts,
        }
    }
    fn precalculate_hashes(
        consts_reduced: &Rc<ExConst>,
        vars: &Vec<Rc<ExVar>>,
        sums: &Vec<Rc<ExSum>>,
        products: &Vec<Rc<ExProduct>>,
    ) -> (u64, u64) {
        let mut hasher = DefaultHasher::new();
        let mut sub_hashes = (
            vars.iter().map(Expr::exprall)
        ).chain(
            sums.iter().map(Expr::exprall)
        ).chain(
            products.iter().map(Expr::exprall)
        )
            .map(|it| it.get_hash())
            .collect::<Vec<_>>();

        sub_hashes.sort(); // erase order for commuttative property
        for hash in sub_hashes {
            hasher.write_u64(hash);
        }

        let hash_except_consts = hasher.finish();

        hasher.write_u64(consts_reduced.exprall().get_hash());
        let hash = hasher.finish();

        (hash, hash_except_consts)
    }
    pub fn get_precalculated_hash(&self, ignore_consts: bool) -> u64 {
        if ignore_consts {
            self.precalculated_hash_except_consts
        } else {
            self.precalculated_hash
        }
    }
}
impl Hash for ChildrenAssociativeCommutative {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.precalculated_hash)
    }
}


#[cfg(test)]
mod test {
    use crate::expr::{ExprScope, consts::{ExConst, Const}, product::ExProduct, var::{ExVar, Var}, Expr, associative_commutative::ExprAssociativeCommuttative};

    #[test]
    fn precalculated_hashes() {
        let scope = ExprScope::new();

        let consts = [
            ExConst::new(&scope, Const(0)),
            ExConst::new(&scope, Const(1)),
            ExConst::new(&scope, Const(2)),
        ];
        let vars = [
            ExVar::new(&scope, Var::new("a")),
            ExVar::new(&scope, Var::new("b")),
        ];

        let products = [
            ExProduct::new(&scope, vec![ // 2 * a
                consts[2].exprall(),
                vars[0].exprall(),
            ]),
            ExProduct::new(&scope, vec![ // 1 * 2 * a
                consts[2].exprall(),
                consts[1].exprall(),
                vars[0].exprall(),
            ]),
            ExProduct::new(&scope, vec![ // 1 * a
                consts[1].exprall(),
                vars[0].exprall(),
            ]),
            ExProduct::new(&scope, vec![ // a
                vars[0].exprall(),
            ]),
            ExProduct::new(&scope, vec![ // b
                vars[1].exprall(),
            ]),
        ];

        assert_eq!( // 2 * a = 1 * 2 * a
            products[0].children().get_precalculated_hash(false),
            products[1].children().get_precalculated_hash(false),
        );
        assert_eq!( // 1 * a = a
            products[2].children().get_precalculated_hash(false),
            products[3].children().get_precalculated_hash(false),
        );
        assert_ne!( // 2 * a != 1 * a
            products[0].children().get_precalculated_hash(false),
            products[2].children().get_precalculated_hash(false),
        );

        assert_eq!( // 2 * a ~ 1 * 2 * a
            products[0].children().get_precalculated_hash(true),
            products[1].children().get_precalculated_hash(true),
        );
        assert_eq!( // 2 * a ~ a
            products[0].children().get_precalculated_hash(true),
            products[2].children().get_precalculated_hash(true),
        );

        assert_ne!( // a != b
            products[3].children().get_precalculated_hash(false),
            products[4].children().get_precalculated_hash(false),
        );
    }
}
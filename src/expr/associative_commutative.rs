use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    rc::Rc,
};

use super::{
    consts::{Const, ExConst},
    Expr, ExprAll,
};

pub trait ExprAssociativeCommuttative {
    fn reduce_consts<I: Iterator<Item = Const>>(consts: I) -> Const;
    fn associates_with(child: ExprAll) -> Result<Vec<ExprAll>, ExprAll>;
    fn children(&self) -> &ChildrenAssociativeCommutative;
}
#[derive(Debug)]
pub struct ChildrenAssociativeCommutative {
    consts_reduced: Rc<ExConst>,
    non_consts: Box<[ExprAll]>,
    precalculated_hash: u64,
    precalculated_hash_except_consts: u64,
}
impl ChildrenAssociativeCommutative {
    pub fn get_expralls(&self) -> Vec<ExprAll> {
        [self.consts_reduced.exprall()]
            .into_iter()
            .chain(self.non_consts.iter().map(Clone::clone))
            .collect()
    }
    pub fn get_expralls_filtering<F: FnOnce(&Const) -> bool>(
        &self,
        exclude_const_if: F,
    ) -> Vec<ExprAll> {
        if self.non_consts.len() > 0 {
            (if exclude_const_if(&self.consts_reduced.1) {
                vec![]
            } else {
                vec![self.consts_reduced.exprall()]
            })
            .into_iter()
            .chain(self.non_consts.iter().map(Clone::clone))
            .collect()
        } else {
            vec![self.consts_reduced.exprall()]
        }
    }
    pub fn new<T: ExprAssociativeCommuttative>(raw: Vec<ExprAll>) -> Self {
        let mut consts = vec![];
        let mut non_consts = vec![];
        for ex in raw.into_iter().flat_map(|it| match T::associates_with(it) {
            Ok(sub_children) => sub_children,
            Err(it) => vec![it],
        }) {
            match ex {
                ExprAll::Const(v) => consts.push(v),
                ex => non_consts.push(ex),
            }
        }
        let consts_reduced = ExConst::new(T::reduce_consts(consts.into_iter().map(|v| v.1)));

        let (precalculated_hash, precalculated_hash_except_consts) =
            Self::precalculate_hashes(&consts_reduced, &non_consts);

        Self {
            consts_reduced,
            non_consts: non_consts.into(),
            precalculated_hash,
            precalculated_hash_except_consts,
        }
    }
    fn precalculate_hashes(consts_reduced: &Rc<ExConst>, non_consts: &Vec<ExprAll>) -> (u64, u64) {
        let mut hasher = DefaultHasher::new();
        let mut sub_hashes = non_consts
            .iter()
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
    use crate::expr::{
        associative_commutative::ExprAssociativeCommuttative,
        consts::{Const, ExConst},
        product::ExProduct,
        var::{ExVar, Var},
        Expr,
    };

    #[test]
    fn precalculated_hashes() {
        let consts = [
            ExConst::new(Const(0)),
            ExConst::new(Const(1)),
            ExConst::new(Const(2)),
        ];
        let vars = [ExVar::new(Var::new("a")), ExVar::new(Var::new("b"))];

        let products = [
            ExProduct::new(vec![
                // 2 * a
                consts[2].exprall(),
                vars[0].exprall(),
            ]),
            ExProduct::new(vec![
                // 1 * 2 * a
                consts[2].exprall(),
                consts[1].exprall(),
                vars[0].exprall(),
            ]),
            ExProduct::new(vec![
                // 1 * a
                consts[1].exprall(),
                vars[0].exprall(),
            ]),
            ExProduct::new(vec![
                // a
                vars[0].exprall(),
            ]),
            ExProduct::new(vec![
                // b
                vars[1].exprall(),
            ]),
        ];

        assert_eq!(
            // 2 * a = 1 * 2 * a
            products[0].children().get_precalculated_hash(false),
            products[1].children().get_precalculated_hash(false),
        );
        assert_eq!(
            // 1 * a = a
            products[2].children().get_precalculated_hash(false),
            products[3].children().get_precalculated_hash(false),
        );
        assert_ne!(
            // 2 * a != 1 * a
            products[0].children().get_precalculated_hash(false),
            products[2].children().get_precalculated_hash(false),
        );

        assert_eq!(
            // 2 * a ~ 1 * 2 * a
            products[0].children().get_precalculated_hash(true),
            products[1].children().get_precalculated_hash(true),
        );
        assert_eq!(
            // 2 * a ~ a
            products[0].children().get_precalculated_hash(true),
            products[2].children().get_precalculated_hash(true),
        );

        assert_ne!(
            // a != b
            products[3].children().get_precalculated_hash(false),
            products[4].children().get_precalculated_hash(false),
        );
    }
}

use std::{
    cell::RefCell,
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{Hash, Hasher},
    rc::Rc,
};

#[derive(Debug)]
struct ExprScope {
    sums: HashMap<Id, Rc<ExSum>>,
    consts: HashMap<Id, Rc<ExConst>>,
    vars: HashMap<Id, Rc<ExVar>>,
}

impl ExprScope {
    fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            sums: HashMap::new(),
            consts: HashMap::new(),
            vars: HashMap::new(),
        }))
    }
}

#[derive(Debug, Clone, Copy, Eq)]
struct Var(&'static str, u64);
impl Var {
    fn new(name: &'static str) -> Self {
        Var(name, rand::random())
    }
}
impl PartialEq for Var {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}
impl Hash for Var {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(0xeb42_aa23_948b_300f);
        state.write_u64(self.1);
    }
}
struct VarValues(HashMap<Var, Const>);
impl VarValues {
    fn set(&mut self, var: Var, val: Const) {
        self.0.insert(var, val);
    }
    fn new() -> Self {
        Self(HashMap::new())
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct Id {
    pub content_hash: u64,
}

trait Expr {
    fn eval(&self, vars: &VarValues) -> Const;
    fn exprall(self: &Rc<Self>) -> ExprAll;
}

#[derive(Debug, Clone, Copy, Hash)]
struct Const(i32);
impl std::ops::Add for Const {
    type Output = Const;
    fn add(mut self, rhs: Self) -> Self::Output {
        self.0 += rhs.0;
        self
    }
}
impl std::iter::Sum<Const> for Const {
    fn sum<I: Iterator<Item = Const>>(iter: I) -> Self {
        let mut sum = Const(0);
        for ent in iter {
            sum = sum + ent;
        }
        sum
    }
}

#[derive(Debug)]
struct ExConst(Id, Const);
impl ExConst {
    fn new(scope: &Rc<RefCell<ExprScope>>, c: Const) -> Rc<Self> {
        let content_hash = {
            let mut h = DefaultHasher::new();
            c.hash(&mut h);
            h.finish()
        };
        let id = Id { content_hash };
        scope
            .borrow_mut()
            .consts
            .entry(id)
            .or_insert_with(|| Rc::new(Self(id, c)))
            .clone()
    }
}
impl Expr for ExConst {
    fn eval(&self, _vars: &VarValues) -> Const {
        self.1
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        ExprAll::Const(self.clone())
    }
}
impl PartialEq for ExConst {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[derive(Debug)]
struct ExSum(Id, ChildrenAssociativeCommutative);
impl ExSum {
    fn new(scope: &Rc<RefCell<ExprScope>>, children: Vec<ExprAll>) -> Rc<Self> {
        let children = ChildrenAssociativeCommutative::new::<Self>(scope.clone(), children);
        let content_hash = {
            let mut h = DefaultHasher::new();
            children.hash(&mut h);
            h.finish()
        };
        let id = Id { content_hash };
        scope
            .borrow_mut()
            .sums
            .entry(id)
            .or_insert_with(|| Rc::new(Self(id, children)))
            .clone()
    }
}
impl Expr for ExSum {
    fn eval(&self, vars: &VarValues) -> Const {
        self.1.get_expralls().into_iter().map(|v|v.eval(vars)).sum()
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        ExprAll::Sum(self.clone())
    }
}
impl ExprAssociativeCommuttative for ExSum {
    fn reduce_consts<I: Iterator<Item = Const>>(consts: I) -> Const {
        consts.sum()
    }
    fn associates_with(child: ExprAll) -> Result<Vec<ExprAll>, ExprAll> {
        match child {
            ExprAll::Sum(sum) => Ok(sum.1.get_expralls()),
            child => Err(child),
        }
    }
}
impl PartialEq for ExSum {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}



#[derive(Debug)]
struct ExVar(Id, Var);
impl ExVar {
    fn new(scope: &Rc<RefCell<ExprScope>>, var: Var) -> Rc<Self> {
        let content_hash = var.1;
        let id = Id { content_hash };
        scope
            .borrow_mut()
            .vars
            .entry(id)
            .or_insert_with(|| Rc::new(Self(id, var)))
            .clone()
    }
}
impl Expr for ExVar {
    fn eval(&self, vars: &VarValues) -> Const {
        *vars.0.get(&self.1).expect("eval variable unassigned")
    }
    fn exprall(self: &Rc<Self>) -> ExprAll {
        ExprAll::Var(self.clone())
    }
}
impl ExprAssociativeCommuttative for ExVar {
    fn reduce_consts<I: Iterator<Item = Const>>(consts: I) -> Const {
        consts.sum()
    }
    fn associates_with(child: ExprAll) -> Result<Vec<ExprAll>, ExprAll> {
        match child {
            ExprAll::Sum(sum) => Ok(sum.1.get_expralls()),
            child => Err(child),
        }
    }
}
impl PartialEq for ExVar {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}


trait ExprAssociativeCommuttative {
    fn reduce_consts<I: Iterator<Item = Const>>(consts: I) -> Const;
    fn associates_with(child: ExprAll) -> Result<Vec<ExprAll>, ExprAll>;
}
#[derive(Debug)]
struct ChildrenAssociativeCommutative {
    scope: Rc<RefCell<ExprScope>>,
    consts_reduced: Const,
    sums: Box<[Rc<ExSum>]>,
    vars: Box<[Rc<ExVar>]>,
    hash_except_consts: u64,
}
impl ChildrenAssociativeCommutative {
    fn get_expralls(&self) -> Vec<ExprAll> {
        [
            ExConst::new(
                &self.scope,
                self.consts_reduced.clone(),
            ).exprall(),
        ].into_iter().chain(
            self.sums.iter().map(Expr::exprall)
        ).chain(
            self.vars.iter().map(Expr::exprall)
        ).collect()
    }
    fn new<T: ExprAssociativeCommuttative>(
        scope: Rc<RefCell<ExprScope>>,
        raw: Vec<ExprAll>,
    ) -> Self {
        let mut consts = vec![];
        let mut sums = vec![];
        let mut vars = vec![];
        for ex in raw.into_iter().flat_map(|it| {
            match T::associates_with(it) {
                Ok(sub_children) => sub_children,
                Err(it) => vec![it],
            }
        }) {
            match ex {
                ExprAll::Const(const_val) => consts.push(const_val),
                ExprAll::Sum(sum) => sums.push(sum),
                ExprAll::Var(var) => vars.push(var),
            }
        }
        let hash_except_consts = {
            let mut hasher = DefaultHasher::new();
            let mut sub_hashes = (
                sums.iter().map(Expr::exprall)
            ).chain(
                vars.iter().map(Expr::exprall)
            ).into_iter()
                .map(|it| it.get_hash())
                .collect::<Vec<_>>();
            sub_hashes.sort(); // erase order for commuttative property

            for hash in sub_hashes {
                hasher.write_u64(hash);
            }
            hasher.finish()
        };
        Self {
            scope,
            consts_reduced: T::reduce_consts(consts.into_iter().map(|v| v.1)),
            sums: sums.into(),
            vars: vars.into(),
            hash_except_consts,
        }
    }
}
impl Hash for ChildrenAssociativeCommutative {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut sub_hashes = self
            .get_expralls()
            .into_iter()
            .map(|it| it.get_hash())
            .collect::<Vec<_>>();
        sub_hashes.sort(); // erase order for commuttative property

        for hash in sub_hashes {
            state.write_u64(hash);
        }
    }
}
enum ExprAll {
    Const(Rc<ExConst>),
    Sum(Rc<ExSum>),
    Var(Rc<ExVar>),
}
impl ExprAll {
    fn eval(&self,vars: &VarValues) -> Const {
        match self {
            Self::Const(v) => v.eval(vars),
            Self::Sum(v) => v.eval(vars),
            Self::Var(v) => v.eval(vars),
        }
    }
    fn get_hash(&self) -> u64 {
        match self {
            Self::Const(v) => v.0.content_hash ^ 0xd674_0330_30c0_0c40, // hardcoded noise to avoid hash collisions
            Self::Sum(v) => v.0.content_hash ^ 0x0992_3158_b088_c199,
            Self::Var(v) => v.0.content_hash ^ 0x36a7_4d41_2258_3d0e,
            // 0xadc8_cd4a_57ea_2881
            // 0x676c_ec63_7c5e_d41a
            // 0xc684_0800_00c5_b600
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

fn main() {
    let scope = ExprScope::new();
    
    let var_a = Var::new("a");

    let a = ExConst::new(&scope, Const(3));
    let b = ExConst::new(&scope, Const(6));
    let a_ = ExVar::new(&scope, var_a);

    let a_plus_b = ExSum::new(&scope, vec![
        a.exprall(),
        b.exprall(),
        a_.exprall(),
        a_.exprall(),
    ]);

    let mut vars = VarValues::new();
    vars.set(var_a, Const(2));

    println!("{:?}", a_plus_b.eval(&vars));
}


#[test]
fn exconst_hash_equality() {
    let scope = ExprScope::new();

    let consts = [
        ExConst::new(&scope, Const(1)),
        ExConst::new(&scope, Const(1)),
        ExConst::new(&scope, Const(5)),
    ];

    assert_eq!(consts[0], consts[1]);
    assert_ne!(consts[0], consts[2]);
    assert_ne!(consts[1], consts[2]);
}

#[test]
fn exsum_hash_equality() {
    let scope = ExprScope::new();

    let consts = [
        ExConst::new(&scope, Const(1)),
        ExConst::new(&scope, Const(1)),
        ExConst::new(&scope, Const(5)),
    ];

    let sums = [
        ExSum::new(&scope, vec![
            ExprAll::Const(consts[0].clone()),
            ExprAll::Const(consts[0].clone()),
        ]),
        ExSum::new(&scope, vec![
            ExprAll::Const(consts[0].clone()),
            ExprAll::Const(consts[1].clone()),
        ]),
        ExSum::new(&scope, vec![
            ExprAll::Const(consts[2].clone()),
            ExprAll::Const(consts[1].clone()),
        ]),
        ExSum::new(&scope, vec![
            ExprAll::Const(consts[0].clone()),
            ExprAll::Const(consts[2].clone()),
        ]),
        ExSum::new(&scope, vec![
            ExprAll::Const(consts[0].clone()),
            ExprAll::Const(consts[2].clone()),
            ExprAll::Const(consts[2].clone()),
        ]),
    ];

    assert_eq!(sums[0], sums[1]);
    assert_eq!(sums[2], sums[3]);

    assert_ne!(sums[0], sums[2]);
    assert_ne!(sums[0], sums[3]);
    assert_ne!(sums[1], sums[3]);

    assert_ne!(sums[3], sums[4]);
}



#[test]
fn exvar_hash_equality() {
    let scope = ExprScope::new();

    let a = Var::new("a");
    let b = Var::new("b");
    let vars = [
        ExVar::new(&scope, a),
        ExVar::new(&scope, a),
        ExVar::new(&scope, b),
    ];

    assert_eq!(vars[0], vars[1]);
    assert_ne!(vars[0], vars[2]);
    assert_ne!(vars[1], vars[2]);
}

use std::{fmt, hash, ops};

use crate::util::math::{gcd, nth_root_if_integer};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Rational {
    num: i128,
    den: u128,
}
impl Rational {
    pub const ONE: Rational = Rational::new(1, 1);
    pub const ZERO: Rational = Rational::new(0, 1);

    pub const fn new(num: i128, den: u128) -> Self {
        let gcf = gcd(num.unsigned_abs(), den);
        Self {
            num: num / gcf as i128, // safe because the gcd can never be higher than either input
            den: den / gcf,
        }
    }

    fn f64(&self) -> f64 {
        self.num as f64 / self.den as f64
    }

    fn pow(self, exp: Self) -> Option<Self> {
        if exp.num > u32::MAX as i128 || exp.den > u32::MAX as u128 {
            return None; // no way it's rational and in range lmao (plus we can't handle numbers that big anyway)
        }
        if exp.den % 2 == 0 && self.num < 0 {
            return None; // even roots of negative numbers are undefined
        }
        let num = (nth_root_if_integer(self.num.unsigned_abs(), exp.den as u32)? as i128
            * self.num.signum())
        .checked_pow(exp.num as u32)?;
        let den = nth_root_if_integer(self.den, exp.den as u32)?.checked_pow(exp.num as u32)?;
        Some(Self::new(num, den))
    }
    fn pow_int(self, exp: i32) -> Self {
        if exp < 0 {
            self.reciprocol().pow_int(-exp)
        } else {
            Rational::new(self.num.pow(exp as u32), self.den.pow(exp as u32))
        }
    }

    fn reciprocol(self) -> Self {
        Rational::new(
            self.den as i128 * self.num.signum(),
            self.num.unsigned_abs(),
        )
    }

    fn abs(&self) -> Self {
        Rational::new(self.num.abs(), self.den)
    }

    fn is_zero(&self) -> bool {
        self.num == 0
    }
    fn is_one(&self) -> bool {
        self.num == 1 && self.den == 1
    }
    fn is_negative_one(&self) -> bool {
        self.num == 1 && self.den == 1
    }
}

impl ops::Mul for Rational {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        Rational::new(self.num * rhs.num, self.den * rhs.den)
    }
}
impl ops::Div for Rational {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        Rational::new(
            self.num * rhs.den as i128 * rhs.num.signum(),
            self.den * rhs.num.unsigned_abs(),
        )
    }
}
impl ops::Mul<i128> for Rational {
    type Output = Self;
    fn mul(self, rhs: i128) -> Self::Output {
        Rational::new(self.num * rhs, self.den)
    }
}
impl ops::Add for Rational {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        let gcd = gcd(self.den, rhs.den);
        Self::new(
            self.num * (rhs.den / gcd) as i128 + rhs.num * (self.den / gcd) as i128,
            self.den / gcd * rhs.den,
        )
    }
}
impl ops::Sub for Rational {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        self + rhs * -1
    }
}
impl fmt::Display for Rational {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.den == 1 {
            write!(f, "{}", self.num)
        } else if self.num >= 0 {
            write!(f, "\\frac{{{}}}{{{}}}", self.num, self.den)
        } else {
            write!(f, "-\\frac{{{}}}{{{}}}", self.num.unsigned_abs(), self.den)
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct RationalComplex {
    real: Rational,
    imag: Rational,
}
impl RationalComplex {
    pub fn new(real: Rational, imag: Rational) -> Self {
        Self { real, imag }
    }
    pub fn pow(self, other: Self) -> Option<RationalComplex> {
        if self.is_zero() {
            if other.is_zero() {
                return None;
            } else {
                return Some(RationalComplex {
                    real: Rational::ZERO,
                    imag: Rational::ZERO,
                });
            }
        } else {
            if other.is_zero() {
                return Some(RationalComplex {
                    real: Rational::ONE,
                    imag: Rational::ZERO,
                });
            }
        }
        if self.imag.is_zero() && other.imag.is_zero() {
            return Some(RationalComplex {
                real: self.real.pow(other.real)?,
                imag: Rational::ZERO,
            });
        } else {
            return None; // too lazy
        }
    }
    pub fn is_zero(&self) -> bool {
        self.real.is_zero() && self.imag.is_zero()
    }
    pub fn is_one(&self) -> bool {
        self.real.is_one() && self.imag.is_zero()
    }
    pub fn conjugate(&self) -> Self {
        RationalComplex {
            real: self.real,
            imag: self.imag * -1,
        }
    }
}
impl ops::Add for RationalComplex {
    type Output = RationalComplex;
    fn add(self, rhs: Self) -> Self::Output {
        RationalComplex {
            real: self.real + rhs.real,
            imag: self.imag + rhs.imag,
        }
    }
}
impl ops::Mul for RationalComplex {
    type Output = RationalComplex;
    fn mul(self, rhs: Self) -> Self::Output {
        RationalComplex {
            real: self.real * rhs.real - self.imag * rhs.imag,
            imag: self.real * rhs.imag + self.imag * rhs.real,
        }
    }
}
impl ops::Div for RationalComplex {
    type Output = RationalComplex;
    fn div(self, rhs: Self) -> Self::Output {
        // Calculate the conjugate of the denominator
        let den_conj = rhs.conjugate();

        // Multiply the numerator and denominator by the conjugate of the denominator
        let new_real1 = self.real * den_conj.real - self.imag * den_conj.imag;
        let new_imag1 = self.real * den_conj.imag + self.imag * den_conj.real;

        let new_real2 = rhs.real * den_conj.real - rhs.imag * den_conj.imag;

        // Calculate the new real and imaginary parts of the result
        let result_real = new_real1 / new_real2;
        let result_imag = new_imag1 / new_real2;

        // Return the result as a complex number
        RationalComplex {
            real: result_real,
            imag: result_imag,
        }
    }
}
impl fmt::Display for RationalComplex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.imag.is_zero() {
            write!(f, "{}", self.real)
        } else if self.real.is_zero() {
            if self.imag.is_one() {
                write!(f, "i")
            } else if self.imag.is_negative_one() {
                write!(f, "-i")
            } else {
                write!(f, "{} i", self.imag)
            }
        } else {
            if self.imag.is_one() {
                write!(f, "{} + i", self.real)
            } else if self.imag.is_negative_one() {
                write!(f, "{} - i", self.real)
            } else if self.imag.num > 0 {
                write!(f, "{} + {} i", self.real, self.imag)
            } else {
                write!(f, "{} - {} i", self.real, self.imag.abs())
            }
        }
    }
}

pub const ONE: Const = Const::Int(1);
pub const ZERO: Const = Const::Int(0);
pub const NEG_ONE: Const = Const::Int(-1);
pub const HALF: Const = Const::Rational(Rational::new(1, 2));
pub const TWO: Const = Const::Int(2);
pub const I: Const = Const::RationalComplex(RationalComplex {
    real: Rational::ZERO,
    imag: Rational::ONE,
});

#[derive(Debug, Clone, Copy)]
pub enum Const {
    Int(i128),
    Rational(Rational),
    RationalComplex(RationalComplex),
}
impl Const {
    fn reduce(self) -> ConstReduced {
        match self {
            Const::Int(v) => ConstReduced::Int(v),
            Self::Rational(v) => {
                if v.den == 1 {
                    ConstReduced::Int(v.num)
                } else {
                    ConstReduced::Rational(v)
                }
            }
            Self::RationalComplex(v) => {
                if v.imag.is_zero() {
                    if v.real.den == 1 {
                        ConstReduced::Int(v.real.num)
                    } else {
                        ConstReduced::Rational(v.real)
                    }
                } else {
                    ConstReduced::RationalComplex(v)
                }
            }
        }
    }
    pub fn complex64(&self) -> num_complex::Complex64 {
        match self {
            Self::Int(v) => num_complex::Complex64::new(*v as f64, 0.0),
            Self::Rational(v) => num_complex::Complex64::new(v.f64(), 0.0),
            Self::RationalComplex(v) => num_complex::Complex64::new(v.real.f64(), v.imag.f64()),
        }
    }
    pub fn is_zero(&self) -> bool {
        match self {
            Self::Int(v) => *v == 0,
            Self::Rational(v) => v.is_zero(),
            Self::RationalComplex(v) => v.is_zero(),
        }
    }
    pub fn is_one(&self) -> bool {
        match self {
            Self::Int(v) => *v == 1,
            Self::Rational(v) => v.is_one(),
            Self::RationalComplex(v) => v.is_one(),
        }
    }
    pub fn is_complex(&self) -> bool {
        match self {
            Self::RationalComplex(v) => !v.real.is_zero() && !v.imag.is_zero(),
            _ => false,
        }
    }
    fn upgrade_to_match(self, other: Self) -> (Self, Self) {
        match self {
            Self::Int(a) => match other {
                Self::Int(b) => (Self::Int(a), Self::Int(b)),
                Self::Rational(b) => (Self::Rational(Rational::new(a, 1)), Self::Rational(b)),
                Self::RationalComplex(b) => (
                    Self::RationalComplex(RationalComplex {
                        real: Rational::new(a, 1),
                        imag: Rational::ZERO,
                    }),
                    Self::RationalComplex(b),
                ),
            },
            Self::Rational(a) => match other {
                Self::Int(b) => (Self::Rational(a), Self::Rational(Rational::new(b, 1))),
                Self::Rational(b) => (Self::Rational(a), Self::Rational(b)),
                Self::RationalComplex(b) => (
                    Self::RationalComplex(RationalComplex {
                        real: a,
                        imag: Rational::ZERO,
                    }),
                    Self::RationalComplex(b),
                ),
            },
            Self::RationalComplex(a) => match other {
                Self::Int(b) => (
                    Self::RationalComplex(a),
                    Self::RationalComplex(RationalComplex {
                        real: Rational::new(b, 1),
                        imag: Rational::ZERO,
                    }),
                ),
                Self::Rational(b) => (
                    Self::RationalComplex(a),
                    Self::RationalComplex(RationalComplex {
                        real: b,
                        imag: Rational::ZERO,
                    }),
                ),
                Self::RationalComplex(b) => (Self::RationalComplex(a), Self::RationalComplex(b)),
            },
        }
    }
    pub fn pow(self, n: i32) -> Self {
        if n == 0 && self.is_zero() {
            todo!("handle indeterminate cases (here it's 0 ^ 0)");
        }
        if n == 0 {
            return ONE;
        }
        if self.is_zero() {
            return ZERO;
        }
        match self {
            Self::Int(x) => Self::Rational(Rational::new(x, 1).pow_int(n)),
            Self::Rational(x) => Self::Rational(x.pow_int(n)),
            Self::RationalComplex(x) => Self::RationalComplex(
                x.pow(RationalComplex::new(
                    Rational::new(n as i128, 1),
                    Rational::ZERO,
                ))
                .unwrap_or_else(|| todo!("handle intermediate cases where it's irrational")),
            ),
        }
    }
}
impl ops::Add for Const {
    type Output = Const;
    fn add(self, rhs: Self) -> Self::Output {
        match self.upgrade_to_match(rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a + b),
            (Self::Rational(a), Self::Rational(b)) => Self::Rational(a + b),
            (Self::RationalComplex(a), Self::RationalComplex(b)) => Self::RationalComplex(a + b),
            _ => panic!("impossible"),
        }
    }
}
impl ops::Mul for Const {
    type Output = Const;
    fn mul(self, rhs: Self) -> Self::Output {
        match self.upgrade_to_match(rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a * b),
            (Self::Rational(a), Self::Rational(b)) => Self::Rational(a * b),
            (Self::RationalComplex(a), Self::RationalComplex(b)) => Self::RationalComplex(a * b),
            _ => panic!("impossible"),
        }
    }
}
impl ops::Div for Const {
    type Output = Const;
    fn div(self, rhs: Self) -> Self::Output {
        match self.upgrade_to_match(rhs) {
            (Self::Int(a), Self::Int(b)) => {
                Self::Rational(Rational::new(a * b.signum(), b.unsigned_abs()))
            }
            (Self::Rational(a), Self::Rational(b)) => Self::Rational(a / b),
            (Self::RationalComplex(a), Self::RationalComplex(b)) => Self::RationalComplex(a / b),
            _ => panic!("impossible"),
        }
    }
}
impl std::iter::Sum<Const> for Const {
    fn sum<I: Iterator<Item = Const>>(iter: I) -> Self {
        let mut sum = ZERO;
        for ent in iter {
            sum = sum + ent;
        }
        sum
    }
}
impl std::iter::Product<Const> for Const {
    fn product<I: Iterator<Item = Const>>(iter: I) -> Self {
        let mut prod = ONE;
        for ent in iter {
            prod = prod * ent;
        }
        prod
    }
}

impl hash::Hash for Const {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.reduce().hash(state);
    }
}
impl PartialEq for Const {
    fn eq(&self, other: &Self) -> bool {
        self.reduce() == other.reduce()
    }
}
impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Const::Int(v) => write!(f, "{}", v),
            Const::Rational(v) => write!(f, "{}", v),
            Const::RationalComplex(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum ConstReduced {
    Int(i128),
    Rational(Rational),
    RationalComplex(RationalComplex),
}

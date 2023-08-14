
/// GCD by euclid's agorithm (const functions are pain)
pub const fn gcd(a: u128, b: u128) -> u128 {
    if a == 0 || b == 0 {
        return if a > b { a } else { b }; // return max (max is unstable as const fn blah blah
    }
    let mut x = (a, b); 
    if x.0 > x.1 {
        x = (x.1, x.0); // (lower, higher)
    }

    while x.0 > 0 {
        x = (x.1 % x.0, x.0);
    }
    return x.1;
}

pub fn nth_root_if_integer(base: u128, n: u32) -> Option<u128> {
    if n == 0 {
        return None; // mathematical nonsense
    }
    if n == 1 {
        return Some(base); // trivial
    }

    let mut guess = base;
    let mut prev_guess = 0;

    while guess != prev_guess {
        prev_guess = guess;
        guess = ((n - 1) as u128 * guess + base / u128::pow(guess, n - 1)) / n as u128;
    }

    if u128::pow(guess, n) == base {
        Some(guess)
    } else {
        None
    }
}

#[cfg(test)]
mod test {
    use crate::util::math::{gcd, nth_root_if_integer};

    #[test]
    fn test_gcd() {
        for i in 1..5 {
            assert_eq!(gcd(0, i), i);
            assert_eq!(gcd(1, i), 1);
            assert_eq!(gcd(i, 0), i);
            assert_eq!(gcd(i, 1), 1);
        }
        const PRIMES: [u128; 10] = [2, 3, 5, 13, 61, 67, 71, 79, 89, 97];
        for a in PRIMES {
            for b in PRIMES {
                for c in PRIMES {
                    if a == b || b == c || c == a {
                        continue;
                    }
                    assert_eq!(gcd(a*c, b*c), c);
                }
            }
        }
    }

    #[test]
    fn test_nth_root_if_integer() {
        assert_eq!(nth_root_if_integer(100, 2), Some(10));
        assert_eq!(nth_root_if_integer(101, 2), None);
        assert_eq!(nth_root_if_integer(10000, 2), Some(100));
        assert_eq!(nth_root_if_integer(10000, 4), Some(10));
        assert_eq!(nth_root_if_integer(10000, 3), None);
        assert_eq!(nth_root_if_integer(625, 2), Some(25));
        assert_eq!(nth_root_if_integer(625, 3), None);
        assert_eq!(nth_root_if_integer(1381, 1), Some(1381));
    }
}

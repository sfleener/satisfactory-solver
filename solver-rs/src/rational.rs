use num::{FromPrimitive, Zero};
use std::fmt::{Debug, Formatter};
use std::ops::{Add, Div, Mul, Sub};

#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Rational(num::rational::Rational64);

impl Rational {
    pub const ZERO: Self = Self(num::rational::Rational64::ZERO);

    pub fn is_zero(&self) -> bool {
        self.0.is_zero()
    }

    pub fn reduced(&self) -> Rational {
        Rational(self.0.reduced())
    }
}

impl From<f64> for Rational {
    fn from(value: f64) -> Self {
        fn is_near(v: f64, expected: f64) -> bool {
            const EPSILON: f64 = 1e-5;
            v <= expected + EPSILON && v >= expected - EPSILON
        }

        fn is_zero(v: f64) -> bool {
            is_near(v, 0.0)
        }

        if is_zero(value) {
            return Self(num::rational::Rational64::ZERO);
        }

        let raw = num::rational::Rational64::from_f64(value)
            .unwrap()
            .reduced();
        let numer = *raw.numer();
        let denom = *raw.denom();
        let whole = numer / denom;
        let fractional = numer % denom;
        let fractional = fractional as f64 / denom as f64;

        if is_zero(fractional) {
            return Self(num::rational::Rational64::new(whole, 1));
        }

        for i in 1i64..=20 {
            for j in 1..=i {
                if is_near(fractional, j as f64 / i as f64) {
                    return Self(num::rational::Rational64::new((whole * i) + j, i));
                }
            }
        }

        panic!("Irreducable float: {value}");
    }
}

impl Debug for Rational {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let this = self.0.reduced();
        if *this.denom() == 1 {
            Debug::fmt(this.numer(), f)
        } else {
            let num = *this.numer();
            let den = *this.denom();

            let whole = num / den;
            if whole != 0 {
                write!(f, "{} {}/{}", whole, num % den, den)
            } else {
                write!(f, "{}/{}", num, den)
            }
        }
    }
}

impl Add<Rational> for Rational {
    type Output = Rational;

    fn add(self, rhs: Rational) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl Sub<Rational> for Rational {
    type Output = Rational;

    fn sub(self, rhs: Rational) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl Mul<Rational> for Rational {
    type Output = Rational;

    fn mul(self, rhs: Rational) -> Self::Output {
        Self(self.0 * rhs.0)
    }
}

impl Div<Rational> for Rational {
    type Output = Rational;

    fn div(self, rhs: Rational) -> Self::Output {
        Self(self.0 / rhs.0)
    }
}

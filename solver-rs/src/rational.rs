use crate::rational::units::{Items, Minute, One, Per, Recipes, Second, Unitless};
use num::rational::Rational64 as RawRat;
use num::{FromPrimitive, Zero};
use serde::de::Error;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};
use std::iter::Sum;
use std::marker::PhantomData as Boo;
use std::ops::{Add, AddAssign, Div, Mul, Sub};

pub mod units {
    use std::any::type_name;
    use std::fmt::{Debug, Display, Formatter};
    use std::marker::PhantomData;
    use derive_more::Display;

    pub trait Unit {
        fn new() -> Self;
    }

    impl<T> Unit for T
    where
        T: Default,
    {
        fn new() -> Self {
            Self::default()
        }
    }

    #[derive(Default, Copy, Debug, Display, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
    pub struct Unitless;

    #[derive(Default, Copy, Debug, Display, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
    pub struct Points;
    #[derive(Default, Copy, Debug, Display, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
    pub struct Megawatts;

    #[derive(Default, Copy, Debug, Display, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
    pub struct Items;
    #[derive(Default, Copy, Debug, Display, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
    pub struct Recipes;
    #[derive(Default, Copy, Debug, Display, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
    pub struct Machines;

    #[derive(Default, Copy, Debug, Display, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
    pub struct Second;
    #[derive(Default, Copy, Debug, Display, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
    pub struct Minute;

    #[derive(Default, Copy, Debug, Display, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
    pub struct One;

    #[derive(Default, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
    pub struct Per<N, D>(PhantomData<fn() -> N>, PhantomData<fn() -> D>);

    impl<N, D> Per<N, D> {
        pub fn new() -> Self {
            Self(PhantomData, PhantomData)
        }
    }

    impl<N, D> Debug for Per<N, D> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}/{}", type_name::<N>(), type_name::<D>())
        }
    }

    impl<N, D> Display for Per<N, D> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}/{}", type_name::<N>(), type_name::<D>())
        }
    }
}

pub type ItemsPerMinute = Rat<Per<Items, Minute>>;
pub type ItemsPerMinutePerRecipe = Rat<Per<Per<Items, Minute>, Recipes>>;

#[derive(Copy, Clone)]
pub struct Rat<Unit>(RawRat, Boo<fn() -> Unit>);

impl<Unit> Rat<Unit> {
    pub const ZERO: Self = Self(RawRat::ZERO, Boo);
    pub const ONE: Self = Self(RawRat::ONE, Boo);

    pub const fn new_unchecked(numer: i64, denom: i64) -> Self {
        Self(RawRat::new_raw(numer, denom), Boo)
    }

    pub fn new(numer: i64, denom: i64) -> Self {
        Self(RawRat::new(numer, denom), Boo)
    }

    pub fn whole(numer: i64) -> Self {
        Self(RawRat::from_integer(numer), Boo)
    }

    pub fn is_zero(&self) -> bool {
        self.0.is_zero()
    }

    pub fn is_near_zero(&self) -> bool {
        let epsilon = Self::new(1, 100);
        *self <= epsilon
    }

    pub fn reduced(&self) -> Rat<Unit> {
        Rat(self.0.reduced(), Boo)
    }

    pub fn as_f64(&self) -> f64 {
        self.into()
    }
}

impl<U> PartialEq for Rat<U> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<U> Eq for Rat<U> {}

impl<U> PartialOrd for Rat<U> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<U> Ord for Rat<U> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

impl<Unit> Serialize for Rat<Unit> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&format!("{}/{}", self.0.numer(), self.0.denom()))
    }
}

impl<'de, Unit> Deserialize<'de> for Rat<Unit> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum RationalKind {
            Float(f64),
            Str(String),
        }

        match RationalKind::deserialize(deserializer)? {
            RationalKind::Float(f) => Ok(f.into()),
            RationalKind::Str(s) => {
                let Some((numer, denom)) = s.split_once('/') else {
                    return Err(D::Error::custom(format!("Invalid fraction format: {s}")));
                };

                Ok(Self::new(
                    numer
                        .parse()
                        .map_err(|e| D::Error::custom(format!("{e:?}")))?,
                    denom
                        .parse()
                        .map_err(|e| D::Error::custom(format!("{e:?}")))?,
                ))
            }
        }
    }
}

impl<Unit> From<&Rat<Unit>> for f64 {
    fn from(value: &Rat<Unit>) -> Self {
        *value.0.numer() as f64 / *value.0.denom() as f64
    }
}

impl<Unit> From<Rat<Unit>> for f64 {
    fn from(value: Rat<Unit>) -> Self {
        *value.0.numer() as f64 / *value.0.denom() as f64
    }
}

impl<Unit> From<i64> for Rat<Unit> {
    fn from(value: i64) -> Self {
        Rat::whole(value)
    }
}

impl<Unit> From<f64> for Rat<Unit> {
    fn from(value: f64) -> Self {
        fn is_near(v: f64, expected: f64) -> bool {
            const EPSILON: f64 = 1e-5;
            v <= expected + EPSILON && v >= expected - EPSILON
        }

        fn is_zero(v: f64) -> bool {
            is_near(v, 0.0)
        }

        if is_zero(value) {
            return Self(RawRat::ZERO, Boo);
        }

        let raw = RawRat::from_f64(value).unwrap().reduced();
        let numer = *raw.numer();
        let denom = *raw.denom();
        let whole = numer / denom;
        let fractional = numer % denom;
        let fractional = fractional as f64 / denom as f64;

        if is_zero(fractional) {
            return Self(RawRat::new(whole, 1), Boo);
        }

        for i in 1i64..=10000 {
            for j in 1..=i {
                if is_near(fractional, j as f64 / i as f64) {
                    return Self(RawRat::new((whole * i) + j, i), Boo);
                }
            }
        }

        panic!("Irreducable float: {value}");
    }
}

impl<Unit> Debug for Rat<Unit> where Unit: units::Unit + Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // Debug::fmt(&self.0, f)
        Display::fmt(&self, f)
    }
}

impl<Unit> Display for Rat<Unit>
where
    Unit: units::Unit + Display,
{
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
                write!(f, "{num}/{den}")
            }
        }
    }
}

impl<U> Add<Rat<U>> for Rat<U> {
    type Output = Rat<U>;

    fn add(self, rhs: Rat<U>) -> Self::Output {
        Self(self.0 + rhs.0, Boo)
    }
}

impl<U: Copy> AddAssign<Rat<U>> for Rat<U> {
    fn add_assign(&mut self, rhs: Rat<U>) {
        *self = *self + rhs;
    }
}

impl<U> Sum<Rat<U>> for Rat<U> {
    fn sum<I: Iterator<Item = Rat<U>>>(iter: I) -> Self {
        iter.fold(Rat::ZERO, |acc, a| acc + a)
    }
}

impl<U> Sub<Rat<U>> for Rat<U> {
    type Output = Rat<U>;

    fn sub(self, rhs: Rat<U>) -> Self::Output {
        Self(self.0 - rhs.0, Boo)
    }
}

impl Rat<Second> {
    pub fn to_minutes(self) -> Rat<Minute> {
        Rat(self.0 / RawRat::from_integer(60), Boo)
    }
}

impl<T> Rat<Per<T, Second>> {
    pub fn to_per_minute(&self) -> Rat<Per<T, Minute>> {
        Rat(self.0 * RawRat::from_integer(60), Boo)
    }
}

impl<T, V> Rat<Per<T, V>> {
    pub fn recip(self) -> Rat<Per<V, T>> {
        Rat(self.0.recip(), Boo)
    }
}

impl<T> Rat<T> {
    pub fn recip_raw(self) -> Rat<Per<One, T>> {
        Rat(self.0.recip(), Boo)
    }
}

impl<T, V> Rat<Per<One, Per<T, V>>> {
    pub fn simplify(self) -> Rat<Per<V, T>> {
        Rat(self.0, Boo)
    }
}

macro_rules! impl_mul {
    ($L:ty, $R:ty, $O:ty) => {
        impl Mul<Rat<$R>> for Rat<$L> {
            type Output = Rat<$O>;

            fn mul(self, rhs: Rat<$R>) -> Self::Output {
                Rat(self.0 * rhs.0, Boo)
            }
        }
    };
}

macro_rules! impl_div {
    ($L:ty, $R:ty, $O:ty) => {
        impl Div<Rat<$R>> for Rat<$L> {
            type Output = Rat<$O>;

            fn div(self, rhs: Rat<$R>) -> Self::Output {
                Rat(self.0 / rhs.0, Boo)
            }
        }
    };
}

impl_mul!(Per<Per<Items, Minute>, Recipes>, Recipes, Per<Items, Minute>);

impl_div!(Items, Second, Per<Items, Second>);
impl_div!(Items, Minute, Per<Items, Minute>);
impl_div!(Items, Per<Minute, Recipes>, Per<Per<Items, Minute>, Recipes>);
impl_div!(Per<Items, Second>, Recipes, Per<Per<Items, Second>, Recipes>);
impl_div!(Per<Items, Minute>, Recipes, Per<Per<Items, Minute>, Recipes>);
impl_div!(Per<Items, Minute>, Per<Per<Items, Minute>, Recipes>, Recipes);

impl Div<Rat<Unitless>> for ItemsPerMinute {
    type Output = ItemsPerMinute;

    fn div(self, rhs: Rat<Unitless>) -> Self::Output {
        Rat(self.0 / rhs.0, Boo)
    }
}

impl<T> Div<Rat<T>> for Rat<T> {
    type Output = Rat<Unitless>;

    fn div(self, rhs: Rat<T>) -> Self::Output {
        Rat(self.0 / rhs.0, Boo)
    }
}

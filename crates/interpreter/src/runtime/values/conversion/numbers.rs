pub trait NumberValue: Sized + Clone + PartialEq + PartialOrd + std::fmt::Debug {
    fn as_f64(&self) -> f64;
    fn as_i128(&self) -> i128;
    fn as_u128(&self) -> u128;
    fn as_u64(&self) -> u64;
    fn as_i64(&self) -> i64;
    fn as_i32(&self) -> i32;
    fn as_f32(&self) -> f32;
    fn as_bool(&self) -> bool;
    fn from_f64(f: f64) -> Self;
    fn from_i32(f: i32) -> Self;
    fn from_i128(i: i128) -> Self;
    fn from_u128(u: u128) -> Self;
    fn from_u64(u: u64) -> Self;
    fn from_i64(i: i64) -> Self;
    fn from_f32(f: f32) -> Self;
}

macro_rules! impl_number_value {
    ($ty:ty) => {
        impl NumberValue for $ty {
            fn as_f64(&self) -> f64 {
                *self as f64
            }
            fn as_i32(&self) -> i32 {
                *self as i32
            }
            fn as_i128(&self) -> i128 {
                *self as i128
            }
            fn as_u128(&self) -> u128 {
                *self as u128
            }
            fn as_u64(&self) -> u64 {
                *self as u64
            }
            fn as_i64(&self) -> i64 {
                *self as i64
            }
            fn as_f32(&self) -> f32 {
                *self as f32
            }
            fn as_bool(&self) -> bool {
                0 as $ty != *self
            }
            fn from_f64(f: f64) -> Self {
                f as $ty
            }
            fn from_i128(i: i128) -> Self {
                i as $ty
            }
            fn from_u128(u: u128) -> Self {
                u as $ty
            }
            fn from_u64(u: u64) -> Self {
                u as $ty
            }
            fn from_i64(i: i64) -> Self {
                i as $ty
            }
            fn from_i32(i: i32) -> Self {
                i as $ty
            }
            fn from_f32(f: f32) -> Self {
                f as $ty
            }
        }
    };
}
impl_number_value!(i64);
impl_number_value!(i32);
impl_number_value!(u64);
impl_number_value!(i128);
impl_number_value!(u128);
impl_number_value!(f64);
impl_number_value!(f32);

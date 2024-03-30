#![allow(incomplete_features)]
#![feature(
    unboxed_closures,
    fn_traits,
    lazy_cell,
    impl_trait_in_fn_trait_return,
    impl_trait_in_assoc_type
)]
#![recursion_limit = "512"]

pub mod combine;
pub mod nom;
pub mod parslers;

#[cfg(test)]
mod test;

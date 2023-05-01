#![feature(adt_const_params)]
#![cfg_attr(doc_cfg, feature(doc_cfg))]
#![warn(unsafe_op_in_unsafe_fn)]
#![no_std]

use core::mem::MaybeUninit;

mod pin;

pub use field_projection_internal::*;
pub use pin::*;

/// Representation of a field name.
///
/// A field name `x` is represented with `FieldId(field_name_hash("x"))`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FieldId(u64);

impl FieldId {
    #[doc(hidden)]
    pub const fn from_hash(hash: u64) -> Self {
        Self(hash)
    }
}

pub use const_fnv1a_hash::fnv1a_hash_str_64 as field_name_hash;

/// Information of a field of struct `Base`.
///
/// # Safety
/// The field must represent a field named `NAME` in a type `Base` that has the type `Type`.
/// The `map` function must be implemented such that it returns a pointer to the field.
///
/// This trait should not be implemented manually; instead, use the `#[derive(Field)]` instead.
pub unsafe trait FieldInfo<const F: FieldId> {
    /// The type of the field.
    type Type: ?Sized;
    /// The name of the field.
    const NAME: &'static str;

    /// Adjust the pointer from the containing struct to the field.
    ///
    /// # Safety
    /// `ptr` must be a non-null and aligned pointer to `Self::Base`.
    unsafe fn map(ptr: *const Self) -> *const Self::Type;
}

/// Trait for a wrapper type that can be projected to a field.
///
/// `F` is a descriptor of a field (`FieldName` with some generic parameters).
pub trait Projectable<T: FieldInfo<F>, const F: FieldId> {
    /// Type of the wrapped projected field.
    type Target;

    /// Project the field.
    ///
    /// # Safety
    /// The function must be called only if `F` is accessible with Rust privacy
    /// rules by the caller.
    unsafe fn project(self) -> Self::Target;

    #[doc(hidden)]
    unsafe fn project_with_check(this: Self, _check: fn(&T)) -> Self::Target
    where
        Self: Sized,
    {
        unsafe { Self::project(this) }
    }
}

impl<'a, T, const F: FieldId> Projectable<T, F> for &'a MaybeUninit<T>
where
    T: FieldInfo<F>,
    T::Type: Sized + 'a,
{
    type Target = &'a MaybeUninit<T::Type>;

    unsafe fn project(self) -> Self::Target {
        // SAFETY: Projecting through trusted `F::map`.
        unsafe { &*T::map(self.as_ptr()).cast::<MaybeUninit<T::Type>>() }
    }
}

impl<'a, T, const F: FieldId> Projectable<T, F> for &'a mut MaybeUninit<T>
where
    T: FieldInfo<F>,
    T::Type: Sized + 'a,
{
    type Target = &'a mut MaybeUninit<T::Type>;

    unsafe fn project(self) -> Self::Target {
        // SAFETY: Projecting through trusted `F::map`.
        unsafe {
            &mut *T::map(self.as_mut_ptr())
                .cast_mut()
                .cast::<MaybeUninit<T::Type>>()
        }
    }
}

#[macro_export]
macro_rules! project {
    ($a:expr => $b:ident) => {
        match $a {
            __expr => unsafe {
                $crate::Projectable::<
                    _,
                    { $crate::FieldId::from_hash($crate::field_name_hash(core::stringify!($b))) },
                >::project_with_check(__expr, |__check| {
                    let _ = __check.$b;
                })
            },
        }
    };
}

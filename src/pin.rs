use crate::*;
use core::pin::Pin;

/// Additional information on a field of a struct regarding to pinning.
///
/// # Safety
/// `PinWrapper` must be layout-compatible with `&mut Self::Type`. If the field is pinned, then
/// it should be `Pin<&mut Self::Type>`, otherwise it should be `&mut Self::Type`.
///
/// This trait should not be implemented manually; instead, use the `#[derive(PinField)]` instead.
pub unsafe trait PinField<const F: FieldId>: FieldInfo<F> {
    /// The type when this field is projected from a `Pin<&mut Self::Base>`.
    type PinWrapper<'a, U: ?Sized + 'a>;
}

impl<'a, T, const F: FieldId> Projectable<T, F> for Pin<&'a mut T>
where
    T: PinField<F>,
    T::Type: 'a,
{
    type Target = T::PinWrapper<'a, T::Type>;

    unsafe fn project(self) -> Self::Target {
        // SAFETY: This pointer will not be moved out, and the resulting projection will be wrapped
        // with `Pin` back if the field is pinned.
        let inner = unsafe { Self::into_inner_unchecked(self) };
        // SAFETY: Project the pointer through raw pointer. Note that the `*mut _` cast is important
        // as otherwise the `&mut` to `*const` cast will go through `&` reference which will retag it.
        let ptr = unsafe { &mut *T::map(inner as *mut _).cast_mut() };
        // This is either a `Pin<&mut T>` or `&mut T`, both layout compatible with `&mut T`.
        // Use `transmute_copy` here because the compiler can't prove that `F::PinWrapper` is of
        // the same size.
        unsafe { core::mem::transmute_copy(&ptr) }
    }
}

#[doc(hidden)]
pub struct AlwaysUnpin<T>(T);
impl<T> Unpin for AlwaysUnpin<T> {}

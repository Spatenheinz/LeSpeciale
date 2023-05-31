// use std::{cell::Cell, alloc::Layout, ptr::{NonNull, self}, marker::{PhantomData}, mem::{self, align_of_val_raw}};

// const TAG_BITS: usize = 3;
// const TAG_MASK: usize = 0b111;
// const PTR_MASK: usize = !TAG_MASK;

// #[repr(C)]
// struct RcBox<T: ?Sized> {
//     tag: Cell<usize>,
//     value: T,
// }

// pub struct Rc<T> {
//     ptr: NonNull<RcBox<T>>,
//     phantom: PhantomData<RcBox<T>>,
// }

// #[derive(PartialEq)]
// #[repr(u8)]
// pub enum MemTag {
//     Prim = 0,
//     Pi = 1,

// }

// #[derive(PartialEq)]
// #[repr(u8)]
// pub enum Primitives {
//     Box = 2,
//     Star = 3,
//     ZT = 4,
//     QT = 5,
// }

// pub struct Binder<T> {
//     pub bind: Option<Rc<T>>,
//     pub body: Rc<T>,
// }

// impl<T> Rc<T> {

//     pub fn mk_box() -> Self {
//         Self::primitive(Primitives::Box as usize)
//     }
//     pub fn mk_star() -> Self {
//         Self::primitive(Primitives::Star as usize)
//     }
//     pub fn mk_zt() -> Self {
//         Self::primitive(Primitives::ZT as usize)
//     }
//     pub fn mk_qt() -> Self {
//         Self::primitive(Primitives::QT as usize)
//     }

//     #[inline(always)]
//     pub fn tag(&self) -> MemTag {
//         // safety: pretty obvious knowing pointer tagging isnt it?
//         unsafe { std::mem::transmute((self.ptr.as_ptr() as usize & ALLOC_TAG_MASK) as u8) }
//     }

//     #[inline(always)]
//     fn primitive(raw: usize) -> Self {
//         unsafe { Self::from_ptr((raw | MemTag::Prim as usize) as _) }
//     }

//     #[inline(always)]
//     fn with_tag(raw: *const T, tag: MemTag) -> Self {
//         unsafe { Self::from_ptr((raw as usize | tag as usize) as _) }
//     }

//     #[inline(always)]
//     fn inner(&self) -> &RcBox<T> {
//         // This unsafety is ok because while this Rc is alive we're guaranteed
//         // that the inner pointer is valid.
//         unsafe { self.ptr.as_ref() }
//     }

//     unsafe fn from_inner(ptr: NonNull<RcBox<T>>) -> Self {
//         Self { ptr, phantom: PhantomData }
//     }

//     unsafe fn from_ptr(ptr: *mut RcBox<T>) -> Self {
//         unsafe { Self::from_inner(NonNull::new_unchecked(ptr)) }
//     }

//     // pub fn new(value: T) -> Rc<T> {
//     //     unsafe {
//     //         Self::from_inner(
//     //             Box::leak(Box::new(RcBox { tag: Cell::new(1), value }))
//     //                 .into(),
//     //         )
//     //     }
//     // }

//     pub fn into_raw(this: Self) -> *const T {
//         let ptr = Self::as_ptr(&this);
//         mem::forget(this);
//         ptr
//     }
//     pub fn as_ptr(this: &Self) -> *const T {
//         let ptr: *mut RcBox<T> = NonNull::as_ptr(this.ptr);
//         unsafe { ptr::addr_of_mut!((*ptr).value) }
//     }
//     pub unsafe fn from_raw(ptr: *const T) -> Self {
//         let offset = unsafe { data_offset(ptr) };

//         // Reverse the offset to find the original RcBox.
//         let rc_ptr = unsafe { ptr.byte_sub(offset) as *mut RcBox<T> };

//         unsafe { Self::from_ptr(rc_ptr) }
//     }
// }
// unsafe fn data_offset<T: ?Sized>(ptr: *const T) -> usize {
//     // Align the unsized value to the end of the RcBox.
//     // Because RcBox is repr(C), it will always be the last field in memory.
//     // SAFETY: since the only unsized types possible are slices, trait objects,
//     // and extern types, the input safety requirement is currently enough to
//     // satisfy the requirements of align_of_val_raw; this is an implementation
//     // detail of the language that must not be relied upon outside of std.
//     unsafe { data_offset_align(align_of_val_raw(ptr)) }
// }

// #[inline]
// fn data_offset_align(align: usize) -> usize {
//     let layout = Layout::new::<RcBox<()>>();
//     layout.size() + layout.padding_needed_for(align)
// }

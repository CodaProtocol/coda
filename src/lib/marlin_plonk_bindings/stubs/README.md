# Ocaml-rs

We use [OCaml-rs](https://github.com/zshipko/ocaml-rs) to facilitate exporting a Rust library to a static library that can be used from within OCaml. Insead of exporting code directly to a C interface, it makes use of the OCaml runtime directly and can store values and custom types on the OCaml heap.

The [docs](https://docs.rs/ocaml/0.22.0/ocaml/) are not great there. This is why we are here.

## Dealing with types

There are two ways of dealing with types:

1. let OCaml handle your types: use the `ocaml::ToValue` and `ocaml::FromValue` traits to let OCaml transform your types into OCaml types.
2. Make it opaque to OCaml: use [custom types](https://ocaml.org/manual/intfc.html#s:c-custom) to store opaque blocks within the OCaml heap. There's the [`ocaml::custom!`](https://docs.rs/ocaml/0.22.0/ocaml/macro.custom.html) macro to help you with that.

The first option is the easiest one, unless there are some foreign types in there. (Because of Rust's [*orphan rule*](https://github.com/Ixrec/rust-orphan-rules)) you can't implement the ToValue and FromValue traits on foreign types.)

This means that you'll have to use the second option anytime you're dealing with foreign types, by wrapping them into a local type and using `custom!`. 

## The ToValue and FromValue traits

In both methods, the [traits ToValue and FromValue](https://github.com/zshipko/ocaml-rs/blob/master/src/value.rs#L55:L73) are used:

```rust=
pub unsafe trait IntoValue {
    fn into_value(self, rt: &Runtime) -> Value;
}
pub unsafe trait FromValue<'a> {
    fn from_value(v: Value) -> Self;
}
```

these traits are implemented for all primitive Rust types ([here](https://github.com/zshipko/ocaml-rs/blob/master/src/conv.rs)), and can be derived automatically via [derive macros](https://docs.rs/ocaml/0.22.0/ocaml/#derives). Don't forget that you can use [cargo expand](https://github.com/dtolnay/cargo-expand) to expand macros, which is really useful to understand what the ocaml-rs macros are doing.

```
$ cargo expand -- some_filename_without_rs > expanded.rs
```

## Custom types

The macro [custom!](https://github.com/zshipko/ocaml-rs/blob/master/src/custom.rs) allows you to quickly create custom types.

The particularity of custom types is that OCaml has no idea what they are, they just contain an opaque pointer to some Rust structure that won't be followed when the GC frees that part of the memory. So it is your responsibility to free things using a `finalizer`.

Here is how custom types are transformed into OCaml values:

```rust=
unsafe impl<T: 'static + Custom> IntoValue for T {
    fn into_value(self, rt: &Runtime) -> Value {
        let val: crate::Pointer<T> = Pointer::alloc_custom(self);
        val.into_value(rt)
    }
}
```

which eventually is a call to `caml_alloc_custom`:

```rust=
/// Allocate custom value
pub unsafe fn alloc_custom<T: crate::Custom>() -> Value {
    let size = core::mem::size_of::<T>();
    Value::new(sys::caml_alloc_custom(
        T::ops() as *const _ as *const sys::custom_operations,
        size,
        T::USED,
        T::MAX,
    ))
}
```

and the data of your type (probably a pointer to some Rust memory) is copied into the OCaml's heap ([source](https://github.com/zshipko/ocaml-rs/blob/master/src/types.rs#L80)):

```rust=
pub fn set(&mut self, x: T) {
    unsafe {
        core::ptr::write_unaligned(self.as_mut_ptr(), x);
    }
}
```

## Conventions

* To ease FFI code, we use the `Caml` prefix whenever we're dealing with types that OCaml will be able to understand. This allows to quickly read a function's signature and see that there are only types that support `ocaml::FromValue` and `ocaml::ToValue`.
* You should not include any value allocated on the OCaml heap within a custom type, otherwise it won't get free'ed by OCaml's GC when the host type gets free'ed. Now, I'm not sure how you could achieve that, but if you end up there think about what you're doing.
* You should implement a `drop_in_place` finalizer for all custom types. Better be safe than sorry.

use std::{convert::TryInto, rc::Rc};
use ticc_core::ir;
use crate::Value;

#[derive(Clone)]
pub struct Tagged<F> {
    alloc: Rc<TaggedAlloc<[Value<F>]>>,
}

struct TaggedAlloc<T: ?Sized> {
    tag: ir::Name,
    values: T,
}

impl<F> Tagged<F> {
    pub(crate) fn from_array<const N: usize>(tag: ir::Name, arr: [Value<F>; N]) -> Tagged<F> {
        let alloc = Rc::new(TaggedAlloc {
            tag,
            values: arr,
        });
        Tagged { alloc }
    }

    pub(crate) fn from_vec(tag: ir::Name, values: Vec<Value<F>>) -> Tagged<F> {
        match values.len() {
            0 => Tagged::from_array::<0>(tag, []),
            1 => Tagged::from_array::<1>(tag, values.try_into().unwrap()),
            2 => Tagged::from_array::<2>(tag, values.try_into().unwrap()),
            3 => Tagged::from_array::<3>(tag, values.try_into().unwrap()),
            4 => Tagged::from_array::<4>(tag, values.try_into().unwrap()),
            5 => Tagged::from_array::<5>(tag, values.try_into().unwrap()),
            6 => Tagged::from_array::<6>(tag, values.try_into().unwrap()),
            7 => Tagged::from_array::<7>(tag, values.try_into().unwrap()),
            8 => Tagged::from_array::<8>(tag, values.try_into().unwrap()),
            9 => Tagged::from_array::<9>(tag, values.try_into().unwrap()),
            other => panic!("unsupported tagged value size: {}", other),
        }
    }

    pub fn tag(&self) -> ir::Name {
        self.alloc.tag
    }

    pub fn fields(&self) -> &[Value<F>] {
        &self.alloc.values
    }
}

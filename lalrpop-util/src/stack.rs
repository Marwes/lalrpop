use std::{convert::TryFrom, marker::PhantomData, mem, ptr, slice};

pub struct Stack<T> {
    buf: Vec<u8>,
    len: usize,
    _marker: PhantomData<T>,
}

pub type Tag = u16;

impl<T> Default for Stack<T> {
    fn default() -> Self {
        Stack::new()
    }
}

impl<T> Stack<T> {
    pub fn new() -> Self {
        Stack {
            buf: Vec::new(),
            len: 0,
            _marker: PhantomData,
        }
    }
}

impl<T> Stack<T>
where
    T: Push,
{
    pub fn push(&mut self, value: T::Value) {
        unsafe { T::push_to(value, self) }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub unsafe fn unchecked_push<U>(&mut self, tag: Tag, value: U) {
        self.buf.extend_from_slice(slice::from_raw_parts(
            &value as *const _ as *const u8,
            mem::size_of::<U>(),
        ));
        self.buf.extend_from_slice(&tag.to_ne_bytes());
        self.len += 1;
        mem::forget(value);
    }

    pub fn truncate(&mut self, len: usize) {
        while self.len > len {
            self.pop();
        }
    }

    pub fn pop_until(&mut self, i: usize) -> Option<T::Value> {
        if i >= self.len() {
            None
        } else {
            let mut out = None;
            while i < self.len() {
                out = self.pop();
            }
            out
        }
    }

    pub fn with_last<R>(&mut self, f: impl FnOnce(&T::Value) -> R) -> Option<R> {
        let v = self.pop();
        if let Some(v) = v {
            let r = f(&v);
            self.push(v);
            Some(r)
        } else {
            None
        }
    }

    pub fn pop(&mut self) -> Option<T::Value> {
        if self.len == 0 {
            return None;
        }
        unsafe {
            let tag_start = self.buf.len() - 2;
            let tag = Tag::from_ne_bytes(*<&[u8; 2]>::try_from(&self.buf[tag_start..]).unwrap());
            Some(T::pop_from(tag, self))
        }
    }

    pub unsafe fn unchecked_pop<U>(&mut self) -> U {
        let tag_start = self.buf.len() - 2;
        let value_start = tag_start - mem::size_of::<U>();
        let value = ptr::read_unaligned(self.buf[value_start..].as_ptr() as *const U);
        self.buf.truncate(value_start);
        self.len -= 1;
        value
    }
}

pub trait Push: Sized {
    type Value;
    unsafe fn push_to(value: Self::Value, stack: &mut Stack<Self>);
    unsafe fn pop_from(tag: Tag, stack: &mut Stack<Self>) -> Self::Value;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, PartialEq)]
    enum Test {
        Int(i32),
        Float(f64),
    }

    impl Push for Test {
        type Value = Self;
        unsafe fn push_to(value: Self::Value, stack: &mut Stack<Self>) {
            match value {
                Self::Int(i) => stack.unchecked_push(0, i),
                Self::Float(f) => stack.unchecked_push(1, f),
            }
        }
        unsafe fn pop_from(tag: Tag, stack: &mut Stack<Self>) -> Self {
            match tag {
                0 => Self::Int(stack.unchecked_pop()),
                1 => Self::Float(stack.unchecked_pop()),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn push_pop() {
        let mut stack = Stack::<Test>::new();

        assert_eq!(stack.pop(), None);
        assert_eq!(stack.pop(), None);

        stack.push(Test::Int(1));
        assert_eq!(stack.pop(), Some(Test::Int(1)));

        stack.push(Test::Int(1));
        stack.push(Test::Float(3.14));
        assert_eq!(stack.pop(), Some(Test::Float(3.14)));

        stack.push(Test::Float(0.0));
        stack.push(Test::Int(2));
        assert_eq!(stack.pop(), Some(Test::Int(2)));
        assert_eq!(stack.pop(), Some(Test::Float(0.0)));
        assert_eq!(stack.pop(), Some(Test::Int(1)));
    }

    #[test]
    fn pop_until() {
        let mut stack = Stack::<Test>::new();

        stack.push(Test::Int(0));
        stack.push(Test::Int(1));
        stack.push(Test::Int(2));
        assert_eq!(stack.pop_until(1), Some(Test::Int(1)));
        assert_eq!(stack.len(), 1);
    }
}

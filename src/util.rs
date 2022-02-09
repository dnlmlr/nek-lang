/// The PutBackIter allows for items to be put back back and to be peeked. Putting an item back
/// will cause it to be the next item returned by `next`. Peeking an item will get a reference to
/// the next item in the iterator without removing it.
///
/// The whole PutBackIter behaves analogous to `std::iter::Peekable` with the addition of the
/// `putback` function. This is slightly slower than `Peekable`, but allows for an unlimited number
/// of putbacks and therefore an unlimited look-ahead range.
pub struct PutBackIter<T: Iterator> {
    iter: T,
    putback_stack: Vec<T::Item>,
}

impl<T> PutBackIter<T>
where
    T: Iterator,
{
    /// Make the given iterator putbackable, wrapping it in the PutBackIter type. This effectively 
    /// adds the `peek` and `putback` functions.
    pub fn new(iter: T) -> Self {
        Self {
            iter,
            putback_stack: Vec::new(),
        }
    }

    /// Put the given item back into the iterator. This causes the putbacked items to be returned by
    /// next in last-in-first-out order (aka. stack order). Only after all previously putback items
    /// have been returned, the actual underlying iterator is used to get items.
    /// The number of items that can be put back is unlimited.
    pub fn putback(&mut self, it: T::Item) {
        self.putback_stack.push(it);
    }

    /// Peek the next item, getting a reference to it without removing it from the iterator. This 
    /// also includes items that were previsouly put back and not yet removed.
    pub fn peek(&mut self) -> Option<&T::Item> {
        if self.putback_stack.is_empty() {
            let it = self.next()?;
            self.putback(it);
        }

        self.putback_stack.last()
    }
}

impl<T> Iterator for PutBackIter<T>
where
    T: Iterator,
{
    type Item = T::Item;

    fn next(&mut self) -> Option<Self::Item> {
        match self.putback_stack.pop() {
            Some(it) => Some(it),
            None => self.iter.next(),
        }
    }
}

pub trait PutBackableExt {
    /// Make the iterator putbackable, wrapping it in the PutBackIter type. This effectively 
    /// adds the `peek` and `putback` functions.
    fn putbackable(self) -> PutBackIter<Self>
    where
        Self: Iterator + Sized,
    {
        PutBackIter::new(self)
    }
}

impl<T: Iterator> PutBackableExt for T {}

#[cfg(test)]
mod tests {
    use super::PutBackableExt;

    #[test]
    fn putback_iter_next() {
        let mut iter = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].into_iter();
        let mut pb_iter = iter.clone().putbackable();

        // Check if next works
        for _ in 0..iter.len() {
            assert_eq!(pb_iter.next(), iter.next());
        }
    }

    #[test]
    fn putback_iter_peek() {
        let mut iter_orig = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].into_iter();
        let mut iter = iter_orig.clone();
        let mut pb_iter = iter.clone().putbackable();

        for _ in 0..iter.len() {
            // Check if peek gives a preview of the actual next element
            assert_eq!(pb_iter.peek(), iter.next().as_ref());
            // Check if next still returns the next (just peeked) element and not the one after
            assert_eq!(pb_iter.next(), iter_orig.next());
        }
    }

    #[test]
    fn putback_iter_putback() {
        let mut iter_orig = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].into_iter();
        let mut iter = iter_orig.clone();
        let mut pb_iter = iter.clone().putbackable();

        // Get the first 5 items with next and check if they match
        let it0 = pb_iter.next();
        assert_eq!(it0, iter.next());
        let it1 = pb_iter.next();
        assert_eq!(it1, iter.next());
        let it2 = pb_iter.next();
        assert_eq!(it2, iter.next());
        let it3 = pb_iter.next();
        assert_eq!(it3, iter.next());
        let it4 = pb_iter.next();
        assert_eq!(it4, iter.next());

        // Put one value back and check if `next` works as expected, returning the just put back 
        // item
        pb_iter.putback(it0.unwrap());
        assert_eq!(pb_iter.next(), it0);

        // Put all values back
        pb_iter.putback(it4.unwrap());
        pb_iter.putback(it3.unwrap());
        pb_iter.putback(it2.unwrap());
        pb_iter.putback(it1.unwrap());
        pb_iter.putback(it0.unwrap());

        // After all values have been put back, the iter should match the original again
        for _ in 0..iter.len() {
            assert_eq!(pb_iter.next(), iter_orig.next());
        }
    }
}

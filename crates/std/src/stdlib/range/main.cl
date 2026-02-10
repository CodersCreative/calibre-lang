impl range {
	const iter = fn(self: range) -> RangeIter => iter_range(self);
	const into_iter = fn(self: range) -> RangeIter => iter_range(self);
}

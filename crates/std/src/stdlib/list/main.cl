impl<T> list:<T> {
	const len = fn(self: list:<T>) -> int => len(self);
	const iter = fn(self: list:<T>) -> ListIter:<T> => iter_list(self);
	const into_iter = fn(self: list:<T>) -> ListIter:<T> => iter_list(self);
}

impl str {
	const len = fn(self: str) -> int => len(self);
	const trim = fn(self: str) -> str => trim(self);
	const split = fn(self: str, delim: str) -> list:<str> => str_split(self, delim);
	const contains = fn(self: str, needle: str) -> bool => str_contains(self, needle);
	const starts_with = fn(self: str, prefix: str) -> bool => str_starts_with(self, prefix);
	const ends_with = fn(self: str, suffix: str) -> bool => str_ends_with(self, suffix);
	const iter = fn(self: str) -> StrIter => iter_str(self);
	const into_iter = fn(self: str) -> StrIter => iter_str(self);
}

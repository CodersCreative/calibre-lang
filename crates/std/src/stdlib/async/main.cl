type Channel:<T> = struct {};
type WaitGroup = struct {};
type Mutex:<T> = struct {};

impl<T> Channel:<T> {
	const new = fn() -> Channel:<T> => channel_new();
	const send = fn(self: &mut Channel:<T>, value: T) => channel_send(self, value);
	const get = fn(self: &Channel:<T>) -> T? => channel_get(self);
	const close = fn(self: &mut Channel:<T>) => channel_close(self);
	const closed = fn(self: &Channel:<T>) -> bool => channel_closed(self);
}

impl WaitGroup {
	const new = fn() -> WaitGroup => waitgroup_new();
	const add = fn(self: &mut WaitGroup, value: int) => waitgroup_add(self, value);
	const increment = fn(self: &mut WaitGroup) => waitgroup_add(self, 1);
	const done = fn(self: &mut WaitGroup) => waitgroup_done(self);
	const wait = fn(self: &WaitGroup) => waitgroup_wait(self);
	const count = fn(self: &WaitGroup) -> int => waitgroup_count(self);
}

impl<T> Mutex:<T> {
	const new = fn(value: T) -> Mutex:<T> => mutex_new(value);
	const get = fn(self: &Mutex:<T>) -> T => mutex_get(self);
	const set = fn(self: &mut Mutex:<T>, value: T) => mutex_set(self, value);
	const with = fn<U>(self: &mut Mutex:<T>, op: fn(T) -> U) -> U => mutex_with(self, op);
	const write = fn(self: &mut Mutex:<T>) -> &mut T => mutex_write(self);
}

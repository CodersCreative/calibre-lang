type SmthType:<T> = struct {
	value : T
}

const id = fn<T>(x : T) -> T => {
    x;
};

const main = fn() => {
    let smth = SmthType:<int> {value : 42};
    let v : int = id:<int>(smth.value);
    print(v);
};

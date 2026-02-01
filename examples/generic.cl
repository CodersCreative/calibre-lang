const id = fn<T>(x : T) -> T => {
    x;
};

const main = fn() => {
    let v : int = id:<int>("42");
    print(v);
};

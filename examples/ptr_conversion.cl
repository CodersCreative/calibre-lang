const main = fn() => {
    let value = 42;

    let p = value as ptr:<int>;
    print(p);
    match p {
        .Ok : val => print(val as int),
        .Err : msg => print("err: " & msg),
    };

    let any = value as ptr;
    print(any);
    match any {
        .Ok : val => print(val as int),
        .Err : msg => print("err: " & msg),
    };
};

/*const main = fn () => {
    let mut a = 0;
    for => {
        a += 10;
        if a > 10000000 => {
            print(a);
            return;
        };
    }
};*/

type Thing = struct {
    name : str,
};

const main = fn () => {
    let a = [10, 20, 450];
    print(a[0]);
    let thing = Thing {name : "Ty"};
    print(thing.name);
};

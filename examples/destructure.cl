type Pair = struct {
    left : int,
    right : int,
};

type PairEnum = enum {
    Normal : Pair,
    Tuple : <int, int>
};

impl PairEnum {
    const display = fn match &Self {
        .Tuple : left, right => {
            print(left + right);
        },
        .Normal : {left : l, right : r} => {
            print(l + r);
        }
    }

}

const sum_pair = fn((a, b)) => a + b;

const main = fn() => {
    let pair = Pair{left : 10, right : 20};
    let {left, right} = pair;

    let mut first, mut second = tuple(1, 2);
    print(first + second);

    let t = 10, 20, 30, 40;
    let start, .., end = t;
    print(start + end);

    let mut x = 0;
    let mut y = 0;
    x, y = tuple(5, 6);
    print(x + y);

    let mut pair_enum = PairEnum.Tuple : 14, 25;
    print(pair_enum);
    pair_enum.display();

    pair_enum = PairEnum.Normal : pair;
    print(pair_enum);
    pair_enum.display();

    print(sum_pair(tuple(7, 8)));
};

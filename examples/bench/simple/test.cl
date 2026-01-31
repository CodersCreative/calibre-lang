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

type ThingEnum = enum {
    Person : Thing,
    Animal : Thing,
    Object : Thing,
    Nothing,
};

impl ThingEnum {
    const age = 90;
    
    const name = fn match &ThingEnum -> str {
        .Person | .Animal | .Object : thing => *thing.name,
        _ => "None",
    }
}

const main = fn () => {
    let a = [10, 20, 450];
    print(a[0]);
    let thing = Thing {name : "Ty"};
    print(thing.name);
    let thing = ThingEnum.Person : thing;
    print(thing);
    print("mentioned object : " & ThingEnum.name(thing));
    print("object inferred : " & thing.name());
    print("static from object : " & ThingEnum.age);
    print("static from object inferred : " & thing.age);
    
};

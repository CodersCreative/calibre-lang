extern "c" const c_usleep = fn(@uint) -> @int from "libc" as "usleep";

const wait = fn(ms: int) => {
    if ms <= 0 => null else => {
        c_usleep((ms as uint) * 1000);
        null;
    };
};

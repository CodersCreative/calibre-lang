extern "c" const c_usleep = fn(@int) -> @int from "libc" as "usleep";

const wait = fn(ms: int) => if ms <= 0 => null else => {
    c_usleep(ms * 1000);
    null;
};

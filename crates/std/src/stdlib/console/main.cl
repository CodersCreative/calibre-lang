extern "c" const c_write = fn(@int, str, @usize) -> @isize from "libc" as "write";
extern "c" const c_strlen = fn(str) -> @usize from "libc" as "strlen";
extern "c" const c_fdopen = fn(@int, str) -> ptr:<@u8> from "libc" as "fdopen";
extern "c" const c_fgets = fn(ptr:<@u8>, @int, ptr:<@u8>) -> str from "libc" as "fgets";
extern "c" const c_malloc = fn(@usize) -> ptr:<@u8> from "libc" as "malloc";
extern "c" const c_free = fn(ptr:<@u8>) -> null from "libc" as "free";

const out = fn(msg) => print(msg);

const err = fn(msg) => {
    write_fd(2, ("" & msg) & "\n");
    null;
};

const clear = fn() => {
    out("\x1b[2J\x1b[1;1H");
    null;
};

const input = fn(prompt) -> str => {
    if prompt != null => {
        out(prompt);
    } else => null;
    let handle = c_fdopen(0, "r");
    let size = 4096;
    let buf = c_malloc(size as usize);
    let line = c_fgets(buf, size, handle);
    c_free(buf);
    trim(line);
};

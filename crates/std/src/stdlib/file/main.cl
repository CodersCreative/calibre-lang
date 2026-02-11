extern "c" const c_fopen = fn(str, str) -> ptr:<@u8> from "libc" as "fopen";
extern "c" const c_fclose = fn(ptr:<@u8>) -> @int from "libc" as "fclose";
extern "c" const c_fflush = fn(ptr:<@u8>) -> @int from "libc" as "fflush";
extern "c" const c_fgets = fn(ptr:<@u8>, @int, ptr:<@u8>) -> str from "libc" as "fgets";
extern "c" const c_fwrite = fn(str, @usize, @usize, ptr:<@u8>) -> @usize from "libc" as "fwrite";
extern "c" const c_strlen = fn(str) -> @usize from "libc" as "strlen";
extern "c" const c_malloc = fn(@usize) -> ptr:<@u8> from "libc" as "malloc";
extern "c" const c_free = fn(ptr:<@u8>) -> null from "libc" as "free";
extern "c" const c_access = fn(str, @int) -> @int from "libc" as "access";
extern "c" const c_remove = fn(str) -> @int from "libc" as "remove";

const open = fn(path: str, mode: str) -> ptr:<uint> => c_fopen(path, mode);

const open_read = fn(path: str) -> ptr:<uint> => c_fopen(path, "r");
const open_write = fn(path: str) -> ptr:<uint> => c_fopen(path, "w");
const open_append = fn(path: str) -> ptr:<uint> => c_fopen(path, "a");

const close = fn(handle: ptr:<uint>) => {
    c_fclose(handle);
    null;
};

const flush = fn(handle: ptr:<uint>) => {
    c_fflush(handle);
    null;
};

const write = fn(handle: ptr:<uint>, content) => {
    let txt = "" & content;
    let len = c_strlen(txt);
    c_fwrite(txt, 1, len, handle);
    null;
};

const write_line = fn(handle: ptr:<uint>, content) => {
    write(handle, ("" & content) & "\n");
    null;
};

const exists = fn(path: str) -> bool => c_access(path, 0) == 0;

const remove = fn(path: str) -> bool => c_remove(path) == 0;

const read_line = fn(handle: ptr:<uint>, max: int) -> str => {
    let size = if max <= 0 => 1 else => max + 1;
    let buf = c_malloc(size);
    let line = c_fgets(buf, size, handle);
    c_free(buf);
    line;
};

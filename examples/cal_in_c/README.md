## Calibre C Bindings
### Im gonna be honest with ya.
Just make sure to link against the library generated using `cargo build --release --features ffi`
If you do that it should work...

### Building

Here is an example command assuming you are running it from this dir and you havent moved the library.
```sh
# For the Calibre Engine example:
g++ calibre_example.c -o calibre -L../../target/release -lcalibre -Wl,-rpath='../../target/release'

# For the Fmt example:
g++ fmt_example.c -o calibre_fmt -L../../target/release -lcalibre_fmt -Wl,-rpath='../../target/release'
```

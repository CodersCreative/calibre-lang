hyperfine \
  "calibre run test_ffi.cal" \
  "calibre run --no-cache test_ffi.cal" \
  "calibre run --no-type-check test_ffi.cal" \
  "calibre run --no-type-check --no-cache test_ffi.cal" \
  "calibre run test_none_ffi.cal" \
  "calibre run --no-cache test_none_ffi.cal" \
  "calibre run --no-type-check test_none_ffi.cal" \
  "calibre run --no-type-check --no-cache test_none_ffi.cal" \
  "calibre run test.cal" \
  "calibre run --no-cache test.cal" \
  "calibre run --no-type-check test.cal" \
  "calibre run --no-type-check --no-cache test.cal" \
  "calibre run test_tail.cal" \
  "calibre run --no-cache test_tail.cal" \
  "calibre run --no-type-check test_tail.cal" \
  "calibre run --no-type-check --no-cache test_tail.cal" \
  "calibre run test_none.cal" \
  "calibre run --no-cache test_none.cal" \
  "calibre run --no-type-check test_none.cal" \
  "calibre run --no-type-check --no-cache test_none.cal" \
  "python test.py" \
  "rustpython test_none.py" \
  "python test_none.py" \
  "rhai-run test.rhai" \
  "lua test.lua" \
  "ruby test.rb" \
  "perl test.pl" \
  "revo test.rv" \
  "revo test_none.rv" \
  "php test.php" \
  "php test_none.php" \
  --warmup 3
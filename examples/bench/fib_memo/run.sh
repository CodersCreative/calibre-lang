hyperfine \
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
  "rustpython test.py" \
  "python test.py" \
  "rhai-run test.rhai" \
  "lua test.lua" \
  "ruby test.rb" \
  "perl test.pl" \
  "revo test.rv" \
  --warmup 3
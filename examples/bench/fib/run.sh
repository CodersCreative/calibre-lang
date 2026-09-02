hyperfine \
  "rhai-run ../fib_iter/test.rhai" \
  "python ../fib_iter/test.py" \
  "rustpython ../fib_iter/test.py" \
  "revo ../fib_iter/test.rv" \
  "calibre run ../fib_iter/test.cal" \
  "calibre run ../fib_tail/test.cal" \
  "calibre run test.cal" \
  "calibre run ../fib_big/test.cal" \
  "calibre run --no-cache test.cal" \
  "calibre run --no-type-check test.cal" \
  "calibre run --no-type-check ../fib_big/test.cal" \
  "calibre run --no-type-check --no-cache test.cal" \
  "calibre_2 run ../fib_iter/test.cal" \
  "calibre_2 run ../fib_tail/test.cal" \
  "calibre_2 run test.cal" \
  "calibre_2 run ../fib_big/test.cal" \
  "calibre_2 run --no-cache test.cal" \
  "rustpython test.py" \
  "python test.py" \
  "rhai-run test.rhai" \
  "lua test.lua" \
  "ruby test.rb" \
  "perl test.pl" \
  "roc test.roc" \
  "revo test.rv" \
  "cal --path test.cl" \
  --warmup 3
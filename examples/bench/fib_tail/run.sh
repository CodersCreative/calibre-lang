hyperfine \
  "calibre run test_indirect.cal" \
  "calibre run test.cal" \
  "calibre run --backend jit test_indirect.cal" \
  "calibre run --backend jit test.cal" \
  --warmup 3
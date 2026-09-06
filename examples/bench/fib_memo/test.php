<?php
function fib($n) {
    static $cache = [];

    if ($n < 2) {
        return $n;
    }

    if (!isset($cache[$n])) {
        $cache[$n] = fib($n - 1) + fib($n - 2);
    }

    return $cache[$n];
}

for ($i = 0; $i < 31; $i++) {
    echo fib($i) . PHP_EOL;
}
?>
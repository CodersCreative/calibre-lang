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

$n = 28;
$result = fib($n);
echo $result;
?>
sub fib {
    my $n = shift;
    return $n < 2 ? $n : fib($n-1) + fib($n-2);
}

$n = 30;
$result = fib($n);
print $result;
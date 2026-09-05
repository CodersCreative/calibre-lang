sub fib {
    my $n = shift;
    return $n < 2 ? $n : fib($n-1) + fib($n-2);
}

foreach my $i (0..30) {
    print fib($i);
}
#include <stdint.h>

int64_t fib(int64_t n){
    if (n < 2) {
        return n;
    } else {
        return fib(n-1) + fib(n-2);
    }
}

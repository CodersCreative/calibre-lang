#include <stdint.h>

typedef struct {
    int64_t first;
    int64_t second;
    int64_t third;
} Bytes;

int64_t bytes_sum(Bytes v) {
    return v.first + v.second + v.third;
}

int bytes_equal(Bytes a, Bytes b) {
    return (a.first == b.first && a.second == b.second && a.third == b.third) ? 1 : 0;
}

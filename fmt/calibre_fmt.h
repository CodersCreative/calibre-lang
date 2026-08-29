#include <cstdarg>
#include <cstdint>
#include <cstdlib>
#include <ostream>
#include <new>

struct CalibreFmtResult {
  char *output;
  char *error;
};

extern "C" {

CalibreFmtResult *calibre_fmt_format(const char *source);

CalibreFmtResult *calibre_fmt_format_with_width(const char *source, uintptr_t max_width);

void calibre_fmt_result_free(CalibreFmtResult *ptr);

void calibre_fmt_string_free(char *ptr);

}  // extern "C"

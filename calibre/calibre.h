#include <cstdarg>
#include <cstdint>
#include <cstdlib>
#include <ostream>
#include <new>

struct CalibreEngine;

struct CalibreArtifacts {
  char *entry_name;
  char **mappings;
  uintptr_t mappings_len;
};

struct CalibreRunResult {
  char *return_value;
  char *captured_output;
};

extern "C" {

CalibreEngine *calibre_engine_new();

void calibre_engine_free(CalibreEngine *ptr);

void calibre_engine_set_no_std(CalibreEngine *ptr, bool no_std);

void calibre_engine_set_entry_name(CalibreEngine *ptr, const char *name);

void calibre_engine_add_prelude(CalibreEngine *ptr, const char *src);

void calibre_engine_add_input(CalibreEngine *ptr, const char *input);

CalibreArtifacts *calibre_compile_source(CalibreEngine *ptr, const char *src);

void calibre_artifacts_free(CalibreArtifacts *ptr);

CalibreRunResult *calibre_engine_run_source(CalibreEngine *ptr, const char *src);

void calibre_run_result_free(CalibreRunResult *ptr);

}  // extern "C"

#include <stdio.h>
#include <stdlib.h>
#include "../../calibre/calibre.h"

int main(void) {
    CalibreEngine *engine = calibre_engine_new();
    if (!engine) {
        fprintf(stderr, "failed to create calibre engine\n");
        return 1;
    }

    calibre_engine_set_entry_name(engine, "main");
    calibre_engine_add_prelude(engine, "const add := fn(a b : int) -> int => return a + b;\n");

    const char *source =
        "const main := fn -> int => {\n"
            "print(add(10, 32));\n"
            "print(10000 + 70);\n"
            "return 90\n"
        "}\n";


    CalibreRunResult *result = calibre_engine_run_source(engine, source);
    if (!result) {
        fprintf(stderr, "engine execution failed\n");
        calibre_engine_free(engine);
        return 1;
    }

    printf("return_value: %s\n", result->return_value ? result->return_value : "<null>");
    printf("captured_output: %s\n", result->captured_output ? result->captured_output : "<null>");

    calibre_run_result_free(result);
    calibre_engine_free(engine);
    return 0;
}

#include <stdio.h>
#include <stdlib.h>
#include "../../fmt/calibre_fmt.h"

int main(void) {
    const char *source =
        "let x := 1 + 2;\n"
        "print(10);\n";

    CalibreFmtResult *result = calibre_fmt_format(source);
    if (!result) {
        fprintf(stderr, "formatting failed\n");
        return 1;
    }

    if (result->error && result->error[0] != '\0') {
        fprintf(stderr, "format error: %s\n", result->error);
        calibre_fmt_result_free(result);
        return 1;
    }

    printf("formatted output:\n%s\n", result->output ? result->output : "<null>");
    calibre_fmt_result_free(result);

    CalibreFmtResult *narrow_result = calibre_fmt_format_with_width(source, 10);
    if (!narrow_result) {
        fprintf(stderr, "width formatting failed\n");
        return 1;
    }

    if (narrow_result->error && narrow_result->error[0] != '\0') {
        fprintf(stderr, "width format error: %s\n", narrow_result->error);
        calibre_fmt_result_free(narrow_result);
        return 1;
    }

    printf("narrow output:\n%s\n", narrow_result->output ? narrow_result->output : "<null>");
    calibre_fmt_result_free(narrow_result);
    return 0;
}

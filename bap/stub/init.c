#include <stdio.h>
#include <caml/callback.h>

void bap__stdio_init(FILE *, FILE *, FILE *);
int bap_main_init(void *);
char* bap_error_get();

void bap_init(int argc, const char **argv)
{
    caml_startup((char **)argv);
    bap__stdio_init(stdin, stdout, stderr);
    int r = bap_main_init(NULL);
    if (r != 0) {
        char *er = bap_error_get();
        fprintf(stderr, "bap init failed: %s\n", er);
        exit(1);
    }
}

#include <stdio.h>
#include <caml/callback.h>

void bap__stdio_init(FILE *, FILE *, FILE *);

void bap_init(int argc, const char **argv)
{
    caml_startup((char **)argv);
    bap__stdio_init(stdin, stdout, stderr);
}

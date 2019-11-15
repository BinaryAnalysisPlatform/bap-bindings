#include <stdio.h>
#include <caml/callback.h>

void bap__stdio_init(FILE *, FILE *, FILE *);

struct bap_main_parameters_t;
struct bap_init_error_t;
struct bap_init_error_t* bap__main_init(struct bap_main_parameters_t *);

struct bap_init_error_t* bap_init2(int argc, const char **argv, struct bap_main_parameters_t *pars) {
    caml_startup((char **)argv);
    bap__stdio_init(stdin, stdout, stderr);
    return bap__main_init(pars);
}

void bap_init(int argc, const char **argv) {
    bap_init2(argc,argv,0);
}

#include <stdio.h>
#include <caml/callback.h>

void bap__stdio_init(FILE *, FILE *, FILE *);

struct bap_parameters_t;
int bap__main_init(struct bap_parameters_t *);
const char * bap_error_get(void);

int bap_init2(int argc, const char **argv, struct bap_parameters_t *pars) {
    caml_startup((char **)argv);
    bap__stdio_init(stdin, stdout, stderr);
    return bap__main_init(pars);
}

void bap_init(int argc, const char **argv) {
    if (bap_init2(argc,argv,NULL)) {
        fprintf(stderr, "failed to initialize BAP: %s\n", bap_error_get());
        exit(1);
    }
}

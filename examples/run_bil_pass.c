#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "bap.h"

int run = 1;
char *pass_name = "my-pass";


//just a dummy pass that outputs a message to proove it was run
bap_bil_t my_pass(bap_bil_t x, void *a) {
    if (run) {
        printf("pass applied!!!\n");
        run = 0;
    }
    return x;
}

void print_passes(char *prepend, bap_bil_pass_seq_t *p) {
    bap_bil_pass_seq_iterator_t *iter = bap_bil_pass_seq_iterator_create(p);
    bap_bil_pass_t *c = bap_bil_pass_seq_iterator_next(iter);

    printf("%s:\n", prepend);
    while (c) {
        char *name = bap_bil_pass_name(c);
        printf("  - %s\n", name);
        c = bap_bil_pass_seq_iterator_next(iter);
    };
    printf("\n");

    bap_release(iter);
    bap_release(c);
}

void print_all_passes(char *prepend) {
    bap_bil_pass_seq_t *p = bap_bil_pass_passes();
    print_passes(prepend, p);
    bap_release(p);
}

bool its_my_pass(bap_bil_pass_t *c, void *x) {
    char *name = bap_bil_pass_name(c);
    return !(strcmp(name, pass_name));
}


int main(int argc, const char **argv) {
    //init caml runtime, init IO
    bap_init_error_t *er = bap_init2(argc, argv, NULL);

    if (er) {
        printf("failed to initialize BAP: %s\n", bap_error_get());
        return 1;
    }

    if (argc < 2) {
        fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
        return 1;
    }

    // print already registered pass
    print_all_passes("registered passes");

    // registered a new one and print again
    bap_bil_pass_register(pass_name, my_pass, NULL, NULL);
    print_all_passes("updated passes");

    // select only one pass
    bap_bil_pass_seq_t *p = bap_bil_pass_passes();
    bap_bil_pass_seq_t *s = bap_bil_pass_seq_filter(p,its_my_pass,NULL);
    print_passes("filtered passes", s);
    bap_bil_pass_select(s);

    // create project
    bap_project_input_t *input = bap_project_input_file((char *)argv[1], "llvm");
    bap_project_t *proj = bap_project_create(input, NULL);

    // release pointers obtained from BAP
    bap_release(p);
    bap_release(s);
    bap_release(input);
    bap_release(proj);

    return 0;
}

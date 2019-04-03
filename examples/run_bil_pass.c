#include <stdio.h>
#include "bap.h"

int run = 1;

//just a dummy pass that outputs a message to proove it was run
bap_bil_t my_pass(bap_bil_t x, void *a) {
    if (run) {
        printf("pass applied!!!\n");
        run = 0;
    }
    return x;
}

void print_passes(char *prepend) {
    bap_bil_pass_seq_t *p = bap_bil_pass_passes();
    bap_bil_pass_seq_iterator_t *iter = bap_bil_pass_seq_iterator_create(p);
    bap_bil_pass_t *c = bap_bil_pass_seq_iterator_next(iter);

    printf("%s:\n", prepend);
    while (c) {
        char *name = bap_bil_pass_name(c);
        printf("  - %s\n", name);
        c = bap_bil_pass_seq_iterator_next(iter);
    };
    printf("\n");

    bap_release(p);
    bap_release(iter);
    bap_release(c);
}

int main(int argc, const char **argv) {
    //init caml runtime, init IO
    bap_init(argc, argv);

    // load BAP plugins
    if (bap_load_plugins() < 0) {
        fprintf(stderr, "Failed to load BAP plugins\n");
        return 1;
    }

    // print already registered pass
    print_passes("registered passes");

    // registered a new one and print again
    bap_bil_pass_register("my-pass", my_pass, NULL, NULL);
    print_passes("updated passes");

    // select all avalable passes
    bap_bil_pass_seq_t *p = bap_bil_pass_passes();
    bap_bil_pass_select(p);

    // create project
    bap_project_input_t *input = bap_project_input_file((char *)argv[1], NULL);
    bap_project_t *proj = bap_project_create(input, NULL);

    // release pointers obtained from BAP
    bap_release(p);
    bap_release(input);
    bap_release(proj);

    return 0;
}

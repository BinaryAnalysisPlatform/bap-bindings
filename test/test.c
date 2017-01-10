#include <stdio.h>
#include <string.h>

#include "bap.h"


int bap_standalone_init(int, const char **);


void print_sub_names(bap_sub_seq_t *subs) {
    bap_sub_seq_iterator_t *iter = bap_sub_seq_iterator_create(subs);
    while (bap_sub_seq_iterator_has_next(iter)) {
        bap_sub_t *sub = bap_sub_seq_iterator_next(iter);
        char *name = bap_sub_name(sub);
        char *tnam = bap_term_name(sub);
        printf("%s: %s\n", name, tnam);
        bap_free(sub);
        bap_free(name);
        bap_free(tnam);
    }
    bap_free(iter);
}


int main(int argc, const char **argv) {
    int res = bap_standalone_init(argc, argv);
    if (res < 0) {
        printf("Failed to initialize BAP\n");
        return 1;
    }

    printf("BAP.%s was succesfully initialized\n", bap_version());

    struct bap_project_parameters_t params = {0};
    params.bap_project_rooter = bap_rooter_factory_find("byteweight");

    if (params.bap_project_rooter == NULL) {
        printf("Warning: byteweight is not installed\n");
    }

    bap_project_input_t * input = bap_project_input_file("/bin/test", NULL);
    bap_project_t *proj = bap_project_create(input, &params);

    if (!proj) {
        printf("failed to create a project: %s\n", bap_error_get());
        return 2;
    }
    printf("created a project\n");

    bap_arch_tag arch = bap_project_arch(proj);
    char *name = bap_arch_to_string(arch);

    printf("Architecture: %s\n", name);

    bap_program_t *prog = bap_project_program(proj);
    printf("Program:\n%s\n", bap_program_to_string(prog));

    bap_sub_seq_t *subs = bap_program_subs(prog);
    print_sub_names(subs);

    printf("That's all folks\n"); fflush(stdout);

    bap_free(proj);
    bap_free(name);
    return 0;
}

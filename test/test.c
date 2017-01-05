#include <stdio.h>

#include "bap.h"


int one() {return 1;}
int two() {return 2;}


int main(int argc, const char *argv) {
    int res = bap_standalone_init(argc, argv);
    if (res < 0) {
        printf("Failed to initialize BAP\n");
        return 1;
    }

    printf("BAP.%s was succesfully initialized\n", bap_version());

    bap_project_t proj =
        bap_project_create(bap_project_input_file("/bin/true", NULL));

    if (!proj) {
        printf("failed to create a project: %s\n", bap_error_get());
        return 2;
    }
    printf("created a project\n");

    bap_arch_t arch = bap_project_arch(proj);

    printf("Architecture: %s\n", bap_arch_to_string(arch));

    return 0;
}

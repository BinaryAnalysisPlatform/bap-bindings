#include <stdio.h>
#include <string.h>

#include "bap.h"


int one() {return 1;}
int two() {return 2;}

int bap_standalone_init(int, const char **);


int main(int argc, const char **argv) {
    int res = bap_standalone_init(argc, argv);
    if (res < 0) {
        printf("Failed to initialize BAP\n");
        return 1;
    }

    printf("BAP.%s was succesfully initialized\n", bap_version());

    bap_project_t *proj =
        bap_project_create(bap_project_input_file("/bin/true", NULL));

    if (!proj) {
        printf("failed to create a project: %s\n", bap_error_get());
        return 2;
    }
    printf("created a project\n");

    bap_arch_t *arch = bap_project_arch(proj);
    char *name = bap_arch_to_string(arch);

    printf("Architecture: %s\n", name);
    printf("String %s(%p) is managed by OCaml\n", name, name);
    printf("and has length %d == %d\n", bap_strlen(name), (int) strlen(name));

    bap_free(arch);
    bap_free(proj);
    bap_free(name);

    return 0;
}

#include <stdio.h>

#include "bap.h"


int main(int argc, const char *argv) {
    int res = bap_standalone_init(argc, argv);
    if (res < 0) {
        printf("Failed to initialize BAP\n");
        return 1;
    }

    printf("BAP.%s was succesfully initialized\n", bap_version());
    return 0;
}

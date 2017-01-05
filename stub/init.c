#include <caml/callback.h>

int bap__standalone_init(int, const char **);

void bap_standalone_init(int argc, const char **argv)
{
    caml_startup((char **)argv);
    bap__standalone_init(argc, argv);
}

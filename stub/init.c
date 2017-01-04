#include <caml/callback.h>

void bap_standalone_init(int argc, const char *argv)
{
  caml_startup(argv);
  bap__standalone_init(argc, argv);
}

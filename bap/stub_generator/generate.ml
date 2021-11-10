open Format

let generate dirname =
  let prefix = "bap" in
  let path basename = Filename.concat dirname basename in
  let ml_fd = open_out (path "bap_bindings.ml") in
  let c_fd = open_out (path "bap.c") in
  let h_fd = open_out (path "bap.h") in
  let stubs = (module Bindings.Make : Cstubs_inverted.BINDINGS) in
  begin
    Cstubs_inverted.write_ml
      (formatter_of_out_channel ml_fd) ~prefix stubs;

    fprintf (formatter_of_out_channel c_fd)
      "#include \"bap.h\"@\n%a"
      (Cstubs_inverted.write_c ~prefix) stubs;

    fprintf (formatter_of_out_channel h_fd)
      "#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
#if __cplusplus >= 202002L
#define requires requires_
#endif
#ifndef __bool_true_false_are_defined
#include <cstdbool>
#endif

extern \"C\" {
#endif

%a
void bap_init(int argc, const char *argv[]);
int bap_init2(int argc, const char *argv[], struct bap_parameters_t *pars);

#ifdef __cplusplus
#if __cplusplus >= 202002L
#undef requires
#endif

}
#endif@\n%!"
      (Cstubs_inverted.write_c_header ~prefix) stubs;

  end;
  close_out h_fd;
  close_out c_fd;
  close_out ml_fd

let () = generate (Sys.argv.(1))

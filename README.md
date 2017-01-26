# BAP Bindings

This project provides a C interface to BAP library and other
components of the infrastructure. The interface is rather complete,
although some functions may be omitted for a reason or accidentaly.
By desing, everything that is possible to do in OCaml with the Bap.Std
interface should be possible to implement in C, using `bap.h`. If you
find any violations of this rule, please don't hesitate to file an
issue.


## Quick start

The following simple program is a good test that your installation
works fine.

```c
int main(int argc, const char *argv) {
    bap_init(argc, argv);
    printf("Welcome to BAP %s", bap_version());
}

```

The `examples` folder contains a set of small programs, that can be
viewed as tutorials to BAP and BAP bindings. The following order is
suggested:

1. lift_insn - teaches how to disassemble, lift and print code;
2. print_reachable - explains how to write simple analysis;
3. show_image - uncovers low-level details of file parsing.

## Requirements

* BAP 1.x.x
* OCaml 4.03+ with PIC runtime
* GCC
* Patience (it takes some time to generate the bindings)

The PIC runtime is usually installed with OCaml (at least in OPAM).


## Compilation and installation

Nothing new here:

```
./configure
make
make install
```

You can parametrize installation variables, e.g., prefix using the
configure script, see `./configure --help` for more information.

If the `configure` script is not available (e.g., when you just cloned
the repo), then use `autoconf` to generate it.


# Documentation

Lacks. The long story is the following. There is an official
searchable documentation for OCaml library. You should consult it as a
primary source of information. Although it is for OCaml, it should be
understandable without a dictionary. We were trying to follow some
conventions to make it easier to map C functions to their OCaml
counterpart.

1. Module namespaces are translated to the underscore delimited
   prefixes, with exception to Bap.Std that is translated just to
   `bap`, e.g., `Project.Input.file` is `project_input_file`. So to
   get the documentation for the `project_input_file` just type
   `Project.Input.file` in the search window of the OCaml
   documentation. The same is true for types. A type `Project.t` is
   defined as an opaque data structure `bap_project_t`. The name is
   typedefed into from the structure tag namespace to the symbol
   namespace, so no `struct` word is needed.

2. The order of function parameters is preserved, except that optional
   parameters (prefixed with `?` in OCaml) are specified after all
   required parameters. For example,
   `Project.Input.file ?loader:string -> filename:string -> t`
   is translated into `project_input_file(char *filename, char *loader)`.
   The `NULL` value can be passed as an argument to the optional parameter.

3. If there are too many optional parameters, then they are passed
   using a strcture. Field names of a structure correspond to the
   names of optional parameters. For example, `bap_project_create` corresponds
   to `Project.create` function that has the following interface:
    ```ocaml
    val create :
           ?disassembler:string ->
           ?brancher:brancher source ->
           ?symbolizer:symbolizer source ->
           ?rooter:rooter source ->
           ?reconstructor:reconstructor source ->
           input -> t Or_error.t
    ```
       In C land it corresponds to

    ```c
    struct bap_project_t* bap_project_create(struct bap_project_input_t* input,
                                             struct bap_project_parameters_t* params);
    ```

       Where structure `params` is defined as:

    ```c
    struct bap_project_parameters_t {
      bap_rooter_source_t* rooter;
      bap_brancher_source_t* brancher;
      bap_symbolizer_source_t* symbolizer;
      bap_reconstructor_source_t* reconstructor;
      char* disassember;
    };

    ```
   Each individual field of the params data structure can be
   `NULL`. Moreover, the params itself can be also `NULL`. That will denote
   that all optional arguments were omitted.

4. Nontotal functions (those that return `t Or_error.t` or `t option`
   instead of `t`) may return `NULL`. If the OCaml counterpart was
   returning a value of `t Or_error.t` then the error can be retrieved
   with `bap_error_get` function.


5. If a module `XXX` implements `Printable.S` interface, then the following
   functions are available:
   * `bap_XXX_to_string(bap_XXX_t *)` returns a string representation of a value;
   * `bap_fprint(bap_XXX_t *value, FILE *)` prints value into a stream;
   * `bap_print(bap_XXX_t *)` prints a value into stdout;
   * `bap_eprint(bap_XXX_t *)` prints a value into stderr;

6. If a module `XXX` implements `Data.S` interface, then the following
   functions are available:
   * `bap_XXX_data_version(void)`;
   * `bap_XXX_data_size(bap_XXX_t *)`;
   * `bap_XXX_copy(bap_XXX_t *value, char *data, int len)`;
   * `bap_XXX_of_bytes(char *data, int len)`;
   * `bap_XXX_input(const char *filename)`;
   * `bap_XXX_output(const char *filename)`;


6. If a module `XXX` implements `Data.S` interface, (that subsumes
   `Printable.S` and `Data.S`) then the following functions are
   also available:
   * `bap_XXX_hash(bap_XXX_t *)`;
   * `bap_XXX_equal(bap_XXX_t *x, bap_XXX_t *y)`;
   * `bap_XXX_compare(bap_XXX_t *x, bap_XXX_t *y)`;

7. Polymorphic operations are provided for certain type in a type-safe
   maner. A polymorphic operation `OP` defined in module `M`
   concretized for type `XXX` will be named `bap_XXX_M_OP` for
   example, `Seq.map` for a sequence of `insn` is named
   `bap_insn_seq_map`.  Currenlty, all Regular types provide two
   polymorphic containers: `set` and `seq`. The `set` is implemented
   using mutable hash table, and `seq` is a generic sequence (that can
   be even infinte).

8. Truly generic operatons, like `Seq.length` are provided as
   `bap_seq_length`, wher `bap_seq_t` is base class for all sequences
   (in the sense, `bap_XXX_seq_t` is an instance of `bap_seq_t` for
   all `XXX`). The instance relation is checked in runtime, a static
   cast is required at compile time.

#include <stdio.h>
#include <string.h>
#include <bap.h>

// This example will show how to create your own analysis, or a pass
// in BAP parlance. Start with the `lift_insn` example to get the
// basics.


// A pass can have dependencies, that will be run before
// the pass. These dependencies are not really needed for our analysis,
// so the are listed for the demonstration purposes only.
static char *deps[] = {"abi", "api", NULL};


// find a subroutine that has the given name
bap_sub_t *find_sub_by_name(bap_program_t *prog, const char *name) {
    // we can also use `length` and `nth` operations to iterate
    // over subterms.
    int terms = bap_program_sub_length(prog);

    for (int i = 0; i < terms; i++) {
        bap_sub_t *sub = bap_program_sub_nth(prog, i);
        char *subname = bap_sub_name(sub);
        int found = strcmp(subname, name) == 0;
        bap_release(subname);
        if (found) {
            return sub;
        } else {
            bap_release(sub);
        }
    }
    return NULL;
}

// this function will be run each time a pass is invoked.
// It may return NULL if the project structure wasn't modified,
// or it may return a pointer to a new, modified project.
static bap_project_t *print_reachable(bap_project_t *proj, void *data) {
    // in this simple analysis we will print all functions
    // that are reachable from the specified function

    // we could use gcc's fancy nested functions feature, and caught the
    // name of the function from a lexical scope, but for the purpose of
    // demonstration let's use the default C method of passing extra data
    // to a visitor, a voided pointer.
    const char *hay = (const char *)data;

    bap_program_t *prog = bap_project_program(proj);

    //bap_program_print(prog);
    bap_callgraph_t *callgraph = bap_program_to_graph(prog);


    bap_sub_t *main = find_sub_by_name(prog, hay);

    void print_node(bap_tid_t *tid, void *data) {
        // bap_tid_print will just print the tid number, but we want
        // something more fancy:
        char *name = bap_tid_name(tid);
        printf("\t%s\n", name);
        bap_release(name);
    }

    if (main) {
        bap_tid_t *main_tid = bap_term_tid((bap_term_t *)main);
        printf("called by %s:\n", hay);
        bap_callgraph_visit_reachable(callgraph, 0, main_tid, print_node, NULL);
        bap_release(main_tid);
    } else {
        printf("Can't find the function %s!\n", hay);
    }

    bap_release(main); // it's OK to pass NULL to bap_release
    bap_release(proj);
    bap_release(prog);
    return NULL;
}


// currently only a standalone mode is supported, so we will create a
// project and do all the stuff manually, later, it should be possible
// to write a plugin in C and dynamically load it, so that the bap
// frontend will take care of the overall pipeline for us.
// Of course, we can just run our pass directly, without registering
// it, but it will defeat the whole purpose of demonstation.
int main(int argc, const char **argv) {
    bap_init_error_t *er = bap_init2(argc, argv, NULL);

    if (er) {
        printf("failed to initialize BAP: %s\n", bap_error_get());
        return 1;
    }

    if (argc < 3) {
        fprintf(stderr, "Usage: %s <filename> <subname>\n", argv[0]);
        return 1;
    }

    struct bap_pass_t pass = {0};
    pass.deps = deps;
    pass.run = &print_reachable;

    bap_project_pass_register(&pass, (void *)argv[2]);


    struct bap_project_parameters_t params = {0};

    bap_project_input_t *input = bap_project_input_file((char *)argv[1], "llvm");
    bap_project_t *proj = bap_project_create(input, &params);

    if (!proj) {
        fprintf(stderr, "Failed to create a project: %s", bap_error_get());
        return 1;
    }


    // if a pass name is omitted, then it defaults to the basename of
    // the program.

    proj = bap_project_pass_run(proj, "print_reachable");

    if (!proj) {
        fprintf(stderr, "%s\n", bap_error_get());
        return 1;
    }

    bap_release(params.rooter);
    bap_release(params.symbolizer);
    bap_release(input);
    bap_release(proj);
    return 0;
}

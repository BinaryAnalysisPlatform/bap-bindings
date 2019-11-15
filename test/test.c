#include <assert.h>
#include <stdio.h>
#include <string.h>

#include <inttypes.h>

#include "bap.h"

void print_sub_names(bap_sub_seq_t *subs) {
    bap_sub_seq_iterator_t *iter = bap_sub_seq_iterator_create(subs);
    while (bap_sub_seq_iterator_has_next(iter)) {
        bap_sub_t *sub = bap_sub_seq_iterator_next(iter);
        char *name = bap_sub_name(sub);
        bap_word_t *addr = bap_term_get_address((bap_term_t *)sub);
        if (addr) {
            assert(bap_word_fits_into_int64(addr));
            printf("%"PRIX64": %s\n", bap_word_to_int64(addr), name);
        }
        bap_release(sub);
        bap_release(name);
        bap_release(addr);
    }
    bap_release(iter);
}

bap_project_t *dead_taint_analysis(bap_project_t *proj, void *unused) {
    bap_program_t *prog = bap_project_program(proj);
    bap_sub_seq_t *subs = bap_program_subs(prog);
    bap_sub_seq_iterator_t *subi = bap_sub_seq_iterator_create(subs);

    while (bap_sub_seq_iterator_has_next(subi)) {
        bap_sub_t *sub = bap_sub_seq_iterator_next(subi);
        bap_blk_seq_t *blks = bap_sub_blks(sub);
        bap_blk_seq_iterator_t *blki = bap_blk_seq_iterator_create(blks);

        while (bap_blk_seq_iterator_has_next(blki)) {
            bap_blk_t *blk = bap_blk_seq_iterator_next(blki);
            bap_jmp_seq_t *jmps = bap_blk_jmps(blk);
            bap_jmp_seq_iterator_t *jmpi = bap_jmp_seq_iterator_create(jmps);

            while (bap_jmp_seq_iterator_has_next(jmpi)) {
                bap_jmp_t *jmp = bap_jmp_seq_iterator_next(jmpi);
                if (bap_term_has_dead((bap_term_t *)jmp)) {
                    bap_insn_t *insn = bap_term_get_insn((bap_term_t *)jmp);
                    if (insn) {
                        char *code = bap_insn_asm(insn);
                        printf("unchecked %s\n", code);
                        bap_release(code);
                    }
                    bap_release(insn);
                }
                bap_release(jmp);
            }
            bap_release(jmps);
            bap_release(blk);
        }
        bap_release(blki);
        bap_release(blks);
        bap_release(sub);
    }
    bap_release(subi);
    bap_release(subs);
    bap_release(prog);
    return proj;
}


int main(int argc, const char **argv) {
    bap_init_error_t *er = bap_init2(argc, argv, NULL);

    if (er) {
        printf("failed to initialize BAP: %s\n", bap_error_get());
        return 1;
    }

    printf("BAP.%s was succesfully initialized\n", bap_version());

    bap_project_input_t * input = bap_project_input_file("/bin/true", "llvm");
    bap_project_t *proj = bap_project_create(input, NULL);

    if (!proj) {
        printf("failed to create a project: %s\n", bap_error_get());
        return 2;
    }
    printf("created a project\n");

    bap_arch_t arch = bap_project_arch(proj);
    char *name = bap_arch_to_string(arch);

    printf("Architecture: %s\n", name);

    bap_program_t *prog = bap_project_program(proj);
    printf("Program:\n");
    bap_program_print(prog);

    bap_sub_seq_t *subs = bap_program_subs(prog);
    print_sub_names(subs);

    struct bap_pass_t pass = {0};
    char *deps[] = {"abi", "api", "warn-unused-mark", "propagate-taint", NULL};
    pass.deps = deps;
    pass.name = "deadcode-analyzer";
    pass.run = &dead_taint_analysis;
    bap_project_pass_register(&pass, NULL);
    bap_project_t *final = bap_project_pass_run(proj, "test-deadcode-analyzer");
    if (!final) {
        printf("failed to run pass main: %s\n", bap_error_get());
        return 3;
    }
    printf("That's all folks\n"); fflush(stdout);

    bap_release(final);
    bap_release(proj);
    bap_release(name);
    return 0;
}

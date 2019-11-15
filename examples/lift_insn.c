#include <stdio.h>
#include <string.h>

#include <bap.h>

// In this example we will disassemble the following string:
char data[] = "\x48\x8d\x00\x48\x83\xec\x08\x48\x83\xc4\x08\xe8\x47\xee\xff\xff";


int main(int argc, const char **argv) {
    // a call to bap_init should precede any calls to bap
    bap_init_error_t *er = bap_init2(argc, argv, NULL);

    if (er) {
        printf("failed to initialize BAP: %s\n", bap_error_get());
        return 1;
    }

    // all OCaml values are visible as opaque structures in C
    bap_word_t *base = bap_word_of_int(32,0x80000);

    bap_memory_t *mem = bap_memory_create(BAP_ENDIAN_LITTLE,
                                          base, data, sizeof(data));

    printf("Will disassemble %d bytes\n", bap_memory_length(mem));

    // Non-total functions return NULL in case of error.
    if (!mem) {
        // If an error has occured, then it can be queried with bap_error_get:
        printf("Failed to create a memory chunk: %s\n", bap_error_get());
        return 1;
    }

    // disasm value contains the result of disassemblation
    bap_disasm_t *dis = bap_disasm_of_mem(BAP_ARCH_X86_64, mem, NULL);

    if (!dis) {
        printf("Can't disassemble data: %s\n", bap_error_get());
        return 1;
    }

    // Data is disassembled to a Control Flow Graph, but
    // lets start with just a linear list of instructions:
    bap_insn_seq_t *insns = bap_disasm_insns(dis);

    // The sequence module provides all regular functional
    // iterators like map, iter, etc. However, in C it is
    // easier to use Java-style iterators:
    bap_insn_seq_iterator_t *iter = bap_insn_seq_iterator_create(insns);
    while (bap_insn_seq_iterator_has_next(iter)) {
        bap_insn_t *insn = bap_insn_seq_iterator_next(iter);
        char *code = bap_insn_asm(insn);
        printf("%s\n", code);

        // now, let's look a the instruction semantics, we can use
        // bap_bil_to_string to get the string representation, but
        // instead we will output directly to the channel:
        bap_stmt_seq_t *bil = bap_insn_bil(insn);
        bap_stmt_seq_print(bil);
        printf("\n\n");

        // all pointers returned from BAP are pointing to values
        // managed by the BAP runtime. The garbage collector will
        // not touch they are released with a call to bap_release
        bap_release(insn);
        bap_release(bil);
        bap_release(code); // yes, including strings.
    }

    // now the boring stuff, of course we can omit it since we are
    // still exiting, but for the sake of excercise:
    bap_release(base);
    bap_release(mem);
    bap_release(dis);
    bap_release(insns);
    bap_release(iter);

    // As a final note, the order in which values are released doesn't
    // really matter, as the release doesn't necessary invalidate the
    // value. If the runtime has a reference to it will not be
    // removed. The `bap_release(p)` statement means, that the C
    // program do not need the value pointed by `p` and the value of
    // `p` will not be used anymore. That means, for example, that
    // values suchs as `base` and `mem` can be safely released as soon
    // as they are passed to `bap_disasm_of_mem`
}

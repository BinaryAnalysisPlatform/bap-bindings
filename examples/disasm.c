#include <stdio.h>

#include <bap.h>


// This example demonstrates the low-level disassembler API, that
// allows you to disassemble individual instructions with a minimized
// overhead. This interface allows you to use BAP as a simple
// disassembler (that can also provide instruction semantics).  See
// `lift_insn.c` and `print_reachable.c` for more high-level
// interfaces.
//
// The example assumes, that you already have a basic understanding of
// the API (obtained from other examples).

char data[] = "\x48\x8d\x00\x48\x83\xec\x08\x48\x83\xc4\x08\xe8\x47\xee\xff\xff";


int main(int argc, const char **argv) {
    bap_init_error_t *er = bap_init2(argc, argv, NULL);

    if (er) {
        printf("failed to initialize BAP: %s\n", bap_error_get());
        return 1;
    }

    // this will create a handler to a disassembler, the
    // bap_disasm_basic_create_expert allows you to control every detail of a
    // disassembler.
    bap_disasm_basic_t *dis = bap_disasm_basic_create(BAP_ARCH_X86_64);

    if (!dis) {
        fprintf(stderr, "can't create a disassembler: %s\n", bap_error_get());
    }

    uint64_t base = 0x80000L;
    const int len = sizeof(data) - 1;

    for (int pos = 0; pos < len; ) {
        // code is an opaque structure that contains a pointer to instruction
        // code, the instruction itself, and BIL semantics.
        bap_code_t *code =
            bap_disasm_basic_next(dis, &data[pos], len - pos, base+pos);
        if (!code) {
            fprintf(stderr, "can't disassemble instruction: %s\n", bap_error_get());
            return 1;
        }

        pos += bap_code_length(code);
        bap_code_print(code);
        printf("\n");
        bap_release(code);
    }

    bap_disasm_basic_close(dis);
}

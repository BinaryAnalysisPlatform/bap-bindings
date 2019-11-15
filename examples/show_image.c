#include <stdio.h>
#include <bap.h>

// In this example we will show how to work the image interface of
// bap. This interface is sort of low-level, and usually you don't
// need to use it, as the project interface will take care of file
// parsing for you. Please, refer to the print_reachable example
// for more information.

int main(int argc, const char **argv) {
    int r = bap_init2(argc, argv, NULL);

    if (r) {
        printf("failed to initialize BAP: %s\n", bap_error_get());
        return 1;
    }

    if (argc < 2) {
        fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
        return 1;
    }


    bap_image_t *img = bap_image_create((char *)argv[1], NULL);

    if (!img) {
        fprintf(stderr, "Failed to read file: %s\n", bap_error_get());
        return 1;
    }

    void print_segment(bap_segment_t *seg, void *data) {
        printf("Segment ");
        bap_segment_print(seg);
        bap_memory_t *mem = bap_image_memory_of_segment(img, seg);
        printf(":\nStarts: ");
        bap_word_t *start = bap_memory_min_addr(mem);
        bap_word_print(start);
        printf("\nLength: %d\n", bap_memory_length(mem));
        bap_release(start);
        bap_release(mem);
        bap_release(seg); // should we release a seg automatically
                          // or it would be too much?
    }

    bap_segment_seq_t *segs = bap_image_segments(img);
    bap_segment_seq_iter(segs, &print_segment, NULL);
    bap_release(segs);
    bap_release(img);
    return 0;
}

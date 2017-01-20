BUILDDIR=../_build/examples
$(shell mkdir -p $(BUILDDIR))

EXAMPLES=$(BUILDDIR)/lift_insn.native \
	 $(BUILDDIR)/print_reachable.native \
	 $(BUILDDIR)/show_image.native

all: $(EXAMPLES)

clean:
	rm -rf $(BUILDDIR)

$(BUILDDIR)/%.native: $(BUILDDIR)/%.o
	gcc  -o $@ $< -L$(BUILDDIR)/.. -lbap

$(BUILDDIR)/%.o: %.c
	gcc  -Werror -std=gnu99 -c -o $@ -I $(BUILDDIR)/../generated  $<

.PHONY: all clean

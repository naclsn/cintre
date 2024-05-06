## example:
# prog     := simple
# entries  := simple.c cintre/standard.h
# objs     := simple.o
# include driver.makefile # rules: all, $(build)/simple, clean

cintre := cintre
build ?= build

PR := $(build)/preparer.exe
CFLAGS := -O2

#---

$(build)/$(prog): $(build)/c-$(prog).c $(addprefix $(build)/,$(objs)); $(CC) $^ -o $@ $(CFLAGS) -I. -I$(cintre) -DUSE_READLINE $(shell pkg-config readline --cflags --libs)
$(build)/c-$(prog).c: $(patsubst %,$(build)/a-%.h,$(basename $(notdir $(entries)))); $(PR) -m $^ -o $@

# a-entry.h: some/entry.ch $(PR); $(PR) $< -o $(CFLAGS-a-entry)
$(foreach e,$(entries),$(eval $(build)/a-$(basename $(notdir $(e))).h: $(e) $$(PR); $$(PR) $$< -o $$@ $(CFLAGS) $$(CFLAGS-$$(basename $$@))))

$(PR): $(cintre)/preparer.c $(cintre)/*.h; $(CC) $< -o $@ $(CFLAGS)
.PRECIOUS: $(build)/a-%.h $(build)/%.o $(PR)

clean:; $(RM) $(build)/a-*.h $(build)/c-$(prog).c $(build)/*.o
.PHONY: all clean

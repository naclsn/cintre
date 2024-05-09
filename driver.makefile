## example:
# prog     := simple
# entries  := simple.c
# objs     := simple.o
# include driver.makefile # rules: all, $(build)/simple, clean

# TODO: options like -nostd and -norl; maybe: `opts := -nostd` and `$(findstring ..)`

cintre := cintre
build ?= build

PR := $(build)/preparer.exe
CFLAGS := -O2

#---

$(build)/$(prog): $(addprefix $(build)/,$(objs)) $(build)/c-$(prog).c; $(CC) $^ -o $@ $(CFLAGS) -I. -I$(cintre) -DUSE_READLINE $(shell pkg-config readline --cflags --libs)
$(build)/c-$(prog).c: $(build)/a-standard.h $(patsubst %,$(build)/a-%.h,$(basename $(notdir $(entries)))); $(PR) -m $^ -o $@
$(build)/a-standard.h: cintre/standard.h $(PR); $(PR) $< -o $@ $(CFLAGS-a-standard)

# build/a-entry.h: some/entry.ch $(PR); $(PR) $< -o $@ $(CFLAGS-a-entry)
$(foreach e,$(entries),$(eval $(build)/a-$(basename $(notdir $(e))).h: $(e) $$(PR); $$(PR) $$< -o $$@ $(CFLAGS) $$(CFLAGS-$$(basename $$@))))

$(PR): $(cintre)/preparer.c $(cintre)/*.h; $(CC) $< -o $@ $(CFLAGS)
.PRECIOUS: $(build)/a-%.h $(build)/%.o $(PR)

clean:; $(RM) $(build)/a-*.h $(build)/c-$(prog).c $(build)/*.o
.PHONY: all clean

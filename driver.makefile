## example:
# prog     := simple
# entries  := simple.c
# objs     := simple.o
# obps     := # supported: -nostd (disables standard.h), -norl (disables readline)
# include driver.makefile # rules: $(build)/simple, clean-simple

cintre := cintre
build ?= build

PR := $(build)/preparer.exe
CFLAGS := -O2

#---

_nostd = $(findstring -nostd,$(opts))
_norl = $(findstring -norl,$(opts))

$(build)/$(prog): $(addprefix $(build)/,$(objs)) $(build)/c-$(prog).c; $(CC) $^ -o $@ $(CFLAGS) -I. -I$(cintre) $(if $(_nostd),,-lc -lm) $(if $(_norl),,-DUSE_READLINE $(shell pkg-config readline --cflags --libs))
$(build)/c-$(prog).c: $(if $(_nostd),,$(build)/a-standard.h) $(patsubst %,$(build)/a-%.h,$(basename $(notdir $(entries)))); $(PR) -m $^ -o $@
$(build)/a-standard.h: cintre/standard.h $(PR); $(PR) $< -Pno-emit-decl -Pno-emit-incl -o $@ $(CFLAGS-a-standard)

# build/a-entry.h: some/entry.ch $(PR); $(PR) $< -o $@ $(CFLAGS-a-entry)
$(foreach e,$(entries),$(eval $(build)/a-$(basename $(notdir $(e))).h: $(e) $$(PR); $$(PR) $$< -o $$@ $(CFLAGS) $$(CFLAGS-$$(basename $$@))))

$(PR): $(cintre)/preparer.c $(cintre)/*.h; $(CC) $< -o $@ $(CFLAGS)
.PRECIOUS: $(build)/a-%.h $(build)/%.o $(PR)

clean-$(prog):; $(RM) $(build)/a-*.h $(build)/c-$(prog).c $(build)/*.o
.PHONY: clean-$(prog)

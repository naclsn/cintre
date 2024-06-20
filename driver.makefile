## example:
# prog    := simple
# entries := somewhere/simple.c
# objs    := $(build)/simple.o
# opts    := # supported: -nostd (disables standard.h), -norl (disables readline)
# include driver.makefile
#
# - it defines the rules `build/simple` and `clean-simple`
# - it will not try to build the objects, this is left to user
# - for entry-specific CFLAGS, define `$(CFLAGS-a-..)` (so in this example would be `$(CFLAGS-a-simple)`)
# - use `$(LDFLAGS)` for linking

build ?= build
# (this file's dir)/cintre
cintre ?= $(dir $(lastword $(MAKEFILE_LIST)))cintre

# --- soupe

PR := $(build)/preparer.exe
_CFLAGS ?= -O2 -std=c99

build/a-headers := $(if $(findstring -nostd,$(opts)),,$(build)/a-standard.h) $(patsubst %,$(build)/a-%.h,$(basename $(notdir $(entries))))
build/objs := $(objs)
build/c-prog := $(build)/c-$(prog)

ifneq (-norl,$(findstring -norl,$(opts)))
_rl_cflags := -DUSE_READLINE $(shell pkg-config readline --cflags)
_rl_libs := $(shell pkg-config readline --libs)
endif

$(build)/$(prog): $(build/objs) $(build/c-prog).o $(build)/cintre.o; $(CC) $^ -o $@ $(LDFLAGS) -lc -lm $(_rl_libs)

$(build/c-prog).c: $(build/a-headers) $(PR); $(PR) -m $(build/a-headers) -o $@
$(build/c-prog).o: $(build/c-prog).c; $(CC) -c $< -o $@ $(_CFLAGS) -I. -I$(cintre)

$(build)/cintre.o: $(cintre)/cintre.c $(cintre)/*.h; $(CC) -c $< -o $@ $(_CFLAGS) $(_rl_cflags)
$(build)/a-standard.h: $(cintre)/standard.h $(PR); $(PR) $< -Pno-emit-decl -Pno-emit-incl -o $@ $(CFLAGS-a-standard)

# build/a-entry.h: some/entry.ch $(PR); $(PR) $< -o $@ $(CFLAGS-a-entry)
$(foreach e,$(entries),$(eval $(build)/a-$(basename $(notdir $(e))).h: $(e) $$(PR); $$(PR) $$< -o $$@ $$(CFLAGS-$$(basename $$(notdir $$@)))))

.DELETE_ON_ERROR:

$(PR): $(cintre)/preparer.c $(cintre)/*.h; $(CC) $< -o $@ $(_CFLAGS)
.PRECIOUS: $(build)/a-%.h $(build)/%.o $(PR)

clean-$(prog):; $(RM) $(build/a-headers) $(build/c-prog).c $(build/objs) $(build/c-prog).o $(build)/cintre.o $(PR)
.PHONY: clean-$(prog)

## example:
# prog    := simple
# entries := simple.c
# objs    := simple.o
# opts    := # supported: -nostd (disables standard.h), -norl (disables readline)
# include driver.makefile
#
# - it defines the rules `build/simple` and `clean-simple`
# - it will not try to build the objects, this is left to user (in `build/..`)
# - to change the build directory, define `$(build)`
# - for entry-specific CFLAGS, define `$(CFLAGS-a-..)` (so in this example `$(CFLAGS-a-simple)`)

build ?= build
CFLAGS ?= -O2

# (this file's dir)/cintre
cintre ?= $(dir $(lastword $(MAKEFILE_LIST)))cintre

PR := $(build)/preparer.exe

#---

build/a-headers := $(if $(findstring -nostd,$(opts)),,$(build)/a-standard.h) $(patsubst %,$(build)/a-%.h,$(basename $(notdir $(entries))))
build/objs := $(addprefix $(build)/,$(objs))
build/c-prog := $(build)/c-$(prog)

ifneq (-norl,$(findstring -norl,$(opts)))
_rl_cflags := -DUSE_READLINE $(shell pkg-config readline --cflags)
_rl_libs := $(shell pkg-config readline --libs)
endif

$(build)/$(prog): $(build/objs) $(build/c-prog).o $(build)/cintre.o; $(CC) $^ -o $@ $(CFLAGS) $(LDFLAGS) -lc -lm $(_rl_libs)

$(build/c-prog).c: $(build/a-headers) $(PR); $(PR) -m $(build/a-headers) -o $@
$(build/c-prog).o: $(build/c-prog).c; $(CC) -c $< -o $@ $(CFLAGS) -I. -I$(cintre)

$(build)/cintre.o: $(cintre)/cintre.c $(cintre)/*.h; $(CC) -c $< -o $@ $(CFLAGS) $(_rl_cflags)
$(build)/a-standard.h: $(cintre)/standard.h $(PR); $(PR) $< -Pno-emit-decl -Pno-emit-incl -o $@ $(CFLAGS-a-standard)

# build/a-entry.h: some/entry.ch $(PR); $(PR) $< -o $@ $(CFLAGS-a-entry)
$(foreach e,$(entries),$(eval $(build)/a-$(basename $(notdir $(e))).h: $(e) $$(PR); $$(PR) $$< -o $$@ $(CFLAGS) $$(CFLAGS-$$(basename $$@))))

.DELETE_ON_ERROR:

$(PR): $(cintre)/preparer.c $(cintre)/*.h; $(CC) $< -o $@ $(CFLAGS)
.PRECIOUS: $(build)/a-%.h $(build)/%.o $(PR)

clean-$(prog):; $(RM) $(build/a-headers) $(build/c-prog).c $(build/objs) $(build/c-prog).o $(build)/cintre.o $(PR)
.PHONY: clean-$(prog)

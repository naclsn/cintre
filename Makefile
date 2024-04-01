warnings = -Wall -Wextra -Wpedantic -Werror -Wno-unused-function -Wno-unused-variable -Wno-unused-value -Wno-unused-parameter
override CFLAGS += -ggdb -O1 -std=c99 $(warnings)

PR = build/preparer.exe

ifneq ($(OS), Windows_NT)
readline = $(shell pkg-config readline --cflags --libs)
ifneq (,$(readline))
readline += -DUSE_READLINE
endif
endif

# TODO: maybe pattern, also streamline for multiple user files..
build/c-main: build/simple.o build/c-main.c; $(CC) $^ -o $@ $(CFLAGS) -Icintre -I. $(readline)
build/c-main.c: build/a-simple.h build/a-standard.h; $(PR) -m $^ -o $@
build/a-simple.h: simple.c $(PR); $(PR) $< -o $@ $(CFLAGS)
build/simple.o: simple.c;      $(CC) -c $< -o $@ $(CFLAGS)

# (by itself, just a C-like expression interpreter)
build/cintre: build/c-cintre.c cintre/*.[ch]; $(CC) $< -o $@ $(CFLAGS) -Icintre -I. $(readline)
build/c-cintre.c: build/a-standard.h; $(PR) -m $^ -o $@
build/a-standard.h: cintre/standard.h $(PR); $(PR) $< -o $@
$(PR): cintre/preparer.c cintre/*.h; $(CC) $< -o $@ $(CFLAGS)
.PRECIOUS: build/a-%.h build/%.o $(PR)

test: test/???*; for it in $^; do ./$$it check && printf "\x1b[32m$$it success\x1b[m\n" || printf "\x1b[31m$$it failure\x1b[m\n"; done
testup: test/???*; for it in $^; do ./$$it update; done
.PHONY: test testup

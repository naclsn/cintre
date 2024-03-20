warnings = -Wall -Wextra -Wpedantic -Werror -Wno-unused-function -Wno-unused-variable -Wno-unused-value -Wno-unused-parameter
CFLAGS += -ggdb -O1 -std=c99 $(warnings)

PR = build/preparer.exe

readline = $(shell pkg-config readline --cflags --libs)
ifneq (,$(readline))
readline += -DUSE_READLINE
endif

# TODO: (4 effective lines) maybe pattern, also streamline for multiple user files..
build/c-simple: build/simple.o build/a-simple.c; $(CC) $^ -o $@ $(CFLAGS) -Icintre

build/a-simple.c: simple.c $(PR); $(PR) $< -o $@ $(CFLAGS)
build/simple.o: simple.c;      $(CC) -c $< -o $@ $(CFLAGS)

.PRECIOUS: build/a-%.c build/%.o

$(PR): cintre/preparer.c cintre/*.h; $(CC) $< -o $@ $(CFLAGS)
# (by itself, just a C-like expression interpreter)
build/cintre: cintre/cintre.c cintre/*.h; $(CC) $< -o $@ $(CFLAGS) $(readline)

test: test/???*; for it in $^; do ./$$it check; done
testup: test/???*; for it in $^; do ./$$it update; done
.PHONY: test testup

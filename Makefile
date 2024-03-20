warnings = -Wall -Wextra -Wpedantic -Werror -Wno-unused-function -Wno-unused-variable -Wno-unused-value -Wno-unused-parameter
CFLAGS += -ggdb -O1 -std=c99 $(warnings)

PR = build/preparer.exe

build/c-simple: build/simple.o build/a-simple.c; $(CC) $^ -o $@ $(CFLAGS) -Icintre

build/a-simple.c: simple.c $(PR); $(PR) $< -o $@ $(CFLAGS)
build/simple.o: simple.c;      $(CC) -c $< -o $@ $(CFLAGS)

.PRECIOUS: build/a-%.c build/%.o

$(PR): cintre/preparer.c cintre/*.h; $(CC) $< -o $@ $(CFLAGS)

test: test/???*; for it in $^; do ./$$it check; done
testup: test/???*; for it in $^; do ./$$it update; done
.PHONY: test testup

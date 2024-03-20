warnings = -Wall -Wextra -Wpedantic -Werror -Wno-unused-function -Wno-unused-variable -Wno-unused-value -Wno-unused-parameter
CFLAGS += -ggdb -std=c99 $(warnings)

build/preparer: preparer/*.[ch]; $(CC) $^ -o $@ $(CFLAGS)
build/cintre: cintre.c preparer/*.h; $(CC) $< -o $@ $(CFLAGS)

warnings = -Wall -Wextra -Wpedantic -Werror -Wno-unused-function -Wno-unused-variable -Wno-unused-value -Wno-unused-parameter
CFLAGS += -ggdb -std=c99 $(warnings)

build/cintre: cintre.c common.h dyarr.h lexer.h; $(CC) $< -o $@ $(CFLAGS)

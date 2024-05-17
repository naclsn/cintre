warnings := -Wall -Wextra -Wpedantic -Werror
override CFLAGS := -ggdb -O0 -DLOC_NOTIF -std=c99 $(warnings) $(CFLAGS)

prog := cintre
include driver.makefile

tests =         \
    tok-stream  \
    decl-list   \
    expr-tree   \
    type-check  \
    comp-code   \
    prep-adapt  \

test-%:; test/run test/$* check && printf '\x1b[32m$@ success\x1b[m\n' || printf "\x1b[31m$@ $$? failure(s)\x1b[m\n"
testup-%:; test/run test/$* update
cover-%:; test/run test/$* cover
test: $(addprefix test-,$(tests))
testup: $(addprefix testup-,$(tests))
cover: $(addprefix cover-,$(tests))

$(build)/coverage.html: clean cover; gcovr -f cintre --html $@
$(build)/coverage-details.html: clean cover; gcovr -f cintre --html-details $@

clean: clean-cintre; $(RM) $(foreach t,$(tests),build/$(t)*)
.PHONY: test testup cover clean

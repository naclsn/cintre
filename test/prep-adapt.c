#include "run"

#define main(c, v) _preparer_main(c, v)
# include "../cintre/preparer.c"
#undef main

#define main(c, v) _decl_list_main(c, v)
#define run_test(f) decl_list(f)
# include "decl-list.c"
#undef run_test
#undef main

#include <sys/wait.h>
#include <unistd.h>

void run_test(char* file) {
    // bacially: `$ pr file -o- | decl-list -`
    int r_w[2], c;
    if (pipe(r_w) < 0) exitf("Could not pipe");
    if ((c = fork()) < 0) exitf("Could not fork");

    if (!c) {
        dup2(r_w[1], STDOUT_FILENO); // stdout to write end
        close(r_w[0]); // unused read end
        close(r_w[1]);

        do_prepare(1, (char*[]){file, NULL});

        exit(0);
    }

    else {
        dup2(r_w[0], STDIN_FILENO); // read end to stdin
        close(r_w[0]);
        close(r_w[1]); // unused write end

        decl_list("-");

        wait(NULL);
    }
}

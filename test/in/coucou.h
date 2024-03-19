#define _HERE_STR(__ln) #__ln
#define _HERE_XSTR(__ln) _HERE_STR(__ln)
#define HERE __FILE__ ":" _HERE_XSTR(__LINE__)
#define exitf(...) (notif(__VA_ARGS__), exit(1))
#define notif(...) (fprintf(stderr, HERE ": " __VA_ARGS__), fputc('\n', stderr))

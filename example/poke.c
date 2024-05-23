struct dec_entry {
    enum types {
        NONE,
        NORMAL,
        FIRE,
        WATER,
        GRASS,
        FLYING,
        GHOST,
        ELECTRIC,
    } types[2];
    char const* name;
    struct {
        unsigned at;
        struct dec_entry const* into;
    } evo;
} dec[] = {
    [0]= {NORMAL,   NONE,     "Bidoof",       15, dec+1},
    [1]= {NORMAL,   NONE,     "Bibarel",      },
    [2]= {GHOST,    FLYING,   "Drifloon",     28, dec+3},
    [3]= {GHOST,    FLYING,   "Drifblim",     },
    [4]= {ELECTRIC, NONE,     "Shinx",        15, dec+5},
    [5]= {ELECTRIC, NONE,     "Luxio",        30, dec+6},
    [6]= {ELECTRIC, NONE,     "Luxray",       },
};

typedef struct mon {
    struct dec_entry const* s;
    char const* cname;
    unsigned level;
} mon_t;

char const* const typenames[] = {
    "NONE",
    "NORMAL",
    "FIRE",
    "WATER",
    "GRASS",
    "FLYING",
    "GHOST",
    "ELECTRIC",
};

static char const* _a_an(char const* const c)
{
    char* strchr(char const*, int);
    return strchr("aeiou", c[0]|32) ? "a" : "an";
}
#define _a_an_fmt(__c)  _a_an((__c)), (__c)

void levelup(struct mon* const self)
{
    int printf(char const* restrict, ...);

    char const* const dname = self->cname ? self->cname : self->s->name;
    printf("%s gained a level!\n", dname);

    if (self->s->evo.at <= ++self->level) {
        self->s = self->s->evo.into;
        printf("%s evolved into a %s\n", dname, self->s->name);
    }
}

void summary(mon_t const* const self)
{
    int printf(char const* restrict, ...);

    if (!self->cname) printf("%s:\n", self->s->name);
    else printf("%s (%s %s):\n", self->cname, _a_an_fmt(self->s->name));

    printf("   level: %u\n", self->level);
    if (self->s->evo.into)
    printf("   until evolution into %s %s: %u\n", _a_an_fmt(self->s->evo.into->name), self->s->evo.at - self->level);
}

void decinfo(struct dec_entry const* const self)
{
    int printf(char const* restrict, ...);

    printf("%s:\n", self->name);

    printf("   type%s: %s", self->types[1] ? "s" : "", typenames[self->types[0]]);
    if (self->types[1]) printf(" and %s\n", typenames[self->types[0]]);
    else printf("\n");

    if (self->evo.into) printf("   evolves into %s at level %u", self->evo.into->name, self->evo.at);
}

void declist(void)
{
    int printf(char const* restrict, ...);
    for (unsigned k = 0; k < sizeof dec / sizeof*dec; k++) decinfo(dec+k);
}

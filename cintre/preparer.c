// big ol' TODO: all of it
// - typedef
// - struct/enum/union
// - static vars

#include "common.h"
#include "parser.h"

lex_state ls = {0};
FILE* result = NULL;

struct {
    dyarr(struct seen_fun { bufsl name; }) fun;
    dyarr(struct seen_var { bufsl name; }) var;
} seen = {0};

#define errdie(...) exitf("preparer: %s:%zu: " __VA_ARGS__, ls.file, ls.line)

#define _linked_it_type_comp struct decl_type_field
#define _linked_it_type_fun struct decl_type_param
#define for_linked(__info, __ty) for (_linked_it_type_##__ty* curr = __info.__ty.first; curr; curr = curr->next)

void emit_type(struct decl_type cref type);
void emit_adpt_type(struct decl_type cref type);
void emit_decl(declaration cref decl);

// emit specifics {{{
void _emit_comp(declaration cref decl) {
    if (KIND_STRUCT != decl->type.kind)
        errdie("NIY (union declaration)");

    fprintf(result, "struct ");
    if (decl->type.name.len) {
        fprintf(result, "%.*s ", bufmt(decl->type.name));
        if (0 == decl->type.info.comp.count)
            errdie("NIY (forward declaration for struct)");
    }

    fprintf(result, "{\n");
    for_linked (decl->type.info,comp) {
        fprintf(result, "    ");
        emit_type(&curr->decl->type);
        fprintf(result, "%.*s", bufmt(curr->decl->name)); // FIXME: function pointers and arrays members
        if (curr->bitw) fprintf(result, " :%.*s", bufmt(curr->bitw->info.atom)); // TODO: expression might be more complicated (rarely tho)
        fprintf(result, ";\n");
    }
    if (SPEC_TYPEDEF == decl->spec)
        fprintf(result, "} %.*s;\n", bufmt(decl->name));
    else
        fprintf(result, "};\n");

    if (decl->type.name.len) {
        fprintf(result, "static struct adpt_type const struct_%.*s_adapt_type = {\n", bufmt(decl->type.name));
        // TODO: add to correct namespace
    } else if (SPEC_TYPEDEF == decl->spec) {
        fprintf(result, "static struct adpt_type const %.*s_adapt_type = {\n", bufmt(decl->name));
        // TODO: add to correct namespace
    } else errdie("NIY (static annon struct variable declaration)");

    fprintf(result, "    .size= sizeof("); emit_type(&decl->type); fprintf(result, "), .align= alignof("); emit_type(&decl->type); fprintf(result, "),\n");
    fprintf(result, "    .tyty= TYPE_STRUCT,\n");
    fprintf(result, "    .info.comp= {\n");
    fprintf(result, "        .fields= (struct adpt_comp_field[]){\n");
    size_t count = 0;
    for_linked (decl->type.info,comp) {
        fprintf(result, "            { .name= \"%.*s\"\n", bufmt(curr->decl->name));
        fprintf(result, "            , .type= &"); emit_adpt_type(&curr->decl->type); fprintf(result, ",\n");
        fprintf(result, "            , .offset= offsetof("); emit_type(&decl->type); fprintf(result, ", %.*s) },\n", bufmt(curr->decl->name));
        count++;
    }
    fprintf(result, "        },\n");
    fprintf(result, "        .count= %zu,\n", count);
    fprintf(result, "    },\n");

    fprintf(result, "};\n");
    if (decl->type.name.len && SPEC_TYPEDEF == decl->spec) {
        fprintf(result, "static struct adpt_type const %.*s = struct_%.*s_adapt_type;\n", bufmt(decl->name), bufmt(decl->type.name));
        // TODO: add to correct namespace
    }

    // handle nested struct/union/enum definitions
    for_linked (decl->type.info,comp) switch (curr->decl->type.kind) {
    case KIND_STRUCT:
    case KIND_UNION:
    case KIND_ENUM:
        errdie("NIY (non top-level struct/union/enum)");
        break;

    default:;
    }
}

void _emit_fun(declaration cref decl) {
    search_namespace (decl->name, seen.fun) return;
    struct seen_fun* p = dyarr_push(&seen.fun);
    if (!p) exitf("OOM");
    p->name = decl->name;

    if (-1ul == decl->type.info.fun.count)
        errdie("not supported, use (void) or specify the actual arguments");

    emit_type(&decl->type.info.fun.ret->type);
    fprintf(result, "%.*s(", bufmt(decl->name));
    if (!decl->type.info.fun.count) fprintf(result, "void");
    for_linked (decl->type.info,fun) {
        emit_type(&curr->decl->type);
        fprintf(result, "%.*s", bufmt(curr->decl->name));
        if (curr->next) fprintf(result, ", ");
    }
    fprintf(result, ");\n");

    fprintf(result, "void %.*s_adapt_call(char* ret, char** args) {\n", bufmt(decl->name));
    if (!decl->type.info.fun.count)
        fprintf(result, "    (void)args;\n");
    if (bufis(decl->type.info.fun.ret->type.name, "void"))
        fprintf(result, "    (void)ret;\n    ");
    else {
        fprintf(result, "    *(");
        emit_type(&decl->type.info.fun.ret->type);
        fprintf(result, "*)ret = ");
    }
    fprintf(result, "%.*s(", bufmt(decl->name));
    size_t k = 0;
    for_linked (decl->type.info,fun) {
        fprintf(result, "*(");
        emit_type(&curr->decl->type);
        fprintf(result, "*)args[%zu]", k++);
        if (curr->next) fprintf(result, ", ");
    }
    fprintf(result, ");\n");
    fprintf(result, "}\n");

    fprintf(result, "static struct adpt_type const %.*s_adapt_type = {\n", bufmt(decl->name));
    fprintf(result, "    .size= sizeof(void(*)()), .align= sizeof(void(*)()),\n");
    fprintf(result, "    .tyty= TYPE_FUN,\n");
    fprintf(result, "    .info.fun= {\n");
    fprintf(result, "        .ret= &"); emit_adpt_type(&decl->type.info.fun.ret->type); fprintf(result, ",\n");
    fprintf(result, "        .params= (struct adpt_fun_param[]){\n");
    size_t count = 0;
    for_linked (decl->type.info,fun) {
        fprintf(result, "            { .name= \"%.*s\"\n", bufmt(curr->decl->name));
        fprintf(result, "            , .type= &"); emit_adpt_type(&curr->decl->type); fprintf(result, " },\n");
        count++;
    }
    fprintf(result, "        },\n");
    fprintf(result, "        .count= %zu,\n", count);
    fprintf(result, "    },\n");
    fprintf(result, "};\n");

    fprintf(result, "\n");
}
// }}}

// emit generics {{{
void emit_type(struct decl_type cref type) {
    switch (type->kind) {
    case KIND_NOTAG: fprintf(result, "%.*s ", bufmt(type->name)); break;

    case KIND_STRUCT: fprintf(result, "struct "); if (0)
    case KIND_UNION:  fprintf(result, "union ");
        if (type->name.len) fprintf(result, "%.*s ", bufmt(type->name));
        else {
            errdie("NIY (annon struct/union type, will make a internal name like `struct@1` or something..)");
        }

        //if (-1 == type->info.obj.count) break;
        //fprintf(result, "{");
        //for (struct decl_type_field* curr = type->info.obj.first; curr; curr = curr->next) {
        //    emit_decl(curr->decl, depth+1);
        //    printf("\n");
        //}
        //fprintf(result, "}");
        break;

    case KIND_ENUM:   fprintf(result, "enum ");   break;
    case KIND_PTR:
        emit_type(&type->info.ptr->type);
        fprintf(result, "*");
        break;
    case KIND_FUN: break;
    case KIND_ARR: break;
    }

    for (unsigned k = 0; type->quals[k]; k++) switch (type->quals[k]) {
    case QUAL_END: break;
    case QUAL_CONST:     fprintf(result, "const ");     break;
    case QUAL_RESTRICT:  fprintf(result, "restrict ");  break;
    case QUAL_VOLATILE:  fprintf(result, "volatile ");  break;
    case QUAL_INLINE:    fprintf(result, "inline ");    break;
    case QUAL_SIGNED:    fprintf(result, "signed ");    break;
    case QUAL_UNSIGNED:  fprintf(result, "unsigned ");  break;
    case QUAL_SHORT:     fprintf(result, "short ");     break;
    case QUAL_LONG:      fprintf(result, "long ");      break;
    case QUAL_COMPLEX:   fprintf(result, "complex ");   break;
    case QUAL_IMAGINARY: fprintf(result, "imaginary "); break;
    }
}

void emit_adpt_type(struct decl_type cref type) {
    switch (type->kind) {
    case KIND_NOTAG:
        if (bufis(type->name, "size_t")) {
            fprintf(result, "adptb_ulong_type");
            break;
        }
        fprintf(result, "adptb_%.*s_type", bufmt(type->name));
        break;

    case KIND_STRUCT:
    case KIND_UNION:
    case KIND_ENUM:
        break;

    case KIND_PTR:
        fprintf(result, "(struct adpt_type){"
                            ".size= sizeof(void*), "
                            ".align= sizeof(void*), "
                            ".tyty= TYPE_PTR, "
                            ".info.ptr= &"); emit_adpt_type(&type->info.ptr->type);
        fprintf(result, "}");
        break;

    case KIND_FUN:
    case KIND_ARR:
        break;
    }
}

void emit_decl(declaration cref decl) {
    switch (decl->spec) {
    case SPEC_NONE: break;
    case SPEC_TYPEDEF:  fprintf(result, "typedef ");  break;
    case SPEC_EXTERN:   fprintf(result, "extern ");   break;
    case SPEC_STATIC:   fprintf(result, "static ");   break;
    case SPEC_AUTO:     fprintf(result, "auto ");     break;
    case SPEC_REGISTER: fprintf(result, "register "); break;
    }

    switch (decl->type.kind) {
    case KIND_NOTAG:
        errdie("NIY (static variable declaration)");
        break;

    case KIND_STRUCT:
    case KIND_UNION:
        _emit_comp(decl);
        break;

    case KIND_ENUM:
        errdie("NIY (enum declaration)");
        break;

    case KIND_PTR:
        errdie("NIY (static pointer variable declaration)");
        break;

    case KIND_FUN:
        _emit_fun(decl);
        break;

    case KIND_ARR:
        errdie("NIY (array declaration)");
        break;
    }
}
// }}}

void emit_top(void* _, declaration cref decl, bufsl ref tok) {
    (void)_;
    emit_decl(decl);
}

// random things {{{
void cleanup(void) {
    ldel(&ls);
    if (result && stdout != result) fclose(result); // yyy
}

bufsl name_space(char ref name) {
    bufsl itns = {.ptr= name, .len= strlen(name)};
    char* basename = strrchr(itns.ptr, '/');
    if (basename) itns.ptr = basename+1;
    char const* fileext = strchr(itns.ptr, '.');
    if (fileext) itns.len = fileext - itns.ptr;
    for (size_t k = 0; k < itns.len; k++)
        if ((itns.ptr[k]|32) < 'a' || 'z' < (itns.ptr[k]|32))
            ((char*)itns.ptr)[k] = '_';
    return itns;
}
// }}}

// do-s {{{
int do_merge(int argc, char** argv) {
    char** const first = (--argc, ++argv);
    char** last = first;
    while (argc) {
        char* arg = (argc--, *argv++);
        char* val;
        if (!memcmp("-o", arg, 2)) {
            last = argv;
            val = arg[2] ? arg+2 : (argc--, *argv++);
            if ('-' != val[0]) result = fopen(val, "wb");
            break;
        }
    }

    for (char** it = first; it < last-1; it++)
        fprintf(result, "#include \"%s\"\n", *it);
    fprintf(result, "\n");

    fprintf(result, "static struct adpt_namespace const namespaces[] = {\n");
    for (char** it = first; it < last-1; it++) {
        bufsl itns = name_space(*it);
        if ('a' == itns.ptr[0] && '-' == itns.ptr[1]) {
            itns.ptr+= 2;
            itns.len-= 2;
        }
        fprintf(result, "    { .name= \"%.*s\"\n", bufmt(itns));
        fprintf(result, "    , .count= countof(adptns_%.*s)\n", bufmt(itns));
        fprintf(result, "    , .items= adptns_%.*s },\n", bufmt(itns));
    }
    fprintf(result, "};\n");
    fprintf(result, "\n");

    fprintf(result, "#define CINTRE_NAMESPACES_DEFINED\n");
    fprintf(result, "#include \"cintre.c\"\n");

    return EXIT_SUCCESS;
}

int do_prepare(int argc, char** argv) {
    char* file = (argc--, *argv++);
    result = stdout; // yyy

    atexit(cleanup);

    while (argc) {
        char* arg = (argc--, *argv++);
        char* val;

        if ('-' == *arg) switch (arg[1]) {
        case 'D':
            val = strchr(arg, '=');
            if (val) *(val++) = '\0';
            else val = "1";
            ldef(&ls, arg+2, val);
            break;

        case 'I':
            linc(&ls, arg+2);
            break;

        case 'o':
            val = arg[2] ? arg+2 : (argc--, *argv++);
            if ('-' != val[0]) result = fopen(val, "wb");
            break;
        }
    }

    if (!result) exitf("Missing result operand or file not writable");
    fprintf(result, "#include \"adapter.h\"\n\n");

    lini(&ls, file);

    bufsl tok = lext(&ls);
    parse_decl_state ps = {.ls= &ls, .on= emit_top};
    while (tok.len) if ((tok = parse_declaration(&ps, tok)).len) switch (*tok.ptr) {
    case '{':
        for (unsigned depth = 0; (tok = lext(&ls)).len; ) {
            bool c = '}' == *tok.ptr;
            if (!tok.len || (!depth && c)) break;
            depth+= ('{' == *tok.ptr)-c;
        }
        tok = lext(&ls);
        ps.base = (declaration){0}; // reset
        continue;

    case '=':
        tok = parse_expression(&(parse_expr_state){.ls= &ls}, lext(&ls));
        if (tok.len && ';' == *tok.ptr)
    case ';':
            ps.base = (declaration){0}; // reset
        // fall through
    case ',':
        tok = lext(&ls);
        continue;
    }

    bufsl thisns = name_space(file);
    fprintf(result, "static struct adpt_item const adptns_%.*s[] = {\n", bufmt(thisns));
    for (size_t k = 0; k < seen.fun.len; k++) {
        fprintf(result, "    { .name= \"%.*s\"\n", bufmt(seen.fun.ptr[k].name));
        fprintf(result, "    , .type= &%.*s_adapt_type\n", bufmt(seen.fun.ptr[k].name));
        fprintf(result, "    , .as.function= %.*s_adapt_call\n", bufmt(seen.fun.ptr[k].name));
        fprintf(result, "    , .kind= ITEM_VALUE },\n");
    }
    if (seen.var.len) errdie("NIY (other kinds of static variable declaration)");
    fprintf(result, "};\n");

    return EXIT_SUCCESS;
}
// }}}

int main(int argc, char** argv) {
    char* prog = (argc--, *argv++);
    if (!argc || !strcmp("-h", *argv) || !strcmp("--help", *argv)) exitf(
            "Usage: %s <entry> [-D...,-I...] -o <a-file.h>\n"
            "   or: %s -m <a-files...> -o <c-main.c>\n"
            , prog, prog);

    return !strcmp("-m", *argv) ? do_merge(argc, argv) : do_prepare(argc, argv);
}

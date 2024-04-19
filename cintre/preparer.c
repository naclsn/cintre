// big ol' TODO: all of it

#include "common.h"
#include "parser.h"
#include "prints.h"

lex_state ls = {0};
FILE* result = NULL;
dyarr(struct seen {
    bufsl name;
}) seen = {0};

void cleanup(void) {
    ldel(&ls);
    if (result && stdout != result) fclose(result); // yyy
}

// emit {{{
void emit_type(struct decl_type cref type);
void emit_adpt_type(struct decl_type cref type);
void emit_decl(declaration cref decl);

void emit_type(struct decl_type cref type) {
    switch (type->kind) {
    case KIND_NOTAG: fprintf(result, "%.*s ", bufmt(type->name)); break;

    case KIND_STRUCT: fprintf(result, "struct "); if (0)
    case KIND_UNION:  fprintf(result, "union ");
        //if (type->name.len) {
            notif("NIY: emit struct/union extra info");
        //}
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
    case KIND_NOTAG: break;
    case KIND_STRUCT: fprintf(result, "struct "); break;
    case KIND_UNION:  fprintf(result, "union ");  break;
    case KIND_ENUM:   fprintf(result, "enum ");   break;
    case KIND_PTR: break;

    case KIND_FUN:
        if ((size_t)-1 == decl->type.info.fun.count)
            exitf("%s:%zu: not supported, use (void) or specify the actual arguments", ls.file, ls.line);

        emit_type(&decl->type.info.fun.ret->type);
        fprintf(result, "%.*s(", bufmt(decl->name));
        if (!decl->type.info.fun.count) printf("void");
        for (struct decl_type_param* curr = decl->type.info.fun.first; curr; curr = curr->next) {
            emit_type(&curr->decl->type);
            fprintf(result, "%.*s", bufmt(curr->decl->name));
            if (curr->next) fprintf(result, ", ");
        }
        fprintf(result, ");\n");

        fprintf(result, "void %.*s_adapt_call(char* ret, char** args) {\n", bufmt(decl->name));
        if (bufis(decl->type.info.fun.ret->name, "void"))
            fprintf(result, "    (void)ret;\n    ");
        else {
            fprintf(result, "    *(");
            emit_type(&decl->type.info.fun.ret->type);
            fprintf(result, "*)ret = ");
        }
        fprintf(result, "%.*s(", bufmt(decl->name));
        if (!decl->type.info.fun.count) printf("void");
        size_t k = 0;
        for (struct decl_type_param* curr = decl->type.info.fun.first; curr; curr = curr->next) {
            fprintf(result, "*(");
            emit_type(&curr->decl->type);
            fprintf(result, "*)args[%zu]", k++);
            if (curr->next) fprintf(result, ", ");
        }
        fprintf(result, ");\n");
        fprintf(result, "}\n");

        fprintf(result, "static struct adpt_type const %.*s_adapt_type = {\n", bufmt(decl->name));
        fprintf(result, "    .size= %zu, .align= %zu,\n", sizeof&fprintf, sizeof&fprintf);
        fprintf(result, "    .tyty= TYPE_FUN,\n");
        fprintf(result, "    .info.fun= {\n");
        fprintf(result, "        .ret= &"); emit_adpt_type(&decl->type.info.fun.ret->type); fprintf(result, ",\n");
        fprintf(result, "        .params= (struct adpt_fun_param[]){\n");
        size_t count = 0;
        for (struct decl_type_param* curr = decl->type.info.fun.first; curr; curr = curr->next) {
            fprintf(result, "            { .name= \"%.*s\"\n", bufmt(curr->decl->name));
            fprintf(result, "            , .type= &"); emit_adpt_type(&curr->decl->type); fprintf(result, ",\n");
            fprintf(result, "            },\n");
            count++;
        }
        fprintf(result, "        },\n");
        fprintf(result, "        .count= %zu,\n", count);
        fprintf(result, "    },\n");
        fprintf(result, "};\n");

        fprintf(result, "\n");
        break;

    case KIND_ARR:
        break;
    }
}

void emit(void* _, declaration cref decl, bufsl ref tok) {
    (void)_;
    if (KIND_FUN != decl->type.kind) return; // yyy

    search_namespace(decl->name, seen) return;
    struct seen* p = dyarr_push(&seen);
    if (!p) exitf("OOM");
    p->name = decl->name;

    emit_decl(decl);
}
// }}}

int main(int argc, char** argv) {
    char* prog = (argc--, *argv++);
    if (!argc || !strcmp("-h", *argv) || !strcmp("--help", *argv)) exitf(
            "Usage: %s <entry> [-D...,-I...] -o <a-file.h>\n"
            "   or: %s -m <a-files...> -o <c-main.c>\n"
            , prog, prog);
    char* file = (argc--, *argv++);
    result = stdout; // yyy

    if (!strcmp("-m", file)) {
        char** first = argv;
        char** last = argv;
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
            bufsl itns = {.ptr= *it, .len= strlen(*it)};
            {
                char const* basename = strrchr(itns.ptr, '/');
                if (basename) itns.ptr = basename+1;
                char const* fileext = strchr(itns.ptr, '.');
                if (fileext) itns.len = fileext - itns.ptr;
                if ('a' == itns.ptr[0] && '-' == itns.ptr[1]) {
                    itns.ptr+= 2;
                    itns.len-= 2;
                }
            }
            fprintf(result, "    {.name= \"%.*s\", .count= countof(adptns_%.*s), .items= adptns_%.*s},\n", bufmt(itns), bufmt(itns), bufmt(itns));
        }
        fprintf(result, "};\n");
        fprintf(result, "\n");

        fprintf(result, "#define CINTRE_NAMESPACES_DEFINED\n");
        fprintf(result, "#include \"cintre.c\"\n");

        return EXIT_SUCCESS;
    }

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
    parse_decl_state ps = {.ls= &ls, .on= emit};
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

    bufsl thisns = {.ptr= file, .len= strlen(file)};
    {
        char const* basename = strrchr(thisns.ptr, '/');
        if (basename) thisns.ptr = basename+1;
        char const* fileext = strchr(thisns.ptr, '.');
        if (fileext) thisns.len = fileext - thisns.ptr;
    }
    fprintf(result, "static struct adpt_item const adptns_%.*s[] = {\n", bufmt(thisns));
    for (size_t k = 0; k < seen.len; k++) {
        fprintf(result, "    { .name= \"%.*s\"\n", bufmt(seen.ptr[k].name));
        fprintf(result, "    , .type= &%.*s_adapt_type\n", bufmt(seen.ptr[k].name));
        fprintf(result, "    , .as.function= %.*s_adapt_call\n", bufmt(seen.ptr[k].name));
        fprintf(result, "    , .kind= ITEM_VALUE },\n");
    }
    fprintf(result, "};\n");

    return EXIT_SUCCESS;
} // main

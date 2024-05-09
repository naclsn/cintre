/// program to generate the <a-file.h> and <c-main.h>
/// see `$ preparer -h` (or the main function at the end of the file)
///
/// function declarations and other static objects make a `name_adapt_type`
/// describing their type, and an entry in the namespace with the normal name
///
/// tag type declarations (eg. `struct name {}`) make an associated
/// `name_adapt_tag_type` for the type itself, and an entry in the namespace
/// with a name like `@name`
///
/// typedefs translate to preprocessor macros to the underlying type; when
/// needed (eg. `typedef struct {} name`) it makes a `name_adapt_tdf_type`

// TODO: more of it
// - typedef
// - struct/enum/union
// - static vars
// - (also coverage and cleanup as it's already quite messy)

#include "common.h"
#include "parser.h"

lex_state ls = {0};
FILE* result = NULL;
int indent = 0;

/// last `emit` of an indented should not be an `emitln` (so a plain `emit`)
#define indented(...)  for (bool _once = (++indent, emitln(__VA_ARGS__), true); _once; _once = (--indent, emitln(" "), false))
#if 0//def LOC_NOTIF
# define emit(...)  fprintf(result, " /*" HERE "*/ " __VA_ARGS__)
#else
# define emit(...)  fprintf(result, __VA_ARGS__)
#endif
#define emitln(...)  (emit(__VA_ARGS__), fprintf(result, "\n%*s", indent*4, ""))

struct {
    dyarr(struct seen_fun { bufsl name; }) funs;
    dyarr(struct seen_tag { bufsl name; }) tags;
    dyarr(struct seen_tdf { bufsl name; }) tdfs;
    dyarr(struct seen_var { bufsl name; }) objs;
} seen = {0};

#define errdie(...) (report_lex_locate(&ls, "preparer: " __VA_ARGS__), exit(EXIT_FAILURE))

#define _linked_it_type_comp struct decl_type_field
#define _linked_it_type_enu struct decl_type_enumer
#define _linked_it_type_fun struct decl_type_param
#define for_linked(__info, __ty) for (_linked_it_type_##__ty* curr = (__info).__ty.first; curr; curr = curr->next)

void emit_adpt_type_val(struct decl_type cref ty, bool const in_decl);
void emit_decl(declaration cref decl);

/*/ emit specifics {{{
void _emit_comp(declaration cref decl)
{
    if (KIND_STRUCT != decl->type.kind)
        errdie("NIY (union declaration)");

    // eg. `struct bidoof a;` or `typedef struct bidoof a` (so `decl->type.name.len` implied)
    if (decl->name.len && -1ul == decl->type.info.comp.count) {
        if (SPEC_TYPEDEF == decl->spec) {
            fprintf(result, "struct %.*s %.*s;\n", bufmt(decl->type.name), bufmt(decl->name));
            // for tentative definitions (ie. `struct bidoof {};` is expected later on)
            fprintf(result, "static struct adpt_type const %.*s_adapt_tag_type;\n", bufmt(decl->type.name));
            fprintf(result, "#define %.*s_adapt_type %.*s_adapt_tag_type\n", bufmt(decl->name), bufmt(decl->type.name));
            bool found = false;
            search_namespace (decl->name, seen.tdfs) { found = true; break; }
            if (!found) {
                struct seen_tdf* p = dyarr_push(&seen.tdfs);
                if (!p) exitf("OOM");
                p->name = decl->name;
            }
            fprintf(result, "\n");
        } else errdie("NIY (static variable def of already declared tag type)");
        return;
    }

    fprintf(result, "struct ");
    if (decl->type.name.len) {
        fprintf(result, "%.*s ", bufmt(decl->type.name));
        // eg. `struct bidoof;`
        if (-1ul == decl->type.info.comp.count) {
            fprintf(result, ";\n\n");
            return;
        }
    }
    // if reaching here, we should really have a full declaration body

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
        fprintf(result, "static struct adpt_type const %.*s_adapt_tag_type = {\n", bufmt(decl->type.name));
        bool found = false;
        search_namespace (decl->type.name, seen.tags) { found = true; break; }
        if (!found) {
            struct seen_tag* p = dyarr_push(&seen.tags);
            if (!p) exitf("OOM");
            p->name = decl->type.name;
        }
    } else if (SPEC_TYPEDEF == decl->spec) {
        // XXX: xxx, but I don't remember why
        fprintf(result, "#define %.*s_adapt_type %.*s_adapt_tdf_type\n", bufmt(decl->name), bufmt(decl->name));
        fprintf(result, "static struct adpt_type const %.*s_adapt_tdf_type = {\n", bufmt(decl->name));
        bool found = false;
        search_namespace (decl->name, seen.tdfs) { found = true; break; }
        if (!found) {
            struct seen_tdf* p = dyarr_push(&seen.tdfs);
            if (!p) exitf("OOM");
            p->name = decl->name;
        }
    } else errdie("NIY (static annon struct variable declaration)");

    fprintf(result, "    .size= sizeof("); emit_type(&decl->type); fprintf(result, "), .align= alignof("); emit_type(&decl->type); fprintf(result, "),\n");
    fprintf(result, "    .tyty= TYPE_STRUCT,\n");
    fprintf(result, "    .info.comp= {\n");
    fprintf(result, "        .fields= (struct adpt_comp_field[]){\n");
    size_t count = 0;
    for_linked (decl->type.info,comp) {
        fprintf(result, "            { .name= \"%.*s\"\n", bufmt(curr->decl->name));
        fprintf(result, "            , .type= &"); emit_adpt_type_val(&curr->decl->type); fprintf(result, "\n");
        fprintf(result, "            , .offset= offsetof("); emit_type(&decl->type); fprintf(result, ", %.*s) },\n", bufmt(curr->decl->name));
        count++;
    }
    fprintf(result, "        },\n");
    fprintf(result, "        .count= %zu,\n", count);
    fprintf(result, "    },\n");

    fprintf(result, "};\n");
    if (decl->type.name.len && SPEC_TYPEDEF == decl->spec) {
        fprintf(result, "#define %.*s_adapt_type %.*s_adapt_tag_type\n", bufmt(decl->name), bufmt(decl->type.name));
        bool found = false;
        search_namespace (decl->name, seen.tdfs) { found = true; break; }
        if (!found) {
            struct seen_tdf* p = dyarr_push(&seen.tdfs);
            if (!p) exitf("OOM");
            p->name = decl->name;
        }
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

    fprintf(result, "\n");
}

void _emit_fun(declaration cref decl)
{
    search_namespace (decl->name, seen.funs) return;
    struct seen_fun* p = dyarr_push(&seen.funs);
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
    fprintf(result, "        .ret= &"); emit_adpt_type_val(&decl->type.info.fun.ret->type); fprintf(result, ",\n");
    fprintf(result, "        .params= (struct adpt_fun_param[]){\n");
    size_t count = 0;
    for_linked (decl->type.info,fun) {
        fprintf(result, "            { .name= \"%.*s\"\n", bufmt(curr->decl->name));
        fprintf(result, "            , .type= &"); emit_adpt_type_val(&curr->decl->type); fprintf(result, " },\n");
        count++;
    }
    fprintf(result, "        },\n");
    fprintf(result, "        .count= %zu,\n", count);
    fprintf(result, "    },\n");
    fprintf(result, "};\n");

    fprintf(result, "\n");
}
// }}} */

/*/ emit generics {{{
void emit_type(struct decl_type cref type)
{
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
    case QUAL_SIGNED:    fprintf(result, "signed ");    break;
    case QUAL_UNSIGNED:  fprintf(result, "unsigned ");  break;
    case QUAL_SHORT:     fprintf(result, "short ");     break;
    case QUAL_LONG:      fprintf(result, "long ");      break;
    case QUAL_COMPLEX:   fprintf(result, "complex ");   break;
    case QUAL_IMAGINARY: fprintf(result, "imaginary "); break;
    }
}

void emit_decl(declaration cref decl)
{
    switch (decl->spec) {
    case SPEC_TYPEDEF:  fprintf(result, "typedef ");  break;
    case SPEC_EXTERN:   fprintf(result, "extern ");   break;
    case SPEC_STATIC:   return; //fprintf(result, "static ");   break;
    //case SPEC_AUTO:     fprintf(result, "auto ");     break;
    //case SPEC_REGISTER: fprintf(result, "register "); break;
    default:;
    }
    if (decl->is_inline) return;

    switch (decl->type.kind) {
    case KIND_NOTAG:
        if (SPEC_TYPEDEF == decl->spec) {
            emit_type(&decl->type); fprintf(result, "%.*s;\n", bufmt(decl->name));
            fprintf(result, "#define %.*s_adapt_type ", bufmt(decl->name)); emit_adpt_type_val(&decl->type); fprintf(result, "\n");
            break;
        }
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
        fprintf(result, "static struct adpt_type const %.*s_adapt_tdf_type = {\n", bufmt(decl->name));
        fprintf(result, "    .size= sizeof(void*), .align= sizeof(void*),\n");
        fprintf(result, "    .tyty= TYPE_PTR,\n");
        fprintf(result, "    .info.ptr= &"); emit_adpt_type_val(&decl->type.info.ptr->type); fprintf(result, "\n");
        fprintf(result, "}\n");
        fprintf(result, "#define %.*s_adapt_type %.*s_adapt_tdf_type\n", bufmt(decl->name), bufmt(decl->name));
        break;

    case KIND_FUN:
        _emit_fun(decl);
        break;

    case KIND_ARR:
        errdie("NIY (array declaration)");
        break;
    }
}
// }}} */

// same (rewrite) {{{
/// forwards as-is
void emit_forward(struct decl_type cref ty, bufsl cref name)
{
    for (unsigned k = 0; ty->quals[k]; k++) switch (ty->quals[k]) {
    case QUAL_END: break;
    case QUAL_CONST:     emit("const ");     break;
    case QUAL_RESTRICT:  emit("restrict ");  break;
    case QUAL_VOLATILE:  emit("volatile ");  break;
    case QUAL_SIGNED:    emit("signed ");    break;
    case QUAL_UNSIGNED:  emit("unsigned ");  break;
    case QUAL_SHORT:     emit("short ");     break;
    case QUAL_LONG:      emit("long ");      break;
    case QUAL_COMPLEX:   emit("complex ");   break;
    case QUAL_IMAGINARY: emit("imaginary "); break;
    }

    switch (ty->kind) {
    case KIND_NOTAG: emit("%.*s", bufmt(ty->name)); break;

    case KIND_STRUCT: emit("struct "); if (0)
    case KIND_UNION:  emit("union ");
        if (ty->name.len) emit("%.*s", bufmt(ty->name));
        else {
            errdie("NIY (annon struct/union ty, will make a internal name like `struct@1` or something..)");
        }

        //if (-1 == ty->info.obj.count) break;
        //emit("{");
        //for (struct decl_type_field* curr = ty->info.obj.first; curr; curr = curr->next) {
        //    emit_decl(curr->decl, depth+1);
        //    printf("\n");
        //}
        //emit("}");
        break;

    case KIND_ENUM:
        emit("enum ");
        if (ty->name.len) emit("%.*s", bufmt(ty->name));
        errdie("NIY");
        break;

    case KIND_PTR:
        emit_forward(&ty->info.ptr->type, NULL);
        if (name) emit("* %.*s", bufmt(*name));
        else emit("*");
        break;

    case KIND_FUN:
        emit_forward(&ty->info.fun.ret->type, NULL);
        if (name) emit(" %.*s(", bufmt(*name));
        else emit(" (*)");
        if (-1ul != ty->info.fun.count) {
            if (0 == ty->info.fun.count) emit("void");
            else for_linked (ty->info,fun) {
                emit_forward(&curr->decl->type, &curr->decl->name);
                if (curr->next) emit(", ");
            }
        }
        emit(")");
        break;

    case KIND_ARR:
        emit_forward(&ty->info.ptr->type, NULL);
        if (name) emit("%.*s", bufmt(*name));
        emit("[?]");
        errdie("NIY");
        break;
    }

}

/// if the type is a named struct/union, it defines its decl_type (name_adapt_tag_type)
void emit_named_comp_adpt_type_def(struct decl_type cref ty)
{
    switch (ty->kind) {
    case KIND_STRUCT:
    case KIND_UNION:
        if (!ty->name.len)
    default:
            return;
    }

    emit("static struct adpt_type const %.*s_adapt_tag_type = ", bufmt(ty->name));
    emit_adpt_type_val(ty, true);
    emitln(";");
}

/// emit a compile-time &-able value that is the decl_type for this type
/// if `in_decl` is true, will not do `(struct adpt_type){..}` but just `{..}`
void emit_adpt_type_val(struct decl_type cref ty, bool const in_decl)
{
    switch (ty->kind) {
    case KIND_NOTAG:;
        bool _signed = false, _unsigned = false, _short = false, _long = false;
        for (size_t k = 0; QUAL_END != ty->quals[k]; k++) switch (ty->quals[k]) {
            case QUAL_SIGNED:    _signed = true;          break;
            case QUAL_UNSIGNED:  _unsigned = true;        break;
            case QUAL_SHORT:     _short = true;           break;
            case QUAL_LONG:      _long = true;            break;
            case QUAL_COMPLEX:   exitf("NIY: complex");   break;
            case QUAL_IMAGINARY: exitf("NIY: imaginary"); break;
            default:;
        }

        char const* adptb = NULL;
#       define nameis(s)  (strlen(s) == ty->name.len && !memcmp(s, ty->name.ptr, strlen(s)))
        if (nameis("char"))
            adptb = _signed ? "adptb_schar_type"
                  : _unsigned ? "adptb_uchar_type"
                  : "adptb_char_type";
        if (nameis("int") || !ty->name.len)
            adptb = _short ? (_unsigned ? "adptb_ushort_type" : "adptb_short_type")
                  : _long ? (_unsigned ? "adptb_ulong_type" : "adptb_long_type")
                  : _unsigned ? "adptb_uint_type" : "adptb_int_type";
        if (nameis("float"))  adptb = "adptb_float_type";
        if (nameis("double")) adptb = "adptb_double_type";
        if (nameis("void"))   adptb = "adptb_void_type";
#       undef nameis

        if (adptb) emit("%s", adptb);
        else emit("%.*s_adapt_type", bufmt(ty->name));
        break;

    case KIND_STRUCT:
    case KIND_UNION:
    case KIND_ENUM:
        if (ty->name.len) {
            emit("%.*s_adapt_tag_type", bufmt(ty->name));
            break;
        }
        exitf("NIY: KIND_STRUCT/UNION/ENUM");
        break;

    case KIND_PTR:
        indented ("%s{", in_decl ? "" : "(struct adpt_type)") {
            emitln(".size= sizeof(void*),");
            emitln(".align= sizeof(void*),");
            emitln(".tyty= TYPE_PTR,");
            emit(".info.ptr= &");
            emit_adpt_type_val(&ty->info.ptr->type, false);
            emit(",");
        }
        emit("}");
        break;

    case KIND_FUN:
        indented ("%s{", in_decl ? "" : "(struct adpt_type)") {
            emitln(".size= sizeof(void(*)()),");
            emitln(".align= sizeof(void(*)()),");
            emitln(".tyty= TYPE_FUN,");
            indented (".info.fun= {") {
                emit(".ret= &");
                emit_adpt_type_val(&ty->info.fun.ret->type, false);
                emitln(",");
                emit(".params= ");
                size_t count = 0;
                if (-1ul == ty->info.fun.count || 0 == ty->info.fun.count) emitln("NULL,");
                else {
                    indented ("(struct adpt_fun_param[]){") for_linked (ty->info,fun) {
                        emitln("{ .name= \"%.*s\"", bufmt(curr->decl->name));
                        emit(", .type= &");
                        emit_adpt_type_val(&curr->decl->type, false);
                        emitln(" },");
                        count++;
                    }
                    emitln("},");
                }
                emit(".count= %zu,", count);
            }
            emit("},");
        }
        emit("}");
        break;

    case KIND_ARR:
        exitf("NIY: KIND_ARR");
        break;
    }
}

void emit_extern(declaration cref decl)
{
    emit("extern ");
    emit_forward(&decl->type, &decl->name);
    emitln(";");

    if (KIND_FUN == decl->type.kind) {
        emit("void %.*s_adapt_call(char* ret, char** args)", bufmt(decl->name));
        indented ("{") {
            if (0 == decl->type.info.fun.count) emitln("(void)args;");
            if (bufis(decl->type.info.fun.ret->type.name, "void")) emitln("(void)ret;");
            else {
                emit("*(");
                emit_forward(&decl->type.info.fun.ret->type, NULL);
                emit("*)ret = ");
            }
            emit("%.*s(", bufmt(decl->name));
            size_t k = 0;
            for_linked (decl->type.info,fun) {
                emit("*(");
                emit_forward(&curr->decl->type, NULL);
                emit("*)args[%zu]", k++);
                if (curr->next) emit(", ");
            }
            emit(");");
        }
        emitln("}");
    }

    emit("static struct adpt_type const %.*s_adapt_type = ", bufmt(decl->name));
    emit_adpt_type_val(&decl->type, true);
    emitln(";");
}

void emit_typedef(declaration cref decl)
{
    emit("typedef ");
    emit_forward(&decl->type, &decl->name);
    emitln(";");

    emit_named_comp_adpt_type_def(&decl->type);
    emit("static struct adpt_type const %.*s_adapt_tdf_type = ", bufmt(decl->name));
    emit_adpt_type_val(&decl->type, true);
    emitln(";");

    emitln("#define %.*s_adapt_type %.*s_adapt_tdf_type", bufmt(decl->name), bufmt(decl->name));
}
// }}}

void emit_top(void* _, declaration cref decl, bufsl ref tok)
{   (void)_;
    (void)tok;

    if (decl->is_inline) return;

    switch (decl->spec) {
    case SPEC_STATIC: // internal linkage (not visible)
    default: // others are unreachable
        return;

    case SPEC_EXTERN: // external linkage
    case SPEC_NONE: // default at file scope is external linkage
        emit_extern(decl);
        fputc('\n', result);
        return;

    case SPEC_TYPEDEF:
        emit_typedef(decl);
        fputc('\n', result);
        return;
    }
}

// random things {{{
void cleanup(void)
{
    ldel(&ls);
    if (result && stdout != result) fclose(result);

    free(seen.funs.ptr);
    free(seen.tags.ptr);
    free(seen.tdfs.ptr);
    free(seen.objs.ptr);
}

bufsl name_space(char ref name)
{
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
int do_merge(int argc, char** argv)
{
    char** const first = (--argc, ++argv);
    result = stdout;
    atexit(cleanup);

    char** last = NULL;
    while (0 < argc) {
        char* arg = (argc--, *argv++);
        char* val;
        if (!memcmp("-o", arg, 2)) {
            last = argv-1;
            val = arg[2] ? arg+2 : (argc--, *argv++);
            if (!val) result = NULL;
            else if ('-' != val[0]) result = fopen(val, "wb");
            break;
        }
    }
    if (!last) last = argv;
    if (!result) exitf("Missing result operand or file not writable");

    for (char** it = first; it < last; it++)
        fprintf(result, "#include \"%s\"\n", *it);
    fprintf(result, "\n");

    fprintf(result, "static struct adpt_namespace const namespaces[] = {\n");
    for (char** it = first; it < last; it++) {
        bufsl itns = name_space(*it);
        // adapter files are expected to start with "a-"
        // tho this is not strictly mandatory I suppose...
        if ('a' == itns.ptr[0] && '_' == itns.ptr[1]) {
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

int do_prepare(int argc, char** argv)
{
    char* file = (argc--, *argv++);
    result = stdout;
    atexit(cleanup);

    while (0 < argc) {
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
            if (!val) result = NULL;
            else if ('-' != val[0]) result = fopen(val, "wb");
            break;
        }
    }

    if (!result) exitf("Missing result operand or file not writable");
    fprintf(result, "#include \"adapter.h\"\n\n");

    lini(&ls, file);

    bufsl tok = lext(&ls);
    parse_decl_state ps = {.ls= &ls, .on= emit_top};
    while (tok.len) if ((tok = parse_declaration(&ps, tok)).len) switch (*tok.ptr) {
    case '=':
        tok = lext(&ls);
        if (tok.len && '{' == *tok.ptr) {

    case '{':
            for (unsigned depth = 0; (tok = lext(&ls)).len; ) {
                bool c = '}' == *tok.ptr;
                if (!tok.len || (!depth && c)) break;
                depth+= ('{' == *tok.ptr)-c;
            }
            tok = lext(&ls);

            if (!tok.len || !strchr(",;", *tok.ptr)) {
                ps.base = (declaration){0}; // reset
                continue;
            }

        } else tok = parse_expression(&(parse_expr_state){.ls= &ls, .disallow_comma= true}, tok);

        if (tok.len && ';' == *tok.ptr)
    case ';':
            ps.base = (declaration){0}; // reset
        // fall through
    case ',':
        tok = lext(&ls);
    }

    bufsl thisns = name_space(file);
    fprintf(result, "static struct adpt_item const adptns_%.*s[] = {\n", bufmt(thisns));
    for (size_t k = 0; k < seen.funs.len; k++) {
        fprintf(result, "    { .name= \"%.*s\"\n", bufmt(seen.funs.ptr[k].name));
        fprintf(result, "    , .type= &%.*s_adapt_type\n", bufmt(seen.funs.ptr[k].name));
        fprintf(result, "    , .as.function= %.*s_adapt_call\n", bufmt(seen.funs.ptr[k].name));
        fprintf(result, "    , .kind= ITEM_VALUE },\n");
    }
    for (size_t k = 0; k < seen.tags.len; k++) {
        fprintf(result, "    { .name= \"@%.*s\"\n", bufmt(seen.tags.ptr[k].name));
        fprintf(result, "    , .type= &%.*s_adapt_tag_type\n", bufmt(seen.tags.ptr[k].name));
        fprintf(result, "    , .kind= ITEM_TYPEDEF },\n");
    }
    for (size_t k = 0; k < seen.tdfs.len; k++) {
        fprintf(result, "    { .name= \"%.*s\"\n", bufmt(seen.tdfs.ptr[k].name));
        fprintf(result, "    , .type= &%.*s_adapt_type\n", bufmt(seen.tdfs.ptr[k].name));
        fprintf(result, "    , .kind= ITEM_TYPEDEF },\n");
    }
    for (size_t k = 0; k < seen.objs.len; k++) {
        errdie("NIY (other kinds of static variable declaration)");
        fprintf(result, "    { .name= \"%.*s\"\n", bufmt(seen.objs.ptr[k].name));
        fprintf(result, "    , .type= &%.*s_adapt_type\n", bufmt(seen.objs.ptr[k].name));
        fprintf(result, "    , .as.object= &%.*s\n", bufmt(seen.objs.ptr[k].name));
        fprintf(result, "    , .kind= ITEM_VARIABLE },\n");
    }
    fprintf(result, "};\n");

    return EXIT_SUCCESS;
}
// }}}

int main(int argc, char** argv)
{
    char* prog = (argc--, *argv++);
    if (!argc || !strcmp("-h", *argv) || !strcmp("--help", *argv)) {
        fprintf(stderr,
                "Usage: %s <entry> [-D...,-I...] -o <a-file.h>\n"
                "       %s -m <a-file.h...> -o <c-main.c>\n"
                "  Without \"-o\", uses standard output.\n"
                "\n"
                "  In the first form, parse <entry> (typically\n"
                "a C library header file) and generate an \"adapter\"\n"
                "<a-file.h>. An \"adapter\" file contains the necessary\n"
                "information for a cintre REPL runtime to figure out how\n"
                "to refer to the objects and functions declared in the\n"
                "<entry> point (and the files it includes via the\n"
                "\"quoted\" form). \"-D\" and \"-I\" have the same meaning as\n"
                "for the C compiler (define macro and include path), and\n"
                "other unrecognized arguments are simply ignored.\n"
                "\n"
                "  The second form (when the first argument is \"-m\")\n"
                "takes a series of previously generated \"adapter\" files\n"
                "and produces a single <c-main.c> entry point file. This\n"
                "file will contain a valid `main` function (included in\n"
                "\"cintre.c\"). Note that the \"adapter\" files are not\n"
                "read; only the names are needed.\n"
                "\n"
                "  A typical processing pipeline is thus as follow,\n"
                "described with makefile syntax:\n"
                "\n"
                "    PR := preparer\n"
                "    CFLAGS := -O1 -std=c99 -Wall\n"
                "\n"
                "    # compile mylib to object\n"
                "    mylib.o: mylib.c mylib.h\n"
                "    	$(CC) -c $< -o $@ $(CFLAGS)\n"
                "\n"
                "    # prepare adapter file for mylib\n"
                "    a-mylib.h: mylib.h $(PR)\n"
                "    	$(PR) $< -o $@ $(CFLAGS)\n"
                "\n"
                "    # prepare adapter file for some standard function\n"
                "    a-standard.h: cintre/standard.h $(PR)\n"
                "    	$(PR) $< -o $@ $(CFLAGS)\n"
                "\n"
                "    # \"merge\" the adapter by preparing the entry point\n"
                "    c-main.c: a-mylib.h a-standard.h\n"
                "    	$(PR) -m $^ -o $@\n"
                "\n"
                "    # finally compile the REPL with the object for mylib\n"
                "    c-main: mylib.o c-main.c\n"
                "    	$(CC) $^ -o $@ $(CFLAGS) -Icintre -DUSE_READLINE -lreadline\n"
                , prog, prog);
        return EXIT_FAILURE;
    }

    return !strcmp("-m", *argv) ? do_merge(argc, argv) : do_prepare(argc, argv);
}

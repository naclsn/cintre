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

#include "common.h"
#define on_lsys on_lsys
void on_lsys(bufsl const path);
#include "parser.h"

lex_state ls = {0};
FILE* result = NULL;
int indent = 0;

bool emit_decl = true, emit_incl = false, emit_sysi = true;

#ifdef LOC_NOTIF
# define EMIT_HERE fprintf(result, " /*\x1b[36m%s(" _HERE_XSTR(__LINE__) ")\x1b[m*/ ", __func__)
#else
# define EMIT_HERE (void)0
#endif

/// last `emit` of an indented should not be an `emitln` (so a plain `emit`)
#define indented(...)  for (bool _once = (++indent, emitln(__VA_ARGS__), true); _once; _once = (--indent, emit_empty(), false))
#define emit(...)  (fprintf(result, __VA_ARGS__), EMIT_HERE)
#define emitln(...)  (emit(__VA_ARGS__), fprintf(result, "\n%*s", indent*4, ""))
#define emit_empty()  fprintf(result, "\n%*s", indent*4, "")

void on_lsys(bufsl const path)
{
    if (emit_sysi) emitln("#include <%.*s>", bufmt(path));
}

struct {
    dyarr(struct seen_fun { bufsl name; }) funs;
    dyarr(struct seen_tag { bufsl name; }) tags;
    dyarr(struct seen_tdf { bufsl name; }) tdfs;
    dyarr(struct seen_obj { bufsl name; }) objs;
} seen = {0};

#define errdie(...) (report_lex_locate(&ls, "preparer: " __VA_ARGS__), exit(EXIT_FAILURE))

#define _linked_it_type_comp struct decl_type_field
#define _linked_it_type_enu struct decl_type_enumer
#define _linked_it_type_fun struct decl_type_param
#define for_linked(__info, __ty) for (_linked_it_type_##__ty* curr = (__info).__ty.first; curr; curr = curr->next)

/// ty->name if not empty, otherwise a unique stable temporary name
/// (allocated on the stack)
#define have_tag_name_bufsl(__ident, __ty)  \
    char __ident_##loc_tmp[35];  \
    if (!(__ty)->name.len) sprintf(__ident_##loc_tmp, "_%016zx_%016zx", (size_t)(__ty), (size_t)ls.slice.ptr);  \
    bufsl const __ident = !(__ty)->__ident.len ? (bufsl){.ptr= __ident_##loc_tmp, .len= sizeof __ident_##loc_tmp-1} : (__ty)->__ident;

/// forwards as-is
void emit_cexpr(expression cref expr)
{
    if (ATOM == expr->kind)
        emit("%.*s", bufmt(expr->info.atom));
    else errdie("NIY: more complex expression than single literal");
}

/// emit a compile-time &-able value that is the adpt_type for this type
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

        {
    case KIND_STRUCT:
    case KIND_UNION:;
            have_tag_name_bufsl(name, ty);
            emit("%.*s_adapt_tag_type", bufmt(name));
        }
        break;

        // xxx: association with the enumerators gets lost here
    case KIND_ENUM:
        emit("adptb_int_type");
        break;

    case KIND_PTR:
        indented (in_decl ? "{" : "(struct adpt_type){") {
            emitln(".size= sizeof(void*),");
            emitln(".align= alignof(void*),");
            emitln(".tyty= TYPE_PTR,");
            emit(".info.ptr= &");
            emit_adpt_type_val(&ty->info.ptr->type, false);
            emit(",");
        }
        emit("}");
        break;

    case KIND_FUN:
        indented (in_decl ? "{" : "(struct adpt_type){") {
            emitln(".size= sizeof(void(*)()),");
            emitln(".align= sizeof(void(*)()),"); // yyy: can't `alignof` here with my janky version of `alignof`
            emitln(".tyty= TYPE_FUN,");
            indented (".info.fun= {") {
                emit(".ret= &");
                emit_adpt_type_val(&ty->info.fun.ret->type, false);
                emitln(",");
                emit(".params= ");
                size_t count = 0;
                if (-1ul == ty->info.fun.count || 0 == ty->info.fun.count) emitln("(void*)0,");
                else {
                    indented ("(struct adpt_fun_param[]){") for_linked (ty->info,fun) {
                        if (!curr->decl) errdie("Variadic functions are not supported");
                        indented ("[%zu]= {", count++) {
                            emitln(".name= \"%.*s\",", bufmt(curr->decl->name));
                            emit(".type= &");
                            emit_adpt_type_val(&curr->decl->type, false);
                            emit(",");
                        }
                        if (curr->next) emitln("},");
                        else emit("},");
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
        indented (in_decl ? "{" : "(struct adpt_type){") {
            emit(".size= ");
            emitln("0,"); // FIXME:
            //emit_adpt_type_val(&ty->info.arr.item->type, false);
            //emit(".size * (");
            //if (!ty->info.arr.count) emit("1"); // yyy: idk
            //else emit_cexpr(ty->info.arr.count);
            //emitln("),");
            emit(".align= ");
            emitln("0,"); // FIXME:
            //emit_adpt_type_val(&ty->info.arr.item->type, false);
            //emitln(".align,");
            emitln(".tyty= TYPE_ARR,");
            indented (".info.arr= {") {
                emit(".item= &");
                emit_adpt_type_val(&ty->info.arr.item->type, false);
                emitln(",");
                emit(".count= ");
                if (!ty->info.arr.count) emit("1"); // yyy: idk
                else emit_cexpr(ty->info.arr.count);
                emit(",");
            }
            emit("},");
        }
        emit("}");
        break;
    }
}

/// forwards as-is
void emit_forward(struct decl_type cref ty, bufsl cref name, bool const in_cast)
{
    for (unsigned k = 0; ty->quals[k]; k++) switch (ty->quals[k]) {
    case QUAL_END: break;
    case QUAL_CONST:                   emit("const ");     break;
    case QUAL_RESTRICT:  if (!in_cast) emit("restrict ");  break;
    case QUAL_VOLATILE:                emit("volatile ");  break;
    case QUAL_SIGNED:                  emit("signed ");    break;
    case QUAL_UNSIGNED:                emit("unsigned ");  break;
    case QUAL_SHORT:                   emit("short ");     break;
    case QUAL_LONG:                    emit("long ");      break;
    case QUAL_COMPLEX:                 emit("complex ");   break;
    case QUAL_IMAGINARY:               emit("imaginary "); break;
    }

    switch (ty->kind) {
    case KIND_NOTAG: emit("%.*s", bufmt(ty->name)); break;

        {
    case KIND_STRUCT: emit("struct "); if (0)
    case KIND_UNION:  emit("union ");
            if (ty->name.len) {
                emit("%.*s", bufmt(ty->name));
                if (-1ul == ty->info.comp.count) break;

                bool found = false;
                search_namespace (ty->name, seen.tags) { found = true; break; }
                if (found) break;
            } else {
                have_tag_name_bufsl(name, ty);
                emit("%.*s", bufmt(name));
            }

            if (in_cast) return;

            if (emit_decl || !ty->name.len) {
                indented (" {") for_linked (ty->info,comp) {
                    emit_forward(&curr->decl->type, &curr->decl->name, in_cast);
                    if (curr->bitw) {
                        emit(" :");
                        emit_cexpr(curr->bitw);
                    }
                    if (curr->next) emitln(";");
                    else emit(";");
                }
                emit("}");
            }

            if (ty->name.len) {
                struct seen_tag ref tag = dyarr_push(&seen.tags);
                if (!tag) errdie("OOM");
                tag->name = ty->name;
            }
        }
        break;

        {
    case KIND_ENUM:
            if (!emit_decl && !ty->name.len) {
                // here: - we can't emit the enumerators (name collision)
                //       - we can't refere to the enum by it's tag name
                // so just say it's an int and the association with the
                // enumerators gets lost here (as in for the C compiler)
                emit("int");
                break;
            }

            emit("enum ");
            if (ty->name.len) {
                emit("%.*s", bufmt(ty->name));

                bool found = false;
                search_namespace (ty->name, seen.tags) { found = true; break; }
                if (found) break;
            } else {
                have_tag_name_bufsl(name, ty);
                emit("%.*s", bufmt(name));
            }

            if (in_cast) return;

            if (emit_decl) {
                indented (" {") for_linked (ty->info,enu) {
                    emit("%.*s", bufmt(curr->name));
                    if (curr->expr) {
                        emit("= ");
                        emit_cexpr(curr->expr);
                    }
                    if (curr->next) emitln(",");
                    else emit(",");
                }
                emit("}");
            }

            // TODO: also need to emit the enumerators and add them to
            // seen.objs as if they were declared `static const int`

            if (ty->name.len) {
                struct seen_tag ref tag = dyarr_push(&seen.tags);
                if (!tag) errdie("OOM");
                tag->name = ty->name;
            }
        }
        break;

    case KIND_PTR:
        // FIXME: hate it but idc
        if (KIND_FUN == ty->info.ptr->type.kind || (KIND_PTR == ty->info.ptr->type.kind && KIND_FUN == ty->info.ptr->type.info.ptr->type.kind)) {
            bool const twice = KIND_PTR == ty->info.ptr->type.kind;
            struct decl_type cref ty2 = twice ? &ty->info.ptr->type.info.ptr->type : &ty->info.ptr->type;
            emit_forward(&ty2->info.fun.ret->type, NULL, in_cast);
            if (name) emit(" (*%s%.*s)", twice ? "*" : "", bufmt(*name));
            else emit(" (*%s)", twice ? "*" : "");
            emit("(");
            if (-1ul != ty2->info.fun.count) {
                if (0 == ty2->info.fun.count) emit("void");
                else for_linked (ty2->info,fun) {
                    if (!curr->decl) errdie("Variadic functions are not supported");
                    emit_forward(&curr->decl->type, &curr->decl->name, in_cast);
                    if (curr->next) emit(", ");
                }
            }
            emit(")");
        }
        else if (KIND_ARR == ty->info.ptr->type.kind || (KIND_PTR == ty->info.ptr->type.kind && KIND_ARR == ty->info.ptr->type.info.ptr->type.kind)) {
            bool const twice = KIND_PTR == ty->info.ptr->type.kind;
            struct decl_type cref ty2 = twice ? &ty->info.ptr->type.info.ptr->type : &ty->info.ptr->type;
            emit_forward(&ty2->info.arr.item->type, NULL, in_cast);
            if (name) emit(" (*%.*s)", bufmt(*name));
            else emit(" (*%s)", twice ? "*" : "");
            emit("[");
            if (ty2->info.arr.count) emit_cexpr(ty2->info.arr.count);
            emit("]");
        }

        else {
            emit_forward(&ty->info.ptr->type, NULL, in_cast);
            if (name) emit("* %.*s", bufmt(*name));
            else emit("*");
        }
        return; // name already done

    case KIND_FUN:
        emit_forward(&ty->info.fun.ret->type, NULL, in_cast);
        if (name) emit(" %.*s", bufmt(*name));
        emit("(");
        if (-1ul != ty->info.fun.count) {
            if (0 == ty->info.fun.count) emit("void");
            else for_linked (ty->info,fun) {
                if (!curr->decl) errdie("Variadic functions are not supported");
                emit_forward(&curr->decl->type, &curr->decl->name, in_cast);
                if (curr->next) emit(", ");
            }
        }
        emit(")");
        return; // name already done

    case KIND_ARR:
        emit_forward(&ty->info.arr.item->type, NULL, in_cast);
        if (name) emit(" %.*s", bufmt(*name));
        emit("[");
        if (ty->info.arr.count) emit_cexpr(ty->info.arr.count);
        emit("]");
        return; // name already done
    }

    if (name && name->len) emit(" %.*s", bufmt(*name));
}

/// emit nested named struct/union that are defined, depth first, ie define the
/// decl_type <name>_adapt_tag_type; and also makes the _%p_%p unnamed tag ones
/// when -Pno-emit-decl because we still need these and because why not be
/// hacky all the way
void emit_named_comps_adpt_type_def(struct decl_type cref ty)
{
    switch (ty->kind) {
    case KIND_PTR: emit_named_comps_adpt_type_def(&ty->info.ptr->type);      return;
    case KIND_FUN: emit_named_comps_adpt_type_def(&ty->info.fun.ret->type);  return;
    case KIND_ARR: emit_named_comps_adpt_type_def(&ty->info.arr.item->type); return;

    case KIND_STRUCT:
    case KIND_UNION:
        if (-1ul == ty->info.comp.count)
    default:
            return;
    }

    if (!emit_decl && !ty->name.len) {
        emit_forward(ty, NULL, false);
        emitln(";");
    }

    have_tag_name_bufsl(name, ty);

    for_linked (ty->info,comp) emit_named_comps_adpt_type_def(&curr->decl->type);

    char const* const lo_kind = KIND_UNION == ty->kind ? "union" : "struct";
    char const* const hi_kind = KIND_UNION == ty->kind ? "UNION" : "STRUCT";

    emit("static struct adpt_type const %.*s_adapt_tag_type = ", bufmt(name));

    indented ("{") {
        emitln(".size= sizeof(%s %.*s),", lo_kind, bufmt(name));
        emitln(".align= alignof(%s %.*s),", lo_kind, bufmt(name));
        emitln(".tyty= TYPE_%s,", hi_kind);
        indented (".info.comp= {") {
            size_t count = 0;
            indented (".fields= (struct adpt_comp_field[]){") for_linked (ty->info,comp) {
                indented ("[%zu]= {", count++) {
                    emitln(".name= \"%.*s\",", bufmt(curr->decl->name));
                    emit(".type= &");
                    emit_adpt_type_val(&curr->decl->type, false);
                    emitln(",");
                    emit(".offset= offsetof(%s %.*s, %.*s),", lo_kind, bufmt(name), bufmt(curr->decl->name));
                }
                if (curr->next) emitln("},");
                else emit("},");
            }
            emitln("},");
            emit(".count= %zu,", count);
        }
        emit("},");
    }
    emit("}");

    emitln(";");
}

/// true if the expression from `emit_adpt_type_val` cannot be used to
/// initialize a static constant, eg this would not be standard:
/// ```c
/// static struct adpt_type const aa_adapt_type = adptb_int_type;
/// ```
bool adpt_type_val_needs_define(struct decl_type cref ty) {
    switch (ty->kind) {
    case KIND_NOTAG:
    case KIND_STRUCT:
    case KIND_UNION:
    case KIND_ENUM:
        return true;
    default:
        return false;
    }
}

void emit_extern(declaration cref decl)
{
    if (emit_decl) {
        if (decl->name.len) {
            emit("extern ");
            emit_forward(&decl->type, &decl->name, false);
            emitln(";");
        } else switch (decl->type.kind) {
        case KIND_STRUCT:
        case KIND_UNION:
        case KIND_ENUM:
            emit_forward(&decl->type, NULL, false);
            emitln(";");
        default:;
        }
    }
    emit_named_comps_adpt_type_def(&decl->type);

    if (KIND_FUN == decl->type.kind) {
        bool found = false;
        search_namespace (decl->name, seen.funs) { found = true; break; }
        if (found) return;

        indented ("void %.*s_adapt_call(char* ret, char** args) {", bufmt(decl->name)) {
            if (0 == decl->type.info.fun.count) emitln("(void)args;");
            if (bufis(decl->type.info.fun.ret->type.name, "void")) emitln("(void)ret;");
            else {
                emit("*(");
                emit_forward(&(struct decl_type){
                        .kind= KIND_PTR,
                        .info.ptr= decl->type.info.fun.ret,
                    }, NULL, true);
                emit(")ret = ");
            }
            emit("%.*s(", bufmt(decl->name));
            size_t k = 0;
            for_linked (decl->type.info,fun) {
                if (!curr->decl) errdie("Variadic functions are not supported");
                emit("*(");
                emit_forward(&(struct decl_type){
                        .kind= KIND_PTR,
                        .info.ptr= curr->decl,
                    }, NULL, true);
                emit(")args[%zu]", k++);
                if (curr->next) emit(", ");
            }
            emit(");");
        }
        emitln("}");

        struct seen_fun ref fun = dyarr_push(&seen.funs);
        if (!fun) errdie("OOM");
        fun->name = decl->name;
    }

    else if (decl->name.len) {
        bool found = false;
        search_namespace (decl->name, seen.objs) { found = true; break; }
        if (found) return;

        emitln("// object not quite handled anywhere yet");

        struct seen_obj ref obj = dyarr_push(&seen.objs);
        if (!obj) errdie("OOM");
        obj->name = decl->name;
    }

    if (decl->name.len) {
        bool const use_def = adpt_type_val_needs_define(&decl->type);
        if (use_def) emit("#define %.*s_adapt_type ", bufmt(decl->name));
        else emit("static struct adpt_type const %.*s_adapt_type = ", bufmt(decl->name));
        emit_adpt_type_val(&decl->type, true);
        if (use_def) emit_empty();
        else emitln(";");
    }
}

void emit_typedef(declaration cref decl)
{
    if (emit_decl) {
        emit("typedef ");
        emit_forward(&decl->type, &decl->name, false);
        emitln(";");
    }
    emit_named_comps_adpt_type_def(&decl->type);

    bool const use_def = adpt_type_val_needs_define(&decl->type);
    if (use_def) emit("#define %.*s_adapt_tdf_type ", bufmt(decl->name));
    else emit("static struct adpt_type const %.*s_adapt_tdf_type = ", bufmt(decl->name));
    emit_adpt_type_val(&decl->type, true);
    if (use_def) emit_empty();
    else emitln(";");

    emitln("#define %.*s_adapt_type %.*s_adapt_tdf_type", bufmt(decl->name), bufmt(decl->name));

    struct seen_tdf ref tdf = dyarr_push(&seen.tdfs);
    if (!tdf) errdie("OOM");
    tdf->name = decl->name;
}

void emit_top(void* _, declaration cref decl, bufsl ref tok)
{
    (void)_;
    (void)tok;

    if (decl->is_inline) return;

    switch (decl->spec) {
    case SPEC_STATIC: // internal linkage (not visible)
    default: // others are unreachable
        return;

    case SPEC_EXTERN: // external linkage
    case SPEC_NONE: // default at file scope is external linkage
        emit_extern(decl);
        emit_empty();
        return;

    case SPEC_TYPEDEF:
        emit_typedef(decl);
        emit_empty();
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

    char const* basename = strrchr(itns.ptr, '/');
    if (basename) itns.ptr = basename+1;

    char const* fileext = strchr(itns.ptr, '.');
    if (fileext) itns.len = fileext - itns.ptr;

    for (size_t k = 0; k < itns.len; k++)
        if ((itns.ptr[k]|32) < 'a' || 'z' < (itns.ptr[k]|32))
            ((char*)itns.ptr)[k] = '_';

    // adapter files are expected to start with "a-",
    // tho this is not strictly mandatory I suppose...
    // this will also catch false positives :/
    if ('a' == itns.ptr[0] && '_' == itns.ptr[1]) {
        itns.ptr+= 2;
        itns.len-= 2;
    }

    return itns;
}
// }}}

// do-s {{{
int do_merge(int argc, char** argv)
{
    char** const first = (--argc, ++argv);
    result = stdout;
    atexit(cleanup);

    char** past_end = NULL;
    while (0 < argc) {
        char* arg = (argc--, *argv++);
        char* val;
        if (!memcmp("-o", arg, 2)) {
            past_end = argv-1;
            val = arg[2] ? arg+2 : (argc--, *argv++);
            if (!val) result = NULL;
            else if ('-' != val[0]) result = fopen(val, "wb");
            break;
        }
    }
    if (!past_end) past_end = argv;
    if (!result) exitf("Missing result operand or file not writable");

    if (first != past_end) {
        for (char** it = first; it < past_end; it++) emitln("#include \"%s\"", *it);
        emit_empty();

        indented ("static struct adpt_namespace const namespaces[] = {") for (char** it = first; it < past_end; it++) {
            bufsl const itns = name_space(*it);
            emit("{.name= \"%.*s\", .count= sizeof adptns_%.*s/sizeof*adptns_%.*s, .items= adptns_%.*s}", bufmt(itns), bufmt(itns), bufmt(itns), bufmt(itns));
            if (past_end == it+1) emit(",");
            else emitln(",");
        }
        emitln("};");
        emit_empty();

        emitln("#define CINTRE_NAMESPACES_DEFINED");
    }

    emitln("#include \"cintre.c\"");

    return EXIT_SUCCESS;
}

int do_prepare(int argc, char** argv)
{
    char* infile = (argc--, *argv++);
    char* outfile = NULL;
    result = stdout;
    atexit(cleanup);
    emit_decl = true, emit_incl = false;

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

        case 'P':
            val = arg+2;
            if (!*val) break;
#           define flagis(l) (!memcmp((l), val, strlen(l)))
            else if (flagis("no-emit-decl")) emit_decl = false, emit_incl = true;
            else if (flagis("no-emit-incl")) emit_incl = false;
            else if (flagis("no-emit-sysi")) emit_sysi = false;
#           undef flagis
            break;

        case 'o':
            val = arg[2] ? arg+2 : (argc--, *argv++);
            if (!val) result = NULL;
            else if ('-' != val[0]) result = fopen(outfile = val, "wb");
            break;
        }
    }

    if (!result) exitf("Missing result operand or file not writable");

    emitln("#include \"adapter.h\"");
    if (emit_incl) emitln("#include \"%s\"", infile);
    emit_empty();

    lini(&ls, infile);

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
        continue;

    default:
        return EXIT_FAILURE;
    }

    bufsl const thisns = name_space(outfile ? outfile : infile);
    indented ("static struct adpt_item const adptns_%.*s[] = {", bufmt(thisns)) {
        for (size_t k = 0; k < seen.funs.len; k++) emitln("{.name= \"%.*s\", .type= &%.*s_adapt_type, .kind= ITEM_VALUE, .as.function= %.*s_adapt_call},", bufmt(seen.funs.ptr[k].name), bufmt(seen.funs.ptr[k].name), bufmt(seen.funs.ptr[k].name));
        for (size_t k = 0; k < seen.tags.len; k++) emitln("{.name= \"@%.*s\", .type= &%.*s_adapt_tag_type, .kind= ITEM_TYPEDEF},", bufmt(seen.tags.ptr[k].name), bufmt(seen.tags.ptr[k].name));
        for (size_t k = 0; k < seen.tdfs.len; k++) emitln("{.name= \"%.*s\", .type= &%.*s_adapt_type, .kind= ITEM_TYPEDEF},", bufmt(seen.tdfs.ptr[k].name), bufmt(seen.tdfs.ptr[k].name));
        for (size_t k = 0; k < seen.objs.len; k++) emitln("{.name= \"%.*s\", .type= &%.*s_adapt_type, .kind= ITEM_VALUE, .as.object= &%.*s},", bufmt(seen.objs.ptr[k].name), bufmt(seen.objs.ptr[k].name), bufmt(seen.objs.ptr[k].name));
        size_t const count = seen.funs.len+seen.tags.len+seen.tdfs.len+seen.objs.len;
        emit("// exporting %zu name%s", count, 1 == count ? "" : "s");
    }
    emitln("};");

    return EXIT_SUCCESS;
}
// }}}

int main(int argc, char** argv)
{
    char* prog = (argc--, *argv++);
    if (!argc || !strcmp("-h", *argv) || !strcmp("--help", *argv)) {
        fprintf(stderr,
                "Usage: %s <entry> [-D...,-I...] [-P...] -o <a-file.h>\n"
                "       %s -m <a-file.h...> -o <c-main.c>\n"
                "  Without \"-o\", uses standard output.\n"
                "\n"
                "  In the first form, parse <entry> (typically\n"
                "a C library header file) and generate an \"adapter\"\n"
                "<a-file.h>. An \"adapter\" file contains the necessary\n"
                "information for a cintre REPL runtime to figure out how\n"
                "to refer to the objects and functions declared in the\n"
                "<entry> point (and the files it includes via the\n"
                "`#include \"quoted\"` form). \"-D\" and \"-I\" have the\n"
                "same meaning as for the C compiler (define macro and\n"
                "include path), unrecognized arguments are ignored. \"-P\"\n"
                "is use as leader for preparer flags:\n"
                "  -Pno-emit-decl  do not declare the C object/functions\n"
                "                  instead emit a `#include \"\"` to the\n"
                "                  entry file\n"
                "  -Pno-emit-incl  do not even emit the `#include \"\"`\n"
                "  -Pno-emit-sysi  do not forward the `#include <>`\n"
                "\n"
                "  The second form (when the first argument is \"-m\")\n"
                "takes a series of previously generated \"adapter\" files\n"
                "and produces a single <c-main.c> entry point file. This\n"
                "file will contain a valid `main` function (included from\n"
                "\"cintre.c\"). Note that the \"adapter\" files are not\n"
                "read; only the names are needed.\n"
                "\n"
                "  A typical processing pipeline would be (in make syntax):\n"
                "\n"
                "PR := %s\n"
                "CFLAGS := -O1 -std=c99 -Wall\n"
                "\n"
                "# compile mylib to object\n"
                "mylib.o: mylib.c mylib.h\n"
                "	$(CC) -c $< -o $@ $(CFLAGS) $(CFLAGS-mylib)\n"
                "\n"
                "# prepare adapter file for mylib\n"
                "a-mylib.h: mylib.h $(PR)\n"
                "	$(PR) $< -o $@ $(CFLAGS) $(CFLAGS-mylib)\n"
                "\n"
                "# prepare adapter file for a bunch of standard things\n"
                "a-standard.h: cintre/standard.h $(PR)\n"
                "	$(PR) $< -o $@ $(CFLAGS)\n"
                "\n"
                "# \"merge\" the adapter by preparing the entry point\n"
                "c-main.c: a-mylib.h a-standard.h\n"
                "	$(PR) -m $^ -o $@\n"
                "\n"
                "# finally compile the REPL with the object for mylib\n"
                "c-main: mylib.o c-main.c\n"
                "	$(CC) $^ -o $@ $(CFLAGS) -Icintre -DUSE_READLINE -lreadline\n"
                , prog, prog, prog);
        return EXIT_FAILURE;
    }

    return !strcmp("-m", *argv) ? do_merge(argc, argv) : do_prepare(argc, argv);
}

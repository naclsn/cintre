/// Program to generate the <a-file.h> and <c-main.h>;
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
#define on_lex_sysinclude(ls, path) on_lex_sysinclude(path)
#define on_lex_missinclude(ls, path) on_lex_missinclude(path)
void (on_lex_sysinclude)(char cref path);
void (on_lex_missinclude)(char cref path);
#include "parser.h"

void emit_top(void* _, declaration cref decl, tokt ref tok);

lex_state _ls = {0}, * ls = &_ls;
parse_decl_state _ps = {.ls= &_ls, .on= emit_top}, * ps = &_ps;
FILE* result = NULL;
int indent = 0;

bool emit_decl = true, emit_incl = false, emit_sysi = true, follow_incl = false;

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

struct {
    dyarr(struct seen_fun { tokt name; }) funs;
    dyarr(struct seen_tag { tokt name; }) tags;
    dyarr(struct seen_tdf { tokt name; }) tdfs;
    dyarr(struct seen_obj { tokt name; }) objs;
    dyarr(struct seen_enu { tokt name; }) enus;
    dyarr(struct seen_sta { tokt name; }) stas;
} seen = {0};
#define find_seen(n, ns) for (size_t k = 0; k < (ns).len; k++) if (!strcmp(tokn((ns).ptr[k].name), tokn((n))))

#define errdie(...) (report_lex_locate(ls, "preparer: " __VA_ARGS__), exit(EXIT_FAILURE))

// so as to remember to put an empty line after the last one
bool just_did_sys_include = false;
void (on_lex_sysinclude)(char cref path)
{
    if (emit_sysi) {
        emitln("#include <%s>", path);
        just_did_sys_include = true;
    }
}
void (on_lex_missinclude)(char cref path)
{
    errdie("Could not include \"%s\"", path);
}

#define _linked_it_type_comp struct decl_type_field
#define _linked_it_type_enu struct decl_type_enumer
#define _linked_it_type_fun struct decl_type_param
#define for_linked(__info, __ty) for (_linked_it_type_##__ty* curr = (__info).__ty.first; curr; curr = curr->next)

/// ty->name if not empty, otherwise a unique stable temporary name
/// (allocated on the stack)
#define have_tag_name_bufsl(__ident, __ty)                                               \
    char __ident_##loc_tmp[35];                                                          \
    char cref __ident = !*tokn((__ty)->name)                                             \
        ? sprintf(__ident_##loc_tmp, "_%016zx_%016zx", (size_t)(__ty), ls->tokens.len),  \
          __ident_##loc_tmp                                                              \
        : tokn((__ty)->name);

/// forwards as-is
void emit_cexpr(expression cref expr)
{
    emit("(");
    switch (expr->kind) {
    case EXPR_ATOM:
        emit("%s", tokn(expr->info.atom));
        break;

    case EXPR_BINOP_SUBSCR:
        emit_cexpr(expr->info.subscr.base);
        emit("[");
        emit_cexpr(expr->info.subscr.off);
        emit("]");
        break;

    case EXPR_BINOP_CALL:
        emit_cexpr(expr->info.call.base);
        emit("(");
        for (struct expr_call_arg* it = expr->info.call.first; it; it = it->next) {
            emit_cexpr(it->expr);
            if (it->next) emit(", ");
        }
        emit(")");
        break;

    case EXPR_BINOP_TERNBRANCH:
        emit("0");
        break;
    case EXPR_BINOP_TERNCOND:
        emit_cexpr(expr->info.binary.lhs); // condition
        emit(" ? ");
        emit_cexpr(expr->info.binary.rhs->info.binary.lhs); // consequence
        emit(" : ");
        emit_cexpr(expr->info.binary.rhs->info.binary.rhs); // alternative
        break;

        char const* binop;
    case EXPR_BINOP_COMMA:     binop = ", ";   goto binop;
    case EXPR_BINOP_ASGN:      binop = " = ";  goto binop;
    case EXPR_BINOP_ASGN_BOR:  binop = "|= ";  goto binop;
    case EXPR_BINOP_ASGN_BXOR: binop = "^= ";  goto binop;
    case EXPR_BINOP_ASGN_BAND: binop = "&= ";  goto binop;
    case EXPR_BINOP_ASGN_BSHL: binop = "<<= "; goto binop;
    case EXPR_BINOP_ASGN_BSHR: binop = ">>= "; goto binop;
    case EXPR_BINOP_ASGN_SUB:  binop = "-= ";  goto binop;
    case EXPR_BINOP_ASGN_ADD:  binop = "+= ";  goto binop;
    case EXPR_BINOP_ASGN_REM:  binop = "%= ";  goto binop;
    case EXPR_BINOP_ASGN_DIV:  binop = "/= ";  goto binop;
    case EXPR_BINOP_ASGN_MUL:  binop = "*= ";  goto binop;
    case EXPR_BINOP_LOR:       binop = " || "; goto binop;
    case EXPR_BINOP_LAND:      binop = " && "; goto binop;
    case EXPR_BINOP_BOR:       binop = " | ";  goto binop;
    case EXPR_BINOP_BXOR:      binop = " ^ ";  goto binop;
    case EXPR_BINOP_BAND:      binop = " & ";  goto binop;
    case EXPR_BINOP_EQ:        binop = " == "; goto binop;
    case EXPR_BINOP_NE:        binop = " != "; goto binop;
    case EXPR_BINOP_LT:        binop = " < ";  goto binop;
    case EXPR_BINOP_GT:        binop = " > ";  goto binop;
    case EXPR_BINOP_LE:        binop = " <= "; goto binop;
    case EXPR_BINOP_GE:        binop = " >= "; goto binop;
    case EXPR_BINOP_BSHL:      binop = "<<";   goto binop;
    case EXPR_BINOP_BSHR:      binop = ">>";   goto binop;
    case EXPR_BINOP_SUB:       binop = "-";    goto binop;
    case EXPR_BINOP_ADD:       binop = "+";    goto binop;
    case EXPR_BINOP_REM:       binop = "%";    goto binop;
    case EXPR_BINOP_DIV:       binop = "/";    goto binop;
    case EXPR_BINOP_MUL:       binop = "*";    goto binop;
    binop:
        emit_cexpr(expr->info.binary.lhs);
        emit("%s", binop);
        emit_cexpr(expr->info.binary.rhs);
        break;

    case EXPR_UNOP_CAST:
        emit("(");
        errdie("TODO: emit_decl_type(expr->info.cast.type)");
        emit(")");
        emit_cexpr(expr->info.cast.opr);
        break;

        char const* unop;
    case EXPR_UNOP_ADDR:    unop = "&";  goto unop;
    case EXPR_UNOP_DEREF:   unop = "*";  goto unop;
    case EXPR_UNOP_BNOT:    unop = "~";  goto unop;
    case EXPR_UNOP_LNOT:    unop = "!";  goto unop;
    case EXPR_UNOP_MINUS:   unop = "-";  goto unop;
    case EXPR_UNOP_PLUS:    unop = "+";  goto unop;
    case EXPR_UNOP_PRE_DEC: unop = "--"; goto unop;
    case EXPR_UNOP_PRE_INC: unop = "++"; goto unop;
    unop:
        emit("%s", unop);
        emit_cexpr(expr->info.unary.opr);
        break;

    case EXPR_UNOP_POST_DEC:
        emit_cexpr(expr->info.unary.opr);
        emit("--");
        break;
    case EXPR_UNOP_POST_INC:
        emit_cexpr(expr->info.unary.opr);
        emit("++");
        break;

    case EXPR_UNOP_PMEMBER:
        emit_cexpr(expr->info.member.base);
        emit("->%s", tokn(expr->info.member.name));
        break;
    case EXPR_UNOP_MEMBER:
        emit_cexpr(expr->info.member.base);
        emit(".%s", tokn(expr->info.member.name));
        break;
    }
    emit(")");
}
void _emit_cexpr(void* _, expression ref expr, tokt ref tok) { (void)_; (void)tok; emit_cexpr(expr); }

/// emit a compile-time &-able value that is the adpt_type for this type
/// if `in_decl` is true, will not do `(struct adpt_type){..}` but just `{..}`
/// if `do_decay` is true, will do array to pointer 'decay'
void emit_adpt_type_val(struct decl_type cref ty, bool const in_decl, bool const do_decay)
{
    switch (ty->kind) {
    case DECL_KIND_NOTAG:;
        bool _signed = false, _unsigned = false, _short = false, _long = false;
        for (size_t k = 0; DECL_QUAL_END != ty->quals[k]; k++) switch (ty->quals[k]) {
            case DECL_QUAL_SIGNED:    _signed = true;          break;
            case DECL_QUAL_UNSIGNED:  _unsigned = true;        break;
            case DECL_QUAL_SHORT:     _short = true;           break;
            case DECL_QUAL_LONG:      _long = true;            break;
            case DECL_QUAL_COMPLEX:   exitf("NIY: complex");   break;
            case DECL_QUAL_IMAGINARY: exitf("NIY: imaginary"); break;
                // noop cases
            case DECL_QUAL_END: case DECL_QUAL_CONST: case DECL_QUAL_RESTRICT: case DECL_QUAL_VOLATILE:;
            }

        char const* adptb = NULL;
#       define nameis(s)  (!strcmp(s, tokn(ty->name)))
        if (nameis("char"))
            adptb = _signed ? "adptb_schar_type"
                  : _unsigned ? "adptb_uchar_type"
                  : "adptb_char_type";
        if (nameis("int"))
            adptb = _short ? (_unsigned ? "adptb_ushort_type" : "adptb_short_type")
                  : _long ? (_unsigned ? "adptb_ulong_type" : "adptb_long_type")
                  : _unsigned ? "adptb_uint_type" : "adptb_int_type";
        if (nameis("float"))  adptb = "adptb_float_type";
        if (nameis("double")) adptb = "adptb_double_type";
        if (nameis("void"))   adptb = "adptb_void_type";
#       undef nameis

        if (adptb) emit("%s", adptb);
        else emit("%s_adapt_type", tokn(ty->name));
        break;

        {
    case DECL_KIND_STRUCT:
    case DECL_KIND_UNION:;
            have_tag_name_bufsl(name, ty);
            emit("%s_adapt_tag_type", name);
        }
        break;

        // xxx: association with the enumerators gets lost here
    case DECL_KIND_ENUM:
        emit("adptb_int_type");
        break;

    case DECL_KIND_PTR:
        indented (in_decl ? "{" : "(struct adpt_type){") {
            emitln(".size= sizeof(void*),");
            emitln(".align= alignof(void*),");
            emitln(".tyty= ADPT_TYPE_PTR,");
            emit(".info.ptr= &");
            emit_adpt_type_val(&ty->info.ptr->type, false, false);
            emit(",");
        }
        emit("}");
        break;

    case DECL_KIND_FUN:
        indented (in_decl ? "{" : "(struct adpt_type){") {
            emitln(".size= sizeof(void(*)()),");
            emitln(".align= sizeof(void(*)()),"); // yyy: can't `alignof` here with my janky version of `alignof`
            emitln(".tyty= ADPT_TYPE_FUN,");
            indented (".info.fun= {") {
                emit(".ret= &");
                emit_adpt_type_val(&ty->info.fun.ret->type, false, false);
                emitln(",");
                emit(".params= ");
                size_t count = 0;
                if (-1ul == ty->info.fun.count || 0 == ty->info.fun.count) emitln("(void*)0,");
                else {
                    indented ("(struct adpt_fun_param[]){") for_linked (ty->info,fun) {
                        if (!curr->decl) errdie("Variadic functions are not supported");
                        indented ("[%zu]= {", count++) {
                            emitln(".name= \"%s\",", tokn(curr->decl->name));
                            emit(".type= &");
                            emit_adpt_type_val(&curr->decl->type, false, true);
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

    case DECL_KIND_ARR:
        if (do_decay) {
            indented (in_decl ? "{" : "(struct adpt_type){") {
                emitln(".size= sizeof(void*),");
                emitln(".align= alignof(void*),");
                emitln(".tyty= ADPT_TYPE_PTR,");
                emit(".info.ptr= &");
                emit_adpt_type_val(&ty->info.arr.item->type, false, false);
                emit(",");
            }
            emit("}");
            break;
        } // else
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
            emitln(".tyty= ADPT_TYPE_ARR,");
            indented (".info.arr= {") {
                emit(".item= &");
                emit_adpt_type_val(&ty->info.arr.item->type, false, false);
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
void emit_forward(struct decl_type cref ty, char cref name, bool const in_cast)
{
    switch (ty->kind) {
    case DECL_KIND_NOTAG: emit("%s", tokn(ty->name)); break;

        {
    case DECL_KIND_STRUCT: emit("struct "); if (0)
    case DECL_KIND_UNION:  emit("union ");
            if (*tokn(ty->name)) {
                emit("%s", tokn(ty->name));
                if (-1ul == ty->info.comp.count) break;

                bool found = false;
                find_seen (ty->name, seen.tags) { found = true; break; }
                if (found) break;
            } else {
                have_tag_name_bufsl(name, ty);
                emit("%s", name);
            }

            if (in_cast) return;

            if (*tokn(ty->name)) {
                struct seen_tag ref tag = dyarr_push(&seen.tags);
                if (!tag) errdie("OOM");
                tag->name = ty->name;
            }

            if (emit_decl || !*tokn(ty->name)) {
                indented (" {") for_linked (ty->info,comp) {
                    emit_forward(&curr->decl->type, tokn(curr->decl->name), in_cast);
                    if (curr->bitw) {
                        emit(" :");
                        emit_cexpr(curr->bitw);
                    }
                    if (curr->next) emitln(";");
                    else emit(";");
                }
                emit("}");
            }
        }
        break;

        {
    case DECL_KIND_ENUM:
            if (!emit_decl && !*tokn(ty->name)) {
                // here: - we can't emit the enumerators (name collision)
                //       - we can't refere to the enum by it's tag name
                // so just say it's an int and the association with the
                // enumerators gets lost here (as in for the C compiler)
                emit("int");
                break;
            }

            emit("enum ");
            if (*tokn(ty->name)) {
                emit("%s", tokn(ty->name));

                bool found = false;
                find_seen (ty->name, seen.tags) { found = true; break; }
                if (found) break;
            } else {
                have_tag_name_bufsl(name, ty);
                emit("%s", name);
            }

            if (in_cast) return;

            if (*tokn(ty->name)) {
                struct seen_tag ref tag = dyarr_push(&seen.tags);
                if (!tag) errdie("OOM");
                tag->name = ty->name;
            }

            if (emit_decl && ty->info.enu.count) {
                indented (" {") for_linked (ty->info,enu) {
                    emit("%s", tokn(curr->name));
                    if (curr->expr) {
                        emit("= ");
                        emit_cexpr(curr->expr);
                    }
                    if (curr->next) emitln(",");
                    else emit(",");

                    struct seen_enu ref enu = dyarr_push(&seen.enus);
                    if (!enu) errdie("OOM");
                    enu->name = curr->name;
                }
                emit("}");
            }
        }
        break;

    case DECL_KIND_PTR:
        // FIXME: hate it but idc
        if (DECL_KIND_FUN == ty->info.ptr->type.kind || (DECL_KIND_PTR == ty->info.ptr->type.kind && DECL_KIND_FUN == ty->info.ptr->type.info.ptr->type.kind)) {
            bool const twice = DECL_KIND_PTR == ty->info.ptr->type.kind;
            struct decl_type cref ty2 = twice ? &ty->info.ptr->type.info.ptr->type : &ty->info.ptr->type;
            emit_forward(&ty2->info.fun.ret->type, NULL, in_cast);
            // const and such (after '*')
            if (name) emit(" (*%s%s)", twice ? "*" : "", name);
            else emit(" (*%s)", twice ? "*" : "");
            emit("(");
            if (-1ul != ty2->info.fun.count) {
                if (0 == ty2->info.fun.count) emit("void");
                else for_linked (ty2->info,fun) {
                    if (!curr->decl) errdie("Variadic functions are not supported");
                    emit_forward(&curr->decl->type, tokn(curr->decl->name), in_cast);
                    if (curr->next) emit(", ");
                }
            }
            emit(")");
            return; // name already done
        }
        else if (DECL_KIND_ARR == ty->info.ptr->type.kind || (DECL_KIND_PTR == ty->info.ptr->type.kind && DECL_KIND_ARR == ty->info.ptr->type.info.ptr->type.kind)) {
            bool const twice = DECL_KIND_PTR == ty->info.ptr->type.kind;
            struct decl_type cref ty2 = twice ? &ty->info.ptr->type.info.ptr->type : &ty->info.ptr->type;
            emit_forward(&ty2->info.arr.item->type, NULL, in_cast);
            if (name) emit(" (**%s)", name);
            else emit(" (**%s)", twice ? "*" : "");
            //emit("[");
            //if (ty2->info.arr.count) emit_cexpr(ty2->info.arr.count);
            //emit("]");
            return; // name already done
        }

        else {
            emit_forward(&ty->info.ptr->type, NULL, in_cast);
            emit("*");
        }
        break;

    case DECL_KIND_FUN:
        emit_forward(&ty->info.fun.ret->type, NULL, in_cast);
        if (name) emit(" %s", name);
        emit("(");
        if (-1ul != ty->info.fun.count) {
            if (0 == ty->info.fun.count) emit("void");
            else for_linked (ty->info,fun) {
                if (!curr->decl) errdie("Variadic functions are not supported");
                emit_forward(&curr->decl->type, tokn(curr->decl->name), in_cast);
                if (curr->next) emit(", ");
            }
        }
        emit(")");
        return; // name already done

    case DECL_KIND_ARR:
        emit_forward(&ty->info.arr.item->type, NULL, in_cast);
        if (in_cast) {
            emit("*");
            break;
        } else {
            if (name) emit(" %s", name);
            emit("[");
            if (ty->info.arr.count) emit_cexpr(ty->info.arr.count);
            emit("]");
            return; // name already done
        }
    }

    for (unsigned k = 0; ty->quals[k]; k++) switch (ty->quals[k]) {
    case DECL_QUAL_END: break;
    case DECL_QUAL_CONST:                   emit(" const");     break;
    case DECL_QUAL_RESTRICT:  if (!in_cast) emit(" restrict");  break;
    case DECL_QUAL_VOLATILE:                emit(" volatile");  break;
    case DECL_QUAL_SIGNED:                  emit(" signed");    break;
    case DECL_QUAL_UNSIGNED:                emit(" unsigned");  break;
    case DECL_QUAL_SHORT:                   emit(" short");     break;
    case DECL_QUAL_LONG:                    emit(" long");      break;
    case DECL_QUAL_COMPLEX:                 emit(" complex");   break;
    case DECL_QUAL_IMAGINARY:               emit(" imaginary"); break;
    }

    if (name && *name) emit(" %s", name);
}

/// emit nested named struct/union that are defined, depth first, ie define the
/// decl_type <name>_adapt_tag_type; and also makes the _%p_%p unnamed tag ones
/// when -Pno-emit-decl because we still need these and because why not be
/// hacky all the way
void emit_named_comps_adpt_type_def(struct decl_type cref ty)
{
    switch (ty->kind) {
    case DECL_KIND_PTR: emit_named_comps_adpt_type_def(&ty->info.ptr->type);      return;
    case DECL_KIND_FUN: emit_named_comps_adpt_type_def(&ty->info.fun.ret->type);  return;
    case DECL_KIND_ARR: emit_named_comps_adpt_type_def(&ty->info.arr.item->type); return;

    case DECL_KIND_ENUM:
        if (*tokn(ty->name)) emitln("#define %s_adapt_tag_type adptb_int_type", tokn(ty->name));
        return;

    case DECL_KIND_STRUCT:
    case DECL_KIND_UNION:
        if (-1ul == ty->info.comp.count)
    case DECL_KIND_NOTAG:
            return;
    }

    if (!emit_decl && !*tokn(ty->name)) {
        emit_forward(ty, NULL, false);
        emitln(";");
    }

    have_tag_name_bufsl(name, ty);
    emitln("static struct adpt_type const %s_adapt_tag_type;", name);

    for_linked (ty->info,comp) emit_named_comps_adpt_type_def(&curr->decl->type);

    char const* const comp_kind = DECL_KIND_UNION == ty->kind ? "union" : "struct";

    emit("static struct adpt_type const %s_adapt_tag_type = ", name);

    indented ("{") {
        emitln(".size= sizeof(%s %s),", comp_kind, name);
        emitln(".align= alignof(%s %s),", comp_kind, name);
        emitln(".tyty= ADPT_TYPE_%s,", DECL_KIND_UNION == ty->kind ? "UNION" : "STRUCT");
        indented (".info.comp= {") {
            if (*tokn(ty->name)) emitln(".named= \"%s\",", tokn(ty->name));
            size_t count = 0;
            indented (".fields= (struct adpt_comp_field[]){") for_linked (ty->info,comp) {
                indented ("[%zu]= {", count++) {
                    emitln(".name= \"%s\",", tokn(curr->decl->name));
                    emit(".type= &");
                    emit_adpt_type_val(&curr->decl->type, false, false);
                    emitln(",");
                    emit(".offset= offsetof(%s %s, %s),", comp_kind, name, tokn(curr->decl->name));
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
    case DECL_KIND_NOTAG:
    case DECL_KIND_STRUCT:
    case DECL_KIND_UNION:
    case DECL_KIND_ENUM:
        return true;
    case DECL_KIND_PTR:
    case DECL_KIND_FUN:
    case DECL_KIND_ARR:
        return false;
    }
    // unreachable
    return false;
}

void emit_extern(declaration cref decl)
{
    if (emit_decl) {
        if (*tokn(decl->name)) {
            emit("extern ");
            emit_forward(&decl->type, tokn(decl->name), false);
            emitln(";");
        } else switch (decl->type.kind) {
        case DECL_KIND_STRUCT:
        case DECL_KIND_UNION:
        case DECL_KIND_ENUM:
            emit_forward(&decl->type, NULL, false);
            emitln(";");
            // unreachable cases
        case DECL_KIND_NOTAG: case DECL_KIND_PTR: case DECL_KIND_FUN: case DECL_KIND_ARR:;
        }
    }
    emit_named_comps_adpt_type_def(&decl->type);

    if (DECL_KIND_FUN == decl->type.kind) {
        bool found = false;
        find_seen (decl->name, seen.funs) { found = true; break; }
        if (found) return;

        struct seen_fun ref fun = dyarr_push(&seen.funs);
        if (!fun) errdie("OOM");
        fun->name = decl->name;

        indented ("void %s_adapt_call(char* ret, char** args) {", tokn(decl->name)) {
            if (0 == decl->type.info.fun.count) emitln("(void)args;");
            if (!strcmp("void", tokn(decl->type.info.fun.ret->type.name))) emitln("(void)ret;");
            else {
                emit("*(");
                emit_forward(&(struct decl_type){
                        .kind= DECL_KIND_PTR,
                        .info.ptr= decl->type.info.fun.ret,
                    }, NULL, true);
                emit(")ret = ");
            }
            emit("%s(", tokn(decl->name));
            size_t k = 0;
            for_linked (decl->type.info,fun) {
                if (!curr->decl) errdie("Variadic functions are not supported");
                emit("*(");
                emit_forward(&(struct decl_type){
                        .kind= DECL_KIND_PTR,
                        .info.ptr= curr->decl,
                    }, NULL, true);
                emit(")args[%zu]", k++);
                if (curr->next) emit(", ");
            }
            emit(");");
        }
        emitln("}");
    }

    else if (*tokn(decl->name)) {
        bool found = false;
        find_seen (decl->name, seen.objs) { found = true; break; }
        if (found) return;

        struct seen_obj ref obj = dyarr_push(&seen.objs);
        if (!obj) errdie("OOM");
        obj->name = decl->name;

        emitln("// object not quite handled anywhere yet");
    }

    if (*tokn(decl->name)) {
        bool const use_def = adpt_type_val_needs_define(&decl->type);
        if (use_def) emit("#define %s_adapt_type ", tokn(decl->name));
        else emit("static struct adpt_type const %s_adapt_type = ", tokn(decl->name));
        emit_adpt_type_val(&decl->type, true, false);
        if (use_def) emit_empty();
        else emitln(";");
    }
}

void emit_typedef(declaration cref decl)
{
    if (emit_decl) {
        emit("typedef ");
        emit_forward(&decl->type, tokn(decl->name), false);
        emitln(";");
    }
    emit_named_comps_adpt_type_def(&decl->type);

    struct seen_tdf ref tdf = dyarr_push(&seen.tdfs);
    if (!tdf) errdie("OOM");
    tdf->name = decl->name;

    indented ("static struct adpt_type const %s_adapt_tdf_type = {", tokn(decl->name)) {
        emitln(".tyty= ADPT_TYPE_NAMED,");
        indented (".info.named= {") {
            emit(".def= &");
            emit_adpt_type_val(&decl->type, true, false);
            emitln(",");
            emit(".name= \"%s\",", tokn(decl->name));
        }
        emit("},");
    }
    emitln("};");

    emitln("#define %s_adapt_type %s_adapt_tdf_type", tokn(decl->name), tokn(decl->name));
}

bool parse_failure = false;
void emit_top(void* _, declaration cref decl, tokt ref tok)
{
    parse_failure = false;
    (void)_;
    (void)tok;

    // xxx: lexer internals,
    // but basically we need to know if we are in the top-level ("entry") file
    if (!follow_incl && 1 != ls->sources.len) {
        //notif("Skipping: '%s' (%s)", tokn(decl->name), bufmt(decl->type.name));
        goto next;
    }

    if (decl->is_inline) goto next;

    if (just_did_sys_include) {
        just_did_sys_include = false;
        emit_empty();
    }

    switch (decl->spec) {
        // unreachable cases
    case DECL_SPEC_AUTO:
    case DECL_SPEC_REGISTER:
        goto next;

    case DECL_SPEC_STATIC: // internal linkage (not visible)
        {
            bool found = false;
            find_seen (decl->name, seen.stas) { found = true; break; }
            if (found) goto next;

            struct seen_sta ref sta = dyarr_push(&seen.stas);
            if (!sta) errdie("OOM");
            sta->name = decl->name;
        }

        emit("static ");
        emit_forward(&decl->type, tokn(decl->name), false);
        if ('=' == *tokn(*tok)) {
            emit(" = ");
            // YYY: you could have a struct/enum/union declared in there
            *tok = lext(ls);
            if ('{' == *tokn(*tok)) {
                emit("{ ");
                for (unsigned depth = 0; (*tok = lext(ls)), *tokn(*tok); ) {
                    emit("%s ", tokn(*tok));
                    bool c = '}' == *tokn(*tok);
                    if (!*tokn(*tok) || (!depth && c)) break;
                    depth+= ('{' == *tokn(*tok))-c;
                }
                *tok = lext(ls);
            } else *tok = parse_expression(&(parse_expr_state){.ls= ls, .disallow_comma= true, .on= _emit_cexpr}, *tok);
        }
        emitln(";");
        emit_empty();
        goto next;

    case DECL_SPEC_NONE: // default at file scope is external linkage
        {
            bool found = false;
            find_seen (decl->name, seen.stas) { found = true; break; }
            if (found) goto next;
        }
        // fall through
    case DECL_SPEC_EXTERN: // external linkage
        emit_extern(decl);
        emit_empty();
        goto next;

    case DECL_SPEC_TYPEDEF:
        emit_typedef(decl);
        emit_empty();
        goto next;
    }

next:
    switch (*tokn(*tok)) {
    case '=':
        *tok = lext(ls);
        if ('{' == *tokn(*tok)) {

    case '{':
            for (unsigned depth = 0; (*tok = lext(ls)), *tokn(*tok); ) {
                bool c = '}' == *tokn(*tok);
                if (!*tokn(*tok) || (!depth && c)) break;
                depth+= ('{' == *tokn(*tok))-c;
            }
            *tok = lext(ls);

            if (!*tokn(*tok) || !strchr(",;", *tokn(*tok))) {
                ps->base = NULL; // reset
                return;
            }

        } else *tok = parse_expression(&(parse_expr_state){.ls= ls, .disallow_comma= true}, *tok);

        if (';' == *tokn(*tok)) {
    case ';':
            *tok = lext(ls);
            ps->base = NULL; // reset
            return;
        }
        // fall through
    case ',':
        *tok = parse_declaration(ps, lext(ls));
        return;

    case '\0':
        return;

    default:
        errdie("Unexpected token after declaration: '%s'", tokn(*tok));
    }
}

// random things {{{
void cleanup(void)
{
    lex_free(ls);
    if (result && stdout != result) fclose(result);

    free(seen.funs.ptr);
    free(seen.tags.ptr);
    free(seen.tdfs.ptr);
    free(seen.objs.ptr);
}

char* name_space(char ref name)
{
    char* ptr = name;
    size_t len = strlen(name);

    char* const basename = strrchr(ptr, '/');
    if (basename) ptr = basename+1;

    char const* fileext = strchr(ptr, '.');
    if (fileext) len = fileext - ptr;

    for (size_t k = 0; k < len; k++)
        if (!(!k ? isidstart(ptr[k]) : isidcont(ptr[k])))
            ((char*)ptr)[k] = '_';

    // adapter files are expected to start with "a-",
    // tho this is not strictly mandatory I suppose...
    // this will also catch false positives :/
    if ('a' == ptr[0] && '_' == ptr[1]) {
        ptr+= 2;
        len-= 2;
    }

    ptr[len] = '\0';
    return ptr;
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

        indented ("struct adpt_namespace const namespaces[] = {") for (char** it = first; it < past_end; it++) {
            char cref itns = name_space(*it);
            emit("{.name= \"%s\", .count= sizeof adptns_%s/sizeof*adptns_%s, .items= adptns_%s}", itns, itns, itns, itns);
            if (past_end == it+1) emit(",");
            else emitln(",");
        }
        emitln("};");
        emit_empty();

        emitln("struct adpt_namespace const* const namespaces_first = &namespaces[0];");
        emitln("unsigned long const namespaces_count = sizeof namespaces/sizeof*namespaces;");
    } else {
        emitln("struct adpt_namespace const* const namespaces_first = NULL;");
        emitln("unsigned long const namespaces_count = 0;");
    }

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
            lex_define(ls, arg+2, val);
            break;

        case 'I':
            lex_incdir(ls, arg+2);
            break;

        case 'P':
            val = arg+2;
            if (!*val) break;
#           define flagis(l) (!memcmp((l), val, strlen(l)))
            else if (flagis("no-emit-decl")) emit_decl = false, emit_incl = true;
            else if (flagis("no-emit-incl")) emit_incl = false;
            else if (flagis("no-emit-sysi")) emit_sysi = false;
            else if (flagis("do-follow-incl")) follow_incl = true;
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

    {
        FILE* const s = fopen(infile, "r");
        if (!s) exitf("Could not read entry file %s", infile);
        lex_entry(ls, s, infile);
    }

    for (tokt tok = lext(ls); *tokn(tok) && !parse_failure; parse_failure = true, tok = parse_declaration(ps, tok));
    if (parse_failure) exitf("Could not parse source");

    char cref thisns = name_space(outfile ? outfile : infile);
    indented ("static struct adpt_item const adptns_%s[] = {", thisns) {
        for (size_t k = 0; k < seen.funs.len; k++) emitln("{.name= \"%s\", .type= &%s_adapt_type, .kind= ADPT_ITEM_OBJECT, .as.function= %s_adapt_call},", tokn(seen.funs.ptr[k].name), tokn(seen.funs.ptr[k].name), tokn(seen.funs.ptr[k].name));
        for (size_t k = 0; k < seen.tags.len; k++) emitln("{.name= \"@%s\", .type= &%s_adapt_tag_type, .kind= ADPT_ITEM_TYPEDEF},", tokn(seen.tags.ptr[k].name), tokn(seen.tags.ptr[k].name));
        for (size_t k = 0; k < seen.tdfs.len; k++) emitln("{.name= \"%s\", .type= &%s_adapt_type, .kind= ADPT_ITEM_TYPEDEF},", tokn(seen.tdfs.ptr[k].name), tokn(seen.tdfs.ptr[k].name));
        for (size_t k = 0; k < seen.objs.len; k++) emitln("{.name= \"%s\", .type= &%s_adapt_type, .kind= ADPT_ITEM_OBJECT, .as.object= (void*)&%s},", tokn(seen.objs.ptr[k].name), tokn(seen.objs.ptr[k].name), tokn(seen.objs.ptr[k].name));
        for (size_t k = 0; k < seen.enus.len; k++) emitln("{.name= \"%s\", .type= &adptb_int_type, .kind= ADPT_ITEM_VALUE, .as.value= %s},", tokn(seen.enus.ptr[k].name), tokn(seen.enus.ptr[k].name));
        size_t const count = seen.funs.len+seen.tags.len+seen.tdfs.len+seen.objs.len+seen.enus.len;
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
                "  -Pdo-follow-incl  by default the `#include \"\"`\n"
                "                    directives are only preprocessed,\n"
                "                    none of there declarations are used;\n"
                "                    this makes it do declare these too\n"
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

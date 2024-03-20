#include "debug.h"
#include "array.h"
#include "aux.h"
#include "call.h"
#include "map.h"
#include "rt.h"

static const char *opcode_name(Op op)
{
    switch (op) {
        case OP_PUSHNULL:
            return "PUSHNULL";
        case OP_PUSHTRUE:
            return "PUSHTRUE";
        case OP_PUSHFALSE:
            return "PUSHFALSE";
        case OP_PUSHCONST:
            return "PUSHCONST";
        case OP_POP:
            return "POP";
        case OP_CLOSE:
            return "CLOSE";
        case OP_RETURN:
            return "RETURN";
        case OP_CLOSURE:
            return "CLOSURE";
        case OP_GETSUPER:
            return "GETSUPER";
        case OP_INVOKESUPER:
            return "INVOKESUPER";
        case OP_CALL:
            return "CALL";
        case OP_INHERIT:
            return "INHERIT";
        case OP_INVOKE:
            return "INVOKE";
        case OP_JUMP:
            return "JUMP";
        case OP_JUMPFALSE:
            return "JUMPFALSE";
        case OP_JUMPNULL:
            return "JUMPNULL";
        case OP_GLOBAL:
            return "GLOBAL";
        case OP_GETGLOBAL:
            return "GETGLOBAL";
        case OP_SETGLOBAL:
            return "SETGLOBAL";
        case OP_GETLOCAL:
            return "GETLOCAL";
        case OP_SETLOCAL:
            return "SETLOCAL";
        case OP_UPVALUE:
            return "UPVALUE";
        case OP_GETUPVALUE:
            return "GETUPVALUE";
        case OP_SETUPVALUE:
            return "SETUPVALUE";
        case OP_NEWCLASS:
            return "NEWCLASS";
        case OP_NEWMETHOD:
            return "NEWMETHOD";
        case OP_NEWARRAY:
            return "NEWARRAY";
        case OP_NEWMAP:
            return "NEWMAP";
        case OP_VARARG:
            return "VARARG";
        case OP_UNPACK:
            return "UNPACK";
        case OP_UNPACKEX:
            return "UNPACKEX";
        case OP_FORNUM0:
            return "FORNUM0";
        case OP_FORNUM:
            return "FORNUM";
        case OP_FORIN0:
            return "FORIN0";
        case OP_FORIN:
            return "FORIN";
        case OP_LEN:
            return "LEN";
        case OP_NEG:
            return "NEG";
        case OP_NOT:
            return "NOT";
        case OP_BNOT:
            return "BNOT";
        case OP_ADD:
            return "ADD";
        case OP_SUB:
            return "SUB";
        case OP_MUL:
            return "MUL";
        case OP_DIV:
            return "DIV";
        case OP_IDIV:
            return "IDIV";
        case OP_MOD:
            return "MOD";
        case OP_POW:
            return "POW";
        case OP_CONCAT:
            return "CONCAT";
        case OP_BXOR:
            return "BXOR";
        case OP_BAND:
            return "BAND";
        case OP_BOR:
            return "BOR";
        case OP_SHL:
            return "SHL";
        case OP_SHR:
            return "SHR";
        case OP_EQ:
            return "EQ";
        case OP_LT:
            return "LT";
        case OP_LE:
            return "LE";
        case OP_IN:
            return "IN";
        case OP_GETATTR:
            return "GETATTR";
        case OP_SETATTR:
            return "SETATTR";
        case OP_GETITEM:
            return "GETITEM";
        case OP_SETITEM:
            return "SETITEM";
        case OP_RADD:
            return "RADD";
        case OP_RSUB:
            return "RSUB";
        case OP_RMUL:
            return "RMUL";
        case OP_RDIV:
            return "RDIV";
        case OP_RIDIV:
            return "RIDIV";
        case OP_RMOD:
            return "RMOD";
        case OP_RPOW:
            return "RPOW";
        case OP_RCONCAT:
            return "RCONCAT";
        case OP_RBXOR:
            return "RBXOR";
        case OP_RBAND:
            return "RBAND";
        case OP_RBOR:
            return "RBOR";
        case OP_RSHL:
            return "RSHL";
        case OP_RSHR:
            return "RSHR";
        case OP_STR:
            return "STR";
        case OP_INT:
            return "INT";
        case OP_FLOAT:
            return "FLOAT";
        case OP_BOOL:
            return "BOOL";
        case OP_ARRAY:
            return "ARRAY";
        case OP_MAP:
            return "MAP";
        default:
            return "???";
    }
}

void dump_aux(paw_Env *P, Proto *proto, Buffer *print)
{
    const OpCode *pc = proto->source;
    const OpCode *end = pc + proto->length;

    pawL_add_string(P, print, "function '");
    pawL_add_nstring(P, print, proto->name->text, proto->name->length);
    pawL_add_fstring(P, print, "' (%I bytes)\n", (paw_Int)proto->length);
    pawL_add_fstring(P, print, "constant(s) = %I, upvalue(s) = %I\n", (paw_Int)proto->nk, (paw_Int)proto->nup);
    for (int i = 0; pc != end; ++i) {
        pawL_add_fstring(P, print, "%d  %I  %s", i, (paw_Int)(pc - proto->source), opcode_name(pc[0]));
        switch (*pc++) {
            case OP_PUSHCONST: {
                pawL_add_fstring(P, print, " ; id = %d", Iw());
                break;
            }

            case OP_NEWARRAY: {
                pawL_add_fstring(P, print, " ; %d elements", Iw());
                break;
            }

            case OP_NEWMAP: {
                pawL_add_fstring(P, print, " ; %d items", Iw());
                break;
            }

            case OP_FORNUM0: {
                pawL_add_fstring(P, print, " ; offset = %d", decode_jump(Iw()));
                break;
            }

            case OP_FORIN0: {
                pawL_add_fstring(P, print, " ; offset = %d", decode_jump(Iw()));
                break;
            }

            case OP_FORNUM: {
                pawL_add_fstring(P, print, " ; offset = %d", decode_jump(Iw()));
                break;
            }

            case OP_FORIN: {
                pawL_add_fstring(P, print, " ; offset = %d", decode_jump(Iw()));
                break;
            }

            case OP_GETLOCAL: {
                pawL_add_fstring(P, print, " ; id = %d", Iw());
                break;
            }

            case OP_SETLOCAL: {
                pawL_add_fstring(P, print, " ; id = %d", Iw());
                break;
            }

            case OP_GETUPVALUE: {
                pawL_add_fstring(P, print, "%d", Iw());
                break;
            }

            case OP_SETUPVALUE: {
                pawL_add_fstring(P, print, "%d", Iw());
                break;
            }

            case OP_GETGLOBAL: {
                const int iw = Iw();
                const Value v = proto->k[iw];
                const String *s = pawV_get_string(v);
                pawL_add_string(P, print, " ; id = ");
                pawL_add_nstring(P, print, s->text, s->length);
                break;
            }

            case OP_SETGLOBAL: {
                pawL_add_fstring(P, print, " ; id = %d", Iw());
                break;
            }

            case OP_GLOBAL: {
                pawL_add_fstring(P, print, " ; k = %d", Iw());
                break;
            }

            case OP_NEWCLASS: {
                pawL_add_fstring(P, print, " ; k = %d", Iw());
                break;
            }

            case OP_NEWMETHOD: {
                pawL_add_fstring(P, print, " ; k = %d", Iw());
                break;
            }

            case OP_CLOSURE: {
                const int idx = Iw();
                Proto *p = proto->p[idx];
                String *s = p->name;
                pawL_add_string(P, print, " ; '");
                if (s) {
                    pawL_add_nstring(P, print, s->text, s->length);
                } else {
                    pawL_add_string(P, print, "<anonymous fn>");
                }
                pawL_add_fstring(P, print, "', nup = %I", (paw_Int)p->nup);
                for (int i = 0; i < p->nup; ++i) {
                    paw_unused(Iw());
                }
                break;
            }

            case OP_INVOKE: {
                const int id = Iw();
                pawL_add_fstring(P, print, " ; id = %d, # params = %d", id, Ib());
                break;
            }

            case OP_CALL: {
                pawL_add_fstring(P, print, " ; # params = %d", Ib());
                break;
            }

            case OP_VARARG: {
                const int nfixed = Ib();
                const int npassed = paw_get_count(P) - 1;
                pawL_add_fstring(P, print, " ; # argv = %d", npassed - nfixed);
                break;
            }

            case OP_JUMP: {
                pawL_add_fstring(P, print, " ; offset = %d", decode_jump(Iw()));
                break;
            }

            case OP_JUMPFALSE: {
                pawL_add_fstring(P, print, " ; offset = %d", decode_jump(Iw()));
                break;
            }

            case OP_JUMPNULL: {
                pawL_add_fstring(P, print, " ; offset = %d", decode_jump(Iw()));
                break;
            }

                // TODO: Add constant operands and proto names as "; comment" after operator. (like luac)
        }
        pawL_add_char(P, print, '\n');
    }

    // Dump nested protos.
    for (int i = 0; i < proto->nproto; ++i) {
        pawL_add_char(P, print, '\n');
        dump_aux(P, proto->p[i], print);
    }
}

void paw_dump_source(paw_Env *P, Proto *proto)
{
    Buffer print;
    pawL_init_buffer(P, &print);
    dump_aux(P, proto, &print);
    pawL_add_char(P, &print, '\0');
    puts(print.data);
    pawL_discard_result(P, &print);
}

void paw_dump_stack(paw_Env *P)
{
    int i = 0;
    for (StackPtr p = P->stack; p != P->top; ++p) {
        printf("%d: ", i);
        switch (pawV_get_type(*p)) {
            case VNATIVE:
                puts("native");
                break;
            case VCLASS:
                printf("class %p (%zu attrs)\n", (void *)pawV_get_class(*p), pawH_length(pawV_get_class(*p)->attr));
                break;
            case VINSTANCE:
                printf("instance %p (%zu attrs)\n", (void *)pawV_get_instance(*p), pawH_length(pawV_get_instance(*p)->attr));
                break;
            case VMETHOD:
                puts("method");
                break;
            case VCLOSURE:
                puts("closure");
                break;
            case VARRAY:
                printf("array (%zu elems)\n", pawA_length(pawV_get_array(*p)));
                break;
            case VMAP:
                printf("map (%zu items)\n", pawV_get_map(*p)->length);
                break;
            case VSTRING:
                putchar('"');
                for (size_t i = 0; i < pawV_get_string(*p)->length; ++i) {
                    putchar(pawV_get_string(*p)->text[i]);
                }
                putchar('"');
                putchar('\n');
                break;
            case VTRUE:
                puts("true");
                break;
            case VFALSE:
                puts("false");
                break;
            case VBIGINT: {
                const ptrdiff_t save = pawC_stksave(P, p);
                Buffer print;
                pawL_init_buffer(P, &print);
                pawC_pushv(P, *p);
                pawL_add_value(P, &print);
                pawL_add_char(P, &print, '\0');
                printf("bigint %s\n", print.data);
                pawL_discard_result(P, &print);
                p = pawC_stkload(P, save);
                break;
            }
            case VNUMBER:
                printf("%lld\n", (long long)pawV_get_int(*p));
                break;
            case VNULL:
                puts("null");
                break;
            case VUSERDATA:
                printf("userdata %p (%zu attrs)\n", pawV_get_userdata(*p)->data, pawH_length(pawV_get_userdata(*p)->attr));
                break;
            case VPROTO:
                printf("proto k=%d\n", pawV_get_proto(*p)->nk);
                break;
            default:
                printf("%lf\n", pawV_get_float(*p));
        }
        ++i;
    }
}

// TODO: Copy of code in error.c. Maybe merge error.c into this TU
static int current_line(CallFrame *cf)
{
    Proto *p = cf->fn->p;
    const int pc = cf->pc - p->source;

    int i = 0;
    for (; i + 1 < p->nlines; ++i) {
        if (p->lines[i].pc >= pc) {
            break;
        }
    }
    return p->lines[i].line;
}

void paw_stacktrace(paw_Env *P)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);

    const String *modname = P->main.fn->p->name;

    int i = 0;
    CallFrame *cf = P->cf;
    while (!CF_IS_BASE(cf)) {
        pawL_add_fstring(P, &buf, "%d: File ", i);
        pawL_add_nstring(P, &buf, modname->text, modname->length);
        pawL_add_fstring(P, &buf, ", line %d, in ", current_line(cf));
        if (IS_PAW(cf)) {
            Proto *p = cf->fn->p;
            const String *name = p->name;
            pawL_add_nstring(P, &buf, name->text, name->length);
        } else {
            pawL_add_fstring(P, &buf, "<native>");
        }
        cf = cf->prev;
        ++i;
    }
    pawL_push_result(P, &buf);
}

void paw_dump_value(paw_Env *P, Value v)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    pawC_pushv(P, v);
    pawL_add_value(P, &buf);
    pawL_add_char(P, &buf, '\0');
    printf("%s\n", buf.data);
    pawL_discard_result(P, &buf);
}

void paw_dump_map(paw_Env *P, Map *m)
{
    Value v;
    pawV_set_map(&v, m);
    paw_dump_value(P, v);
}

#include "debug.h"
#include "array.h"
#include "auxlib.h"
#include "call.h"
#include "map.h"
#include "rt.h"

const char *paw_unop_name(UnaryOp unop)
{
    switch (unop) {
        case UNARY_LEN:
            return "LEN";
        case UNARY_NEG:
            return "NEG";
        case UNARY_NOT:
            return "NOT";
        case UNARY_BNOT:
            return "BNOT";
        default:
            return "?";
    }
}

const char *paw_binop_name(BinaryOp binop)
{
    switch (binop) {
        case BINARY_ADD:
            return "ADD";
        case BINARY_SUB:
            return "SUB";
        case BINARY_MUL:
            return "MUL";
        case BINARY_DIV:
            return "DIV";
        case BINARY_MOD:
            return "MOD";
        case BINARY_BXOR:
            return "BXOR";
        case BINARY_BAND:
            return "BAND";
        case BINARY_BOR:
            return "BOR";
        case BINARY_SHL:
            return "SHL";
        case BINARY_SHR:
            return "SHR";
        case BINARY_EQ:
            return "EQ";
        case BINARY_NE:
            return "NE";
        case BINARY_LT:
            return "LT";
        case BINARY_LE:
            return "LE";
        case BINARY_GT:
            return "GT";
        case BINARY_GE:
            return "GE";
        case BINARY_IN:
            return "IN";
        default:
            return "?";
    }
}

const char *paw_opcode_name(Op op)
{
    switch (op) {
        case OP_CASTBOOL:
            return "CASTBOOL";
        case OP_CASTINT:
            return "CASTINT";
        case OP_CASTFLOAT:
            return "CASTFLOAT";
        case OP_INCREF:
            return "INCREF";
        case OP_DECREF:
            return "DECREF";
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
        case OP_INVOKE:
            return "INVOKE";
        case OP_JUMP:
            return "JUMP";
        case OP_JUMPFALSE:
            return "JUMPFALSE";
        case OP_JUMPFALSEPOP:
            return "JUMPFALSEPOP";
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
        case OP_FORNUM0:
            return "FORNUM0";
        case OP_FORNUM:
            return "FORNUM";
        case OP_FORIN0:
            return "FORIN0";
        case OP_FORIN:
            return "FORIN";
        case OP_UNOP:
            return "UNOP";
        case OP_BINOP:
            return "BINOP";
        case OP_GETATTR:
            return "GETATTR";
        case OP_SETATTR:
            return "SETATTR";
        case OP_GETITEM:
            return "GETITEM";
        case OP_SETITEM:
            return "SETITEM";
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
        pawL_add_fstring(P, print, "%d  %I  %s", i, (paw_Int)(pc - proto->source), paw_opcode_name(get_OP(pc[0])));
        const OpCode opcode = *pc++;
        switch (get_OP(opcode)) {
            case OP_UNOP: {
                pawL_add_fstring(P, print, " ; type = %s", paw_unop_name(get_A(opcode)));
                break;
            }

            case OP_BINOP: {
                pawL_add_fstring(P, print, " ; type = %s", paw_binop_name(get_A(opcode)));
                break;
            }

            case OP_CLOSE: {
                pawL_add_fstring(P, print, " ; npop = %d, close = %d", get_A(opcode), get_B(opcode));
                break;
            }

            case OP_PUSHCONST: {
                pawL_add_fstring(P, print, " ; id = %d", get_U(opcode));
                break;
            }

            case OP_NEWARRAY: {
                pawL_add_fstring(P, print, " ; %d elements", get_U(opcode));
                break;
            }

            case OP_NEWMAP: {
                pawL_add_fstring(P, print, " ; %d items", get_U(opcode));
                break;
            }

            case OP_FORNUM0: {
                pawL_add_fstring(P, print, " ; offset = %d", get_S(opcode));
                break;
            }

            case OP_FORIN0: {
                pawL_add_fstring(P, print, " ; offset = %d", get_S(opcode));
                break;
            }

            case OP_FORNUM: {
                pawL_add_fstring(P, print, " ; offset = %d", get_S(opcode));
                break;
            }

            case OP_FORIN: {
                pawL_add_fstring(P, print, " ; offset = %d", get_S(opcode));
                break;
            }

            case OP_GETLOCAL: {
                pawL_add_fstring(P, print, " ; id = %d", get_U(opcode));
                break;
            }

            case OP_SETLOCAL: {
                pawL_add_fstring(P, print, " ; id = %d", get_U(opcode));
                break;
            }

            case OP_GETUPVALUE: {
                pawL_add_fstring(P, print, "%d", get_U(opcode));
                break;
            }

            case OP_SETUPVALUE: {
                pawL_add_fstring(P, print, "%d", get_U(opcode));
                break;
            }

            case OP_GETGLOBAL: {
                const int iw = get_U(opcode);
                pawL_add_string(P, print, " ; id = ");
                pawL_add_integer(P, print, iw);
                break;
            }

            case OP_SETGLOBAL: {
                pawL_add_fstring(P, print, " ; id = %d", get_U(opcode));
                break;
            }

            case OP_GLOBAL: {
                pawL_add_fstring(P, print, " ; k = %d", get_U(opcode));
                break;
            }

            case OP_NEWCLASS: {
                pawL_add_fstring(P, print, " ; k = %d, superclass? %d", get_A(opcode), get_B(opcode));
                break;
            }

            case OP_NEWMETHOD: {
                pawL_add_fstring(P, print, " ; k = %d", get_U(opcode));
                break;
            }

            case OP_CLOSURE: {
                const int idx = get_U(opcode);
                Proto *p = proto->p[idx];
                String *s = p->name;
                pawL_add_string(P, print, " ; '");
                if (s) {
                    pawL_add_nstring(P, print, s->text, s->length);
                } else {
                    pawL_add_string(P, print, "<anonymous fn>");
                }
                pawL_add_fstring(P, print, "', nup = %I", (paw_Int)p->nup);
                break;
            }

            case OP_INVOKE: {
                const int id = get_A(opcode);
                pawL_add_fstring(P, print, " ; id = %d, # params = %d", id, get_B(opcode));
                break;
            }

            case OP_CALL: {
                pawL_add_fstring(P, print, " ; # params = %d", get_U(opcode));
                break;
            }

            case OP_VARARG: {
                const int nfixed = get_U(opcode);
                const int npassed = paw_get_count(P) - 1;
                pawL_add_fstring(P, print, " ; # argv = %d", npassed - nfixed);
                break;
            }

            case OP_JUMP: {
                pawL_add_fstring(P, print, " ; offset = %d", get_S(opcode));
                break;
            }

            case OP_JUMPFALSE: {
                pawL_add_fstring(P, print, " ; offset = %d", get_S(opcode));
                break;
            }

            case OP_JUMPNULL: {
                pawL_add_fstring(P, print, " ; offset = %d", get_S(opcode));
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
    while (cf->prev) {
        pawL_add_fstring(P, &buf, "%d: File ", i);
        pawL_add_nstring(P, &buf, modname->text, modname->length);
        pawL_add_fstring(P, &buf, ", line %d, in ", current_line(cf));
        if (cf_is_paw(cf)) {
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

void paw_dump_value(paw_Env *P, Value v, int type)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    pawC_pushv(P, v);
    pawL_add_value(P, &buf, e_tag(P, type));
    pawL_add_char(P, &buf, '\0');
    printf("%s\n", buf.data);
    pawL_discard_result(P, &buf);
}

void paw_dump_map(paw_Env *P, Map *m)
{
    Value v;
    v_set_object(&v, m);
    paw_dump_value(P, v, PAW_TMAP);
}

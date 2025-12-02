#include <stdio.h>
#include <libguile.h>

SCM default_bindings() {
    SCM bindings = scm_c_public_ref("ice-9 sandbox", "all-pure-and-impure-bindings");
    bindings = scm_cons(
        scm_list_2(
            scm_list_2(scm_from_utf8_symbol("srfi"), scm_from_utf8_symbol("srfi-9")),
            scm_from_utf8_symbol("define-record-type")),
        bindings
    );
    bindings = scm_cons(
        scm_list_2(
            scm_list_2(scm_from_utf8_symbol("srfi"), scm_from_utf8_symbol("srfi-1")),
            scm_from_utf8_symbol("drop")),
        bindings
    );
    bindings = scm_cons(
        scm_list_2(
            scm_list_1(scm_from_utf8_symbol("guile")),
            scm_from_utf8_symbol("seed->random-state")),
        bindings
    );
    bindings = scm_cons(
        scm_list_2(
            scm_list_1(scm_from_utf8_symbol("guile")),
            scm_from_utf8_symbol("random")),
        bindings
    );
    bindings = scm_cons(
        scm_list_2(
            scm_list_1(scm_from_utf8_symbol("guile")),
            scm_from_utf8_symbol("eval")),
        bindings
    );
    bindings = scm_cons(
        scm_list_2(
            scm_list_1(scm_from_utf8_symbol("guile")),
            scm_from_utf8_symbol("interaction-environment")),
        bindings
    );
    return bindings;
}

SCM eval_sandbox(SCM call) {
    SCM eval = scm_c_public_ref("ice-9 sandbox", "eval-in-sandbox");
    return scm_call_7(eval, call,
        scm_from_utf8_keyword("time-limit"), scm_from_int64(3),
        scm_from_utf8_keyword("allocation-limit"), scm_from_int64(1024 * 1024 * 1024),
        scm_from_utf8_keyword("bindings"),
        default_bindings()
    );
}

typedef struct check_answer_args {
    char *code;
    char *actual;
    char *expected;
} check_answer_args;
SCM check_answer_catch_body(void *data) {
    check_answer_args *args = (check_answer_args *) data;
    // SCM handler = scm_c_eval_string(args->code);
    // SCM res = scm_call_1(handler, scm_from_utf8_string(args->data));
    SCM readport = scm_open_input_string(scm_from_utf8_string(args->code));
    SCM func = scm_read(readport);
    SCM call = scm_list_3(func, scm_from_utf8_string(args->actual), scm_from_utf8_string(args->expected));
    return eval_sandbox(call);
}
SCM check_answer_catch_handler(void *data, SCM key, SCM args) {
    SCM format = scm_c_public_ref("ice-9 format", "format");
    SCM fmt = scm_from_utf8_string("~a: ~a: ~a");
    SCM func = scm_car(args);
    SCM afmt = scm_cadr(args);
    SCM aargs = scm_caddr(args);
    SCM msg = scm_apply_2(format, SCM_BOOL_F, afmt, aargs);
    return scm_call_5(format, SCM_BOOL_F, fmt, key, func, msg);
}

int check_answer(char **failure, char *code, char *actual, char *expected) {
    scm_init_guile();
    check_answer_args args = { .code = code, .actual = actual, .expected = expected };
    SCM res = scm_c_catch(
        SCM_BOOL_T,
        check_answer_catch_body, &args,
        check_answer_catch_handler, NULL,
        check_answer_catch_handler, NULL);
    if (scm_is_bool(res)) {
        *failure = NULL;
        return scm_to_bool(res);
    } else {
        *failure = scm_to_utf8_stringn(res, NULL);
        return 0;
    }
}

typedef struct gen_input_answer_args {
    char *code;
    char *twitchid;
} gen_input_answer_args;
SCM gen_input_answer_catch_body(void *data) {
    gen_input_answer_args *args = (gen_input_answer_args *) data;
    SCM readport = scm_open_input_string(scm_from_utf8_string(args->code));
    SCM func = scm_read(readport);
    SCM call = scm_list_2(func, scm_from_utf8_string(args->twitchid));
    return eval_sandbox(call);
}
SCM gen_input_answer_catch_handler(void *data, SCM key, SCM args) {
    SCM format = scm_c_public_ref("ice-9 format", "format");
    SCM fmt = scm_from_utf8_string("~a: ~a: ~a");
    SCM func = scm_car(args);
    SCM afmt = scm_cadr(args);
    SCM aargs = scm_caddr(args);
    SCM msg = scm_apply_2(format, SCM_BOOL_F, afmt, aargs);
    return scm_call_5(format, SCM_BOOL_F, fmt, key, func, msg);
}

void gen_input_answer(char **failure, char **input, char **answer, char *code, char *twitchid) {
    scm_init_guile();
    gen_input_answer_args args = { .code = code, .twitchid = twitchid };
    SCM res = scm_c_catch(
        SCM_BOOL_T,
        gen_input_answer_catch_body, &args,
        gen_input_answer_catch_handler, NULL,
        gen_input_answer_catch_handler, NULL);
    if (scm_is_pair(res)) {
        *failure = NULL;
        *input = scm_to_utf8_stringn(scm_car(res), NULL);
        *answer = scm_to_utf8_stringn(scm_cdr(res), NULL);
    } else {
        *failure = scm_to_utf8_stringn(res, NULL);
    }
}

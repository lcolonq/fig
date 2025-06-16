#include <stdio.h>
#include <libguile.h>

typedef struct check_answer_args {
    char *code;
    char *data;
} check_answer_args;
SCM check_answer_catch_body(void *data) {
    check_answer_args *args = (check_answer_args *) data;
    // SCM handler = scm_c_eval_string(args->code);
    // SCM res = scm_call_1(handler, scm_from_utf8_string(args->data));
    SCM readport = scm_open_input_string(scm_from_utf8_string(args->code));
    SCM func = scm_read(readport);
    SCM call = scm_list_2(func, scm_from_utf8_string(args->data));
    SCM eval = scm_c_public_ref("ice-9 sandbox", "eval-in-sandbox");
    return scm_call_1(eval, call);
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

int check_answer(char **failure, char *code, char *data) {
    scm_init_guile();
    check_answer_args args = { .code = code, .data = data };
    SCM res = scm_c_catch(
        SCM_BOOL_T,
        check_answer_catch_body, &args,
        check_answer_catch_handler, NULL,
        check_answer_catch_handler, NULL);
    if (scm_is_integer(res)) {
        *failure = NULL;
        return scm_to_int(res);
    } else {
        *failure = scm_to_utf8_stringn(res, NULL);
        return 0;
    }
}

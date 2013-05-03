
#include <stdio.h>
#include <magic.h>

#include "erl_nif.h"

#define MAXBUFLEN  1024

static ERL_NIF_TERM 
from_file_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	char buf[MAXBUFLEN];

    ERL_NIF_TERM errorTerm = enif_make_atom(env, "error");
    
    if(enif_get_string(env, argv[0], buf, sizeof(buf), ERL_NIF_LATIN1) < 1) {
        return enif_make_tuple2(env, errorTerm, enif_make_string(env, "badarg", ERL_NIF_LATIN1));
    }

    

	const char *magic_full;
    magic_t magic_cookie;
    //MAGIC_MIME tells magic to return a mime of the file, but you can specify different things
    // magic_cookie = magic_open(MAGIC_MIME); 
    magic_cookie = magic_open(MAGIC_MIME_TYPE); 
    if (magic_cookie == NULL) {
        return enif_make_tuple2(env, errorTerm, enif_make_string(env, "magic_open_fail", ERL_NIF_LATIN1));
    }

     //Loading default magic database
    if (magic_load(magic_cookie, NULL) != 0) {
         // cannot load magic database
        magic_close(magic_cookie);
        return enif_make_tuple2(env, errorTerm, enif_make_string(env, "magic_load_database_fail", ERL_NIF_LATIN1));
    }

    //
    magic_full = magic_file(magic_cookie, buf);

    if(magic_full == NULL){
        return enif_make_tuple2(env, errorTerm, enif_make_string(env, magic_error(magic_cookie), ERL_NIF_LATIN1));
    }
    
    magic_close(magic_cookie);

    ERL_NIF_TERM okTerm = enif_make_atom(env, "ok");
    ERL_NIF_TERM value = enif_make_string(env, magic_full, ERL_NIF_LATIN1);   
    

    return  enif_make_tuple2(env, okTerm, value);
}




static ERL_NIF_TERM 
from_buffer_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary p;
    ERL_NIF_TERM errorTerm = enif_make_atom(env, "error");

    if (!enif_inspect_binary(env, argv[0], &p)) {
        return enif_make_tuple2(env, errorTerm, enif_make_string(env, "badarg", ERL_NIF_LATIN1));
    }

    const char *magic_full;
    magic_t magic_cookie;
    //MAGIC_MIME tells magic to return a mime of the file, but you can specify different things
    // magic_cookie = magic_open(MAGIC_MIME); 
    magic_cookie = magic_open(MAGIC_MIME_TYPE); 
    if (magic_cookie == NULL) {
        return enif_make_tuple2(env, errorTerm, enif_make_string(env, "magic_open_fail", ERL_NIF_LATIN1));
    }

     //Loading default magic database
    if (magic_load(magic_cookie, NULL) != 0) {
         // cannot load magic database 
        magic_close(magic_cookie);
        return enif_make_tuple2(env, errorTerm, enif_make_string(env, "magic_load_database_fail", ERL_NIF_LATIN1));
    }
    magic_full = magic_buffer(magic_cookie, p.data, p.size);
    magic_close(magic_cookie);

    if(!magic_full){
        return enif_make_tuple2(env, errorTerm, enif_make_string(env, magic_error(magic_cookie), ERL_NIF_LATIN1));
    }

    ERL_NIF_TERM okTerm = enif_make_atom(env, "ok");
    ERL_NIF_TERM value = enif_make_string(env, magic_full, ERL_NIF_LATIN1);   
    
    return  enif_make_tuple2(env, okTerm, value);
}



static ErlNifFunc nif_funcs[] = {
    {"from_file", 1, from_file_nif}
	,{"from_buffer", 1, from_buffer_nif}
};

ERL_NIF_INIT(magic_nif, nif_funcs, NULL, NULL, NULL, NULL)


#ifndef __BT_LIB_H__
#define __BT_LIB_H__

#include <bluetooth/bluetooth.h>
#include "erl_nif.h"

extern int bt_lib_load_atoms(ErlNifEnv* env);
extern int set_nonblock(int fd, int enable);
extern int get_hex_byte(char* ptr, char* ptr_end, int* value_ptr);
extern int get_bdaddr(ErlNifEnv* env, ERL_NIF_TERM arg, bdaddr_t* addr);
extern ERL_NIF_TERM make_bdaddr(ErlNifEnv* env, bdaddr_t* addr);
extern ERL_NIF_TERM make_bin_bdaddr(ErlNifEnv* env, bdaddr_t* addr);
extern int get_select_mask(ErlNifEnv* env, ERL_NIF_TERM arg, int* maskp);
extern ERL_NIF_TERM make_select_result(ErlNifEnv* env, int mask, int res);

#endif

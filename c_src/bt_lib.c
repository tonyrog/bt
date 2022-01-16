//
//  Bluetooth nif common functions
//

#include <fcntl.h>
#include <bluetooth/bluetooth.h>
#include "erl_nif.h"

#define DECL_ATOM(name) \
    static ERL_NIF_TERM atm_##name = 0

#define LOAD_ATOM(name)			\
    atm_##name = enif_make_atom(env,#name)

#define ATOM(name) atm_##name

DECL_ATOM(ok);
DECL_ATOM(error);
DECL_ATOM(read);
DECL_ATOM(write);
DECL_ATOM(cancel);
DECL_ATOM(stop);
DECL_ATOM(undefined);
DECL_ATOM(invalid);
DECL_ATOM(failed);
DECL_ATOM(cancelled);

int bt_lib_load_atoms(ErlNifEnv* env)
{
    LOAD_ATOM(ok);
    LOAD_ATOM(error);    
    LOAD_ATOM(read);
    LOAD_ATOM(write);
    LOAD_ATOM(cancel);
    LOAD_ATOM(stop);
    LOAD_ATOM(undefined);
    LOAD_ATOM(invalid);
    LOAD_ATOM(failed);
    LOAD_ATOM(cancelled);    
    return 0;
}

void bdaddr_swap(bdaddr_t* dst, const bdaddr_t* src)
{
    int i;
    for (i = 0; i < 6; i++)
	dst->b[5-i] = src->b[i];
}

// set socket in non blocking or blocking mode

int set_nonblock(int fd, int enable)
{
    int flags;
#if defined(O_NONBLOCK)
    if ((flags = fcntl(fd, F_GETFL, 0)) < 0)
	flags = 0;
    if (enable)
	flags = flags | O_NONBLOCK;
    else
	flags = flags & ~O_NONBLOCK;
    return fcntl(fd, F_SETFL, flags);
#else
    flags = enable;
    return ioctl(fd, FIOBIO, &flags);
#endif
}

// return 0 if error otherwise 1,2 or 3
// 1 -  ...x  (last one digit)
// 2 -  ..xy  (last two digits)
// 2 -  x:..  (middl one digit)
// 3 -  xy:..  (middl two digits)
int get_hex_byte(char* ptr, char* ptr_end, int* value_ptr)
{
    uint8_t x = 0;
    int c;

    if (ptr >= ptr_end)
	return 0;
    c = *ptr++;
    if ((c >= '0') && (c <= '9')) x = (c-'0');
    else if ((c >= 'a') && (c <= 'f')) x = 10+(c-'a');
    else if ((c >= 'A') && (c <= 'F')) x = 10+(c-'A');
    else return 0;
    if (ptr >= ptr_end) {
	*value_ptr = x;
	return 1;
    }
    c = *ptr++;
    if ((c >= '0') && (c <= '9')) x = (x<<4) + (c-'0');
    else if ((c >= 'a') && (c <= 'f')) x = (x<<4) + 10+(c-'a');
    else if ((c >= 'A') && (c <= 'F')) x = (x<<4) + 10+(c-'A');
    else if (c == ':') {
	*value_ptr = x;
	return 2;
    }
    else
	return 0;
    if (ptr >= ptr_end) {
	*value_ptr = x;
	return 2;
    }
    if (*ptr == ':') {
	*value_ptr = x;
	return 3;
    }
    return 0;
}

// parse either <<A,B,C,D,E,F>>
// or           {A,B,C,D,E,F}
// or           "AA:BB:CC:DD:EE:FF"
//
//                  MSB                       LSB
//                     NAP    UAP      LAP
//                   ======== ===   ============
// Bluetooth address AA : BB : CC : DD : EE : FF
//                             =================
//                                   SAP
//                   ============
//                       OUI
//
//  structure:
//             NAP: AA:BB
//             OUI: AA:BB:CC
//             UAP: CC
//             LAP: DD:EE:FF
//             SAP: CC:DD:EE:FF
//
// OUI = Organizationally Unique Identifier
// NAP = Non-significant Address Part
// UAP = Upper Address Part
// LAP = Lower Address Part
// SAP = Significant address part
//
// The address is stored reversed since bluetooth transmit LSB first
//
int get_bdaddr(ErlNifEnv* env, ERL_NIF_TERM arg, bdaddr_t* addr)
{
    bdaddr_t laddr;
    ErlNifBinary bin;
    const ERL_NIF_TERM* taddr;
    int i, arity, offs, b;

    if (enif_inspect_iolist_as_binary(env,arg,&bin)) {
	if (bin.size == 6) {
	    memcpy(laddr.b, bin.data, bin.size);
	}
	else { // check for ascii encoded address
	    char* ptr = (char*) bin.data;
	    char* ptr_end = (char*) bin.data + bin.size;
	    for (i = 0; i < 6; i++) {
		if ((offs = get_hex_byte(ptr, ptr_end, &b)) == 0)
		    return 0;
		laddr.b[i] = b;
		ptr += offs;
	    }
	    if (ptr != ptr_end)
		return 0;
	}
    }
    else if (enif_get_tuple(env, arg, &arity, &taddr) && (arity == 6)) {
	for (i = 0; i < 6; i++) {
	    if (!enif_get_int(env, taddr[i], &b)) return 0;
	    laddr.b[i] = b;
	}
    }
    else
	return 0;
    bdaddr_swap(addr, &laddr);
    return 1;
}

//
// Create a bluetooth address binary <<A,B,C,D,E,F>>
//
ERL_NIF_TERM make_bin_bdaddr(ErlNifEnv* env, bdaddr_t* addr)
{
    ERL_NIF_TERM bdaddr;    
    bdaddr_t laddr;
    
    bdaddr_swap(&laddr, addr);
    memcpy(enif_make_new_binary(env, 6, &bdaddr), laddr.b, 6); 
    return bdaddr;    
}
    
//
// Create a bluetooth tuple address {A,B,C,D,E,F}
//
ERL_NIF_TERM make_bdaddr(ErlNifEnv* env, bdaddr_t* addr)
{
    bdaddr_t laddr;

    bdaddr_swap(&laddr, addr);    
    return enif_make_tuple6(env,
			    enif_make_int(env, laddr.b[0]),
			    enif_make_int(env, laddr.b[1]),
			    enif_make_int(env, laddr.b[2]),
			    enif_make_int(env, laddr.b[3]),
			    enif_make_int(env, laddr.b[4]),
			    enif_make_int(env, laddr.b[5]));
}

int get_select_mask(ErlNifEnv* env, ERL_NIF_TERM arg, int* maskp)
{
    int mask = 0;

    if (enif_is_list(env, arg)) {
	ERL_NIF_TERM list = arg;
	ERL_NIF_TERM head, tail;
	while(enif_get_list_cell(env, list, &head, &tail)) {
	    if (head == ATOM(read))
		mask |= ERL_NIF_SELECT_READ;
	    else if (head == ATOM(write))
		mask = ERL_NIF_SELECT_WRITE;
	    else if (head == ATOM(cancel))
		mask = ERL_NIF_SELECT_CANCEL;
	    else
		return 0;
	    list = tail;
	}
	if ((mask == 0) || !enif_is_empty_list(env, list))
	    return 0;
    }
    else if (arg == ATOM(read))
	mask = ERL_NIF_SELECT_READ;
    else if (arg == ATOM(write))
	mask = ERL_NIF_SELECT_WRITE;
    else
	return 0;
    *maskp = mask;
    return 1;
}

ERL_NIF_TERM make_select_result(ErlNifEnv* env, int mask, int res)
{
    if (res < 0) {
	if (res & ERL_NIF_SELECT_INVALID_EVENT)
	    return enif_make_tuple2(env, ATOM(error), ATOM(invalid));
	if (res & ERL_NIF_SELECT_FAILED)
	    return enif_make_tuple2(env, ATOM(error), ATOM(failed));
	return enif_make_tuple2(env, ATOM(error), ATOM(undefined));       
    }
    else if (res > 0) {
	if (res & ERL_NIF_SELECT_STOP_CALLED)  // shold not happen (from user)
	    return enif_make_list1(env, ATOM(stop));
	if (res & ERL_NIF_SELECT_STOP_SCHEDULED) // may happen?
	    return enif_make_list1(env, ATOM(stop));
	if (res & ERL_NIF_SELECT_READ_CANCELLED) { // return when cancel+read
	    if ((mask & ERL_NIF_SELECT_READ) &&
		(mask & ERL_NIF_SELECT_CANCEL))
		return enif_make_tuple2(env, ATOM(ok), ATOM(cancelled));
	    return enif_make_tuple2(env, ATOM(error), ATOM(read));
	}
	if (res & ERL_NIF_SELECT_WRITE_CANCELLED) {
	    if ((mask & ERL_NIF_SELECT_WRITE) &&
		(mask & ERL_NIF_SELECT_CANCEL))
		return enif_make_tuple2(env, ATOM(ok), ATOM(cancelled));
	    // bad mask?
	    return enif_make_tuple2(env, ATOM(error), ATOM(write));
	}
	if (res & ERL_NIF_SELECT_ERROR_CANCELLED) {
	    if ((mask & ERL_NIF_SELECT_ERROR) &&
		(mask & ERL_NIF_SELECT_CANCEL))
		return enif_make_tuple2(env, ATOM(ok), ATOM(cancelled));
	    // bad mask?
	    return enif_make_tuple2(env, ATOM(error), ATOM(error));
	}
    }
    // == 0 ot res == 0
    return ATOM(ok);
}

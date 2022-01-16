//
// HCI
//
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/uio.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <asm/types.h>
#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>

#include "erl_nif.h"
#include "erl_driver.h"
#include "bt_lib.h"

//#define DEBUG
//#define NIF_TRACE

#define UNUSED(a) ((void) a)

#define MAX_HCI_BUF 4096
#define MAX_CONN 10

#ifdef DEBUG
#define DEBUGF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#else
#define DEBUGF(f,a...)
#endif
#define ERRORF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#define BADARG(env) enif_fprintf(stderr, "%s: badarg line=%d\r\n", __FILE__, __LINE__), enif_make_badarg((env))

#define UNUSED(a) ((void) a)

#define ATOM(name) atm_##name

#define DECL_ATOM(name) \
    ERL_NIF_TERM atm_##name = 0

#define LOAD_ATOM(name)			\
    atm_##name = enif_make_atom(env,#name)

#define LOAD_ATOM_STRING(name,string)		\
    atm_##name = enif_make_atom(env,string)


static int load(ErlNifEnv* env, void** priv_data,
		       ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data,
			  void** old_priv_data,
		       ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

// Dirty optional since 2.7 and mandatory since 2.12
#if (ERL_NIF_MAJOR_VERSION > 2) || ((ERL_NIF_MAJOR_VERSION == 2) && (ERL_NIF_MINOR_VERSION >= 7))
#ifdef USE_DIRTY_SCHEDULER
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(0)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#endif
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#endif

#define NIF_LIST					\
    NIF("open", 0, nif_open)				\
    NIF("bind", 2, nif_bind)				\
    NIF("close", 1, nif_close)				\
    NIF("dev_up", 1, nif_dev_up)			\
    NIF("dev_up", 2, nif_dev_up)			\
    NIF("dev_down", 2, nif_dev_down)			\
    NIF("dev_reset", 2, nif_dev_reset)			\
    NIF("dev_restat", 2, nif_dev_restat)		\
    NIF("get_dev_list", 1, nif_get_dev_list)		\
    NIF("get_dev_info", 1, nif_get_dev_info)		\
    NIF("get_dev_info", 2, nif_get_dev_info)		\
    NIF("get_conn_list", 1, nif_get_conn_list)		\
    NIF("get_conn_list", 2, nif_get_conn_list)		\
    NIF("get_conn_info", 3, nif_get_conn_info)		\
    NIF("get_auth_info", 2, nif_get_auth_info)		\
    NIF("set_raw", 1, nif_set_raw)			\
    NIF("set_raw", 2, nif_set_raw)			\
    NIF("set_auth", 2, nif_set_auth)			\
    NIF("set_auth", 3, nif_set_auth)			\
    NIF("set_encrypt", 2, nif_set_encrypt)		\
    NIF("set_encrypt", 3, nif_set_encrypt)		\
    NIF("set_ptype", 2, nif_set_ptype)			\
    NIF("set_ptype", 3, nif_set_ptype)			\
    NIF("set_link_policy", 2, nif_set_link_policy)	\
    NIF("set_link_policy", 3, nif_set_link_policy)	\
    NIF("set_link_mode", 2, nif_set_link_mode)		\
    NIF("set_link_mode", 3, nif_set_link_mode)		\
    NIF("set_scan", 2, nif_set_scan)			\
    NIF("set_scan", 3, nif_set_scan)			\
    NIF("set_acl_mtu", 3, nif_set_acl_mtu)		\
    NIF("set_acl_mtu", 4, nif_set_acl_mtu)		\
    NIF("set_sco_mtu", 3, nif_set_sco_mtu)		\
    NIF("set_sco_mtu", 4, nif_set_sco_mtu)		\
    NIF("block", 2, nif_block)				\
    NIF("unblock", 2, nif_unblock)			\
    NIF("inquiry", 5, nif_inquiry)			\
    NIF("inquiry", 6, nif_inquiry)			\
    NIF("set_filter", 2, nif_set_filter)		\
    NIF("get_filter", 1, nif_get_filter)		\
    NIF("debug", 2, nif_debug)				\
    NIF("write", 2, nif_write)				\
    NIF("read", 1,  nif_read)				\
    NIF("select", 2,  nif_select)
    
static ErlNifResourceType* hci_r;

typedef enum {
    UNDEFINED   = 0x00,
    OPEN        = 0x01,
    CLOSING     = 0x02,
    LISTEN      = 0x04,
    BOUND       = 0x08,
    SCANNING    = 0x10
} hstate_t;


typedef struct _handle_t {
    ErlNifMutex*  access_mtx;
    int           access_count;
    hstate_t      state;
    int           fd;
    int           dev_id;
    char*         buf;
    size_t        buf_len;
} handle_t;

DECL_ATOM(ok);
DECL_ATOM(error);
DECL_ATOM(undefined);
DECL_ATOM(select);
DECL_ATOM(read);
DECL_ATOM(write);
DECL_ATOM(no_such_handle);

DECL_ATOM(enabled);
DECL_ATOM(disabled);
DECL_ATOM(p2p);
DECL_ATOM(hci_filter);
DECL_ATOM(hci_dev_info);
DECL_ATOM(hci_dev_stats);
DECL_ATOM(hci_conn_info);
DECL_ATOM(inquiry_info);

// Declare all nif functions
#undef NIF
#ifdef NIF_TRACE
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]); \
    static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#else
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#endif

NIF_LIST

#undef NIF
#ifdef NIF_TRACE
#define NIF(name,arity,func) NIF_FUNC(name, arity, trace##_##func##_##arity),
#else
#define NIF(name,arity,func) NIF_FUNC(name, arity, func),
#endif

static ErlNifFunc nif_funcs[] =
{
    NIF_LIST
};

static void* realloc_buffer(handle_t* hp, size_t len)
{
    if (hp->buf_len < len) {
	hp->buf = enif_realloc(hp->buf, len);
	hp->buf_len = len;
    }
    return hp->buf;
}


static void dtor(ErlNifEnv* env, handle_t* hp)
{
    UNUSED(env);

    DEBUGF("dtor: hp=%p fd=%d, state=0x%x", hp, hp->fd, hp->state);
    if (hp->fd >= 0) {
	close(hp->fd);
	hp->fd = -1;
	hp->state = UNDEFINED;
    }
    enif_free(hp->buf);
}

static void stop(ErlNifEnv* env, handle_t* hp,
		 ErlNifEvent event, int is_direct_call)
{
    UNUSED(env);
    UNUSED(is_direct_call);
    DEBUGF("stop: hp=%p, fd=%d", hp, (intptr_t)event);
    if ((hp->fd >= 0) && (hp->fd == (intptr_t)event)) {
	close(hp->fd);
	hp->fd = -1;
	hp->state = UNDEFINED;
    }
}

static void down(ErlNifEnv* env, handle_t* hp,
		 const ErlNifPid* pid, const ErlNifMonitor* mon)
{
    UNUSED(env);
    UNUSED(pid);
    UNUSED(mon);
    UNUSED(hp);
    DEBUGF("down: hp=%p, fd=%d", hp, (intptr_t)hp->fd);
}

static int get_handle(ErlNifEnv* env, ERL_NIF_TERM arg,handle_t** handle_ptr,
		      hstate_t some, hstate_t none)
{
    handle_t* hp;
    int r;

    if (!enif_is_ref(env, arg))
	return -1;  // badarg
    if (!enif_get_resource(env, arg, hci_r, (void **)&hp))
	return 0;   // no a valid reource handle / remove / closed
    DEBUGF("get_handle: hp=%p, fd=%d, state=0x%x, some=%x, none=%x",
	   hp, hp->fd, hp->state, some, none);
    if (some && ((hp->state & some) == 0))
	return -1;
    if (none && ((~hp->state & none) == 0))
	return -1;
    enif_mutex_lock(hp->access_mtx);
    if ((r = ((hp->state & OPEN) != 0)))
	hp->access_count++;
    enif_mutex_unlock(hp->access_mtx);
    *handle_ptr = hp;
    return r;
}

static void done_handle(ErlNifEnv* env, handle_t* hp)
{
    int must_close = 0;
    DEBUGF("done_handle: access_count = %d", hp->access_count);    
    enif_mutex_lock(hp->access_mtx);
    hp->access_count--;
    if ((hp->access_count == 0) && (hp->state & CLOSING))
	must_close = 1;
    enif_mutex_unlock(hp->access_mtx);
    if (must_close) {
	DEBUGF("done_handle: close", 0);
	if ((hp->state == SCANNING)) {
	    hp->state = CLOSING;
	    enif_select(env, (ErlNifEvent)hp->fd,
			ERL_NIF_SELECT_STOP,
			hp, NULL, ATOM(undefined));
	}
	else if (hp->fd >= 0) {
	    ERRORF("bt_hci: force closing %d", hp->fd); // for real?
	    close(hp->fd);
	    hp->state = UNDEFINED;
	    hp->fd = -1;
	}
    }
}


static ERL_NIF_TERM make_error(ErlNifEnv* env, int err)
{
    DEBUGF("make_error: %d", err);
    return enif_make_tuple2(env,
			    ATOM(error),
			    enif_make_atom(env, erl_errno_id(err)));
}

// Value must be return from NIF
static ERL_NIF_TERM make_herror(ErlNifEnv* env, handle_t* handle, int err)
{
    DEBUGF("make_herror: %d", err);
    done_handle(env, handle);
    return make_error(env, err);
}

// Value must be return from NIF
static ERL_NIF_TERM make_hbadarg(ErlNifEnv* env, handle_t* handle)
{
    DEBUGF("make_herror: %d", err);
    done_handle(env, handle);
    return enif_make_badarg(env);
}

// Value must be return from NIF
static ERL_NIF_TERM make_bad_handle(ErlNifEnv* env) {
    DEBUGF("bad_handle%s", "");
    return enif_make_tuple2(env, ATOM(error), ATOM(no_such_handle));
}

// Value must be return from NIF
static ERL_NIF_TERM make_result(ErlNifEnv* env, handle_t* handle, ERL_NIF_TERM value) {
    done_handle(env, handle);
    return enif_make_tuple2(env, ATOM(ok), value);
}

// Value must be return from NIF
static ERL_NIF_TERM make_ok(ErlNifEnv* env, handle_t* handle)
{
    UNUSED(env);
    done_handle(env, handle);
    return ATOM(ok);
}

// match hci_drv.hrl record(hci_filter) !
static ERL_NIF_TERM make_hci_filter(ErlNifEnv* env, struct hci_filter* fp)
{
    uint64_t event_mask =
	(((uint64_t)fp->event_mask[0]) << 32) | fp->event_mask[1];
    return enif_make_tuple4(env,
			    ATOM(hci_filter),
			    enif_make_uint(env, fp->type_mask),
			    enif_make_uint64(env, event_mask),
			    enif_make_uint(env, fp->opcode));
}

static int get_hci_filter(ErlNifEnv* env, ERL_NIF_TERM arg,
			  struct hci_filter* fp)
{

    uint64_t event_mask;
    uint32_t value;
    const ERL_NIF_TERM* fields;
    int arity;
    
    if (!enif_get_tuple(env, arg, &arity, &fields) || (arity != 4))
	return 0;
    if (fields[0] != ATOM(hci_filter))
	return 0;
    if (!enif_get_uint(env, fields[1], &value))
	return 0;
    fp->type_mask = value;
    if (!enif_get_uint64(env, fields[2], &event_mask))
	return 0;
    fp->event_mask[0] = event_mask >> 32;
    fp->event_mask[1] = event_mask;
    if (!enif_get_uint(env, fields[3], &value))
	return 0;
    fp->opcode = value;
    return 1;
}


static ERL_NIF_TERM  make_dev_stats(ErlNifEnv* env, struct hci_dev_stats* sp)
{
    return enif_make_tuple(env, 11,
			    ATOM(hci_dev_stats),
			    enif_make_uint(env,sp->err_rx),
			    enif_make_uint(env,sp->err_tx),
			    enif_make_uint(env,sp->cmd_tx),
			    enif_make_uint(env,sp->evt_rx),
			    enif_make_uint(env,sp->acl_tx),
			    enif_make_uint(env,sp->acl_rx),
			    enif_make_uint(env,sp->sco_tx),
			    enif_make_uint(env,sp->sco_rx),
			    enif_make_uint(env,sp->byte_tx),
			    enif_make_uint(env,sp->byte_rx));
};

static ERL_NIF_TERM make_dev_info(ErlNifEnv* env, struct hci_dev_info* di)
{
    char name[9];
    ERL_NIF_TERM features;
    memcpy(name, di->name, sizeof(name));
    name[8] = '\0';

    memcpy(enif_make_new_binary(env, 8, &features), di->features, 8);
    return enif_make_tuple(env, 15,
			   ATOM(hci_dev_info),
			   enif_make_uint(env, di->dev_id),
			   enif_make_string(env, name, ERL_NIF_LATIN1),
			   make_bdaddr(env, &di->bdaddr),
			   enif_make_uint(env, di->flags),
			   enif_make_uint(env, di->type),
			   features,
			   enif_make_uint(env, di->pkt_type),
			   enif_make_uint(env, di->link_policy),
			   enif_make_uint(env, di->link_mode),
			   enif_make_uint(env, di->acl_mtu),
			   enif_make_uint(env, di->acl_pkts),
			   enif_make_uint(env, di->sco_mtu),
			   enif_make_uint(env, di->sco_pkts),
			   make_dev_stats(env, &di->stat));
}

static ERL_NIF_TERM make_conn_info(ErlNifEnv* env, struct hci_conn_info* ci)
{
    return enif_make_tuple7(env,
			    ATOM(hci_conn_info),
			    enif_make_uint(env, ci->handle),
			    make_bdaddr(env, &ci->bdaddr),
			    enif_make_uint(env, ci->type),
			    enif_make_uint(env, ci->out),
			    enif_make_uint(env, ci->state),
			    enif_make_uint(env, ci->link_mode));
}

static ERL_NIF_TERM make_inquiry_info(ErlNifEnv* env, inquiry_info* ip)
{
    ERL_NIF_TERM dev_class;

    memcpy(enif_make_new_binary(env, 3, &dev_class), ip->dev_class, 3);
    return enif_make_tuple(env, 7,
			   ATOM(inquiry_info),
			   make_bdaddr(env, &ip->bdaddr),
			   enif_make_uint(env, ip->pscan_rep_mode),
			   enif_make_uint(env, ip->pscan_period_mode),
			   enif_make_uint(env, ip->pscan_mode),
			   dev_class,
			   enif_make_uint(env, ip->clock_offset));
}


// get_dev_id
//
static int get_dev_id(ErlNifEnv* env, int nargs,
		      int argc, const ERL_NIF_TERM argv[],
		      handle_t* hp, int* dev_id_ptr)
{
    if (argc >= nargs) {
	if (!enif_get_int(env, argv[1], dev_id_ptr))
	    return 0;
	return 2;
    }
    else if (hp->state & BOUND) {
	*dev_id_ptr = hp->dev_id;
	return 1;
    }
    return 0;
}


static ERL_NIF_TERM nif_open(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    handle_t* hp;
    int fd;
    ERL_NIF_TERM ht;
    
    if ((fd = socket(PF_BLUETOOTH,
		     SOCK_RAW | SOCK_CLOEXEC,
		     BTPROTO_HCI))  < 0)
	return make_error(env, errno);

    if ((hp = enif_alloc_resource(hci_r, sizeof(handle_t))) == NULL) {
	int err = errno;
	close(fd);
	return make_error(env, err);
    }
    DEBUGF("open_ hp = %p", hp);
    if (set_nonblock(fd, 1) < 0) {
	int err = errno;
	close(fd);
	return make_error(env, err);
    }
    memset(hp, 0, sizeof(handle_t));
    hp->access_mtx = enif_mutex_create("hci_access");
    hp->fd = fd;
    hp->state = OPEN;
    ht = enif_make_resource(env, hp);
    enif_release_resource(hp);
    return enif_make_tuple2(env, ATOM(ok), ht);
}

// close(Handle::ref()) -> ok
static ERL_NIF_TERM nif_close(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    handle_t* hp;

    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }

    DEBUGF("nif_close: hp=%p, fd=%d, state=0x%x", hp, hp->fd, hp->state);

    enif_mutex_lock(hp->access_mtx);
    if (hp->fd >= 0) {
	hp->state |= CLOSING;
	enif_select(env, (ErlNifEvent)((intptr_t)(hp->fd)),
		    ERL_NIF_SELECT_STOP,
		    hp, NULL, ATOM(undefined));
    }
    enif_mutex_unlock(hp->access_mtx);
    done_handle(env, hp);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_bind(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    handle_t* hp;
    struct sockaddr_hci a;
    int dev_id;

    if (!enif_get_int(env, argv[1], &dev_id))
	enif_make_badarg(env);
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    memset(&a, 0, sizeof(a));
    a.hci_family = AF_BLUETOOTH;
    a.hci_dev = dev_id;
    if (bind(hp->fd, (struct sockaddr *) &a, sizeof(a)) < 0)
	return make_herror(env, hp, errno);
    hp->state |= BOUND;  // set under lock?
    hp->dev_id = dev_id;
    return make_ok(env, hp);    
}


static ERL_NIF_TERM nif_get_filter(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    handle_t* hp;
    struct hci_filter filt;
    socklen_t flen;
    ERL_NIF_TERM r;
    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    flen = sizeof(filt);
    if (getsockopt(hp->fd, SOL_HCI, HCI_FILTER, &filt, &flen) < 0)
	return make_herror(env, hp, errno);
    
    r = make_hci_filter(env, &filt);
    return make_result(env, hp, r);
}

static ERL_NIF_TERM nif_set_filter(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    handle_t* hp;
    struct hci_filter filt;
    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!get_hci_filter(env, argv[1], &filt))
	return make_herror(env, hp, errno);
    if (setsockopt(hp->fd, SOL_HCI, HCI_FILTER, &filt,sizeof(filt)) < 0)
	return make_herror(env, hp, errno);
    return make_ok(env, hp);
}

// dev_up(Hci) | dev_up(Hci, DevId)
static ERL_NIF_TERM nif_dev_up(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    handle_t* hp;
    int dev_id;

    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!get_dev_id(env, 2, argc, argv, hp, &dev_id))
	return make_hbadarg(env, hp);
    if (ioctl(hp->fd, HCIDEVUP, dev_id) < 0)
	return make_herror(env, hp, errno);
    return make_ok(env, hp);
}

// dev_down(Hci) | dev_down(Hci, DevId)
static ERL_NIF_TERM nif_dev_down(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    handle_t* hp;
    int dev_id;

    if (!enif_get_int(env, argv[1], &dev_id))
	enif_make_badarg(env);    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!get_dev_id(env, 2, argc, argv, hp, &dev_id))
	return make_hbadarg(env, hp);
    if (ioctl(hp->fd, HCIDEVDOWN, dev_id) < 0)
	return make_herror(env, hp, errno);
    return make_ok(env, hp);    
}

// dev_reset(Hci) | dev_reset(Hci, DevId)
static ERL_NIF_TERM nif_dev_reset(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    handle_t* hp;
    int dev_id;

    if (!enif_get_int(env, argv[1], &dev_id))
	enif_make_badarg(env);    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!get_dev_id(env, 2, argc, argv, hp, &dev_id))
	return make_hbadarg(env, hp);
    if (ioctl(hp->fd, HCIDEVRESET, dev_id) < 0)
	return make_herror(env, hp, errno);
    return make_ok(env, hp);    
}

// dev_restat(Hci) | dev_restat(Hci, DevId)
static ERL_NIF_TERM nif_dev_restat(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    handle_t* hp;
    int dev_id;

    if (!enif_get_int(env, argv[1], &dev_id))
	enif_make_badarg(env);    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!get_dev_id(env, 2, argc, argv, hp, &dev_id))
	return make_hbadarg(env, hp);
    if (ioctl(hp->fd, HCIDEVRESTAT, dev_id) < 0)
	return make_herror(env, hp, errno);
    return make_ok(env, hp);
}

static ERL_NIF_TERM nif_get_dev_list(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    handle_t* hp;
    struct hci_dev_list_req *dl;
    size_t sz = HCI_MAX_DEV*sizeof(struct hci_dev_req) +
	sizeof(struct hci_dev_list_req);
    int i;
    ERL_NIF_TERM list;

    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }

    if ((dl = realloc_buffer(hp, sz)) == NULL)
	return make_herror(env, hp, errno);
    memset(dl, 0, sz);
    dl->dev_num = HCI_MAX_DEV;

    if (ioctl(hp->fd, HCIGETDEVLIST, (void *) dl) < 0)
	return make_herror(env, hp, errno);	

    list = enif_make_list(env, 0);
    for (i = dl->dev_num-1; i >= 0; i--) {
	ERL_NIF_TERM h =
	    enif_make_tuple2(env,
			     enif_make_uint(env, dl->dev_req[i].dev_id),
			     enif_make_uint(env, dl->dev_req[i].dev_opt));
	list = enif_make_list_cell(env, h, list);
    }
    return make_result(env, hp, list);
}

// get_dev_info(Hci) -> {ok,#hci_dev_info{}} |
// get_dev_info(Hci, DevId) -> {ok,#hci_dev_info{}}
static ERL_NIF_TERM nif_get_dev_info(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    handle_t* hp;
    struct hci_dev_info di;
    int dev_id;
    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }

    if (!get_dev_id(env, 2, argc, argv, hp, &dev_id))
	return make_hbadarg(env, hp);
    di.dev_id = dev_id;
    if (ioctl(hp->fd, HCIGETDEVINFO, &di) < 0)
	return make_herror(env, hp, errno);
    return make_result(env, hp, make_dev_info(env, &di));
}

// get_conn_list(Hci) -> {ok,[#hci_conn_info{}]} |
// get_conn_list(Hci, DevId) -> {ok,[#hci_conn_info{}]}
static ERL_NIF_TERM nif_get_conn_list(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{    
    handle_t* hp;
    struct hci_conn_list_req *cl;
    size_t sz = sizeof(struct hci_conn_list_req) +
	MAX_CONN*sizeof(struct hci_conn_info);
    ERL_NIF_TERM list;    
    int dev_id;
    int i;

    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!get_dev_id(env, 2, argc, argv, hp, &dev_id))
	return make_hbadarg(env, hp);

    if ((cl = realloc_buffer(hp, sz)) == NULL)
	return make_herror(env, hp, errno);
    memset(cl, 0, sz);
    cl->dev_id = dev_id;
    cl->conn_num = MAX_CONN; // fixme: dynamic if ioctl fail...

    if (ioctl(hp->fd, HCIGETCONNLIST, (void *)cl) < 0)
	return make_herror(env, hp, errno);

    list = enif_make_list(env, 0);
    for (i = cl->conn_num-1; i >= 0; i--) {
	ERL_NIF_TERM h = make_conn_info(env, &cl->conn_info[i]);
	list = enif_make_list_cell(env, h, list);
    }    
    return make_result(env, hp, list);
}

// get_conn_info(Handle::handle(), Addr::bdaddr(), Type::uint8())
static ERL_NIF_TERM nif_get_conn_info(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    handle_t* hp;
    struct hci_conn_info_req ci;
    unsigned type;
    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    memset(&ci, 0, sizeof(ci));    
    if (!get_bdaddr(env, argv[1], &ci.bdaddr))
	return make_hbadarg(env, hp);
    if (!enif_get_uint(env, argv[2], &type));
    ci.type = type;
    
    if (ioctl(hp->fd, HCIGETCONNINFO, (void *)&ci) < 0)
	return make_herror(env, hp, errno);
    return make_result(env, hp, make_conn_info(env, ci.conn_info));
}

// get_auth_info(Hci::handle(), Addr::bdaddr()) -> {ok, Type}
static ERL_NIF_TERM nif_get_auth_info(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    handle_t* hp;
    struct hci_auth_info_req ai;
    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!get_bdaddr(env, argv[1], &ai.bdaddr))
	return make_hbadarg(env, hp);    
    if (ioctl(hp->fd, HCIGETAUTHINFO, &ai) < 0)
	return make_herror(env, hp, errno);
    return make_result(env, hp, enif_make_uint(env, ai.type));
}

// set_raw(Hci) -> ok || set_raw(Hci, DevID) -> ok
static ERL_NIF_TERM nif_set_raw(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{    
    handle_t* hp;
    int dev_id;

    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!get_dev_id(env, 2, argc, argv, hp, &dev_id))
	return make_hbadarg(env, hp);

    if (ioctl(hp->fd, HCISETRAW, dev_id) < 0)
	return make_herror(env, hp, errno);
    return make_ok(env, hp);
}

// set_scan(Hci::handle(), Opt::uint32()) -> ok ||
// set_scan(Hci::handle(), DevID::devid(), Opt::uint32()) -> ok
static ERL_NIF_TERM nif_set_scan(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    struct hci_dev_req dr;    
    handle_t* hp;
    int dev_id;
    uint32_t dev_opt;
    int i;
    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!(i=get_dev_id(env, 3, argc, argv, hp, &dev_id)))
	return make_hbadarg(env, hp);
    if (!enif_get_uint(env, argv[i], &dev_opt))
	return make_hbadarg(env, hp);
    dr.dev_id = dev_id;
    dr.dev_opt = dev_opt;
    if (ioctl(hp->fd, HCISETSCAN, (void*) &dr) < 0)
	return make_herror(env, hp, errno);
    return make_ok(env, hp);
}

// set_auth(Hci::handle(), enabled|disabled) -> ok ||
// set_scan(Hci::handle(), DevID::devid(), enabled|disabled) -> ok
static ERL_NIF_TERM nif_set_auth(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    struct hci_dev_req dr;    
    handle_t* hp;
    int dev_id;
    int dev_opt;
    int i;
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!(i=get_dev_id(env, 3, argc, argv, hp, &dev_id)))
	return make_hbadarg(env, hp);
    dr.dev_id = dev_id;
    if (argv[i] == ATOM(enabled))
	dr.dev_opt = AUTH_ENABLED;
    else if (argv[i] == ATOM(disabled))
	dr.dev_opt = AUTH_DISABLED;
    else if (enif_get_int(env, argv[i], &dev_opt))
	dr.dev_opt = dev_opt ? AUTH_ENABLED : AUTH_DISABLED;
    else
	return make_hbadarg(env, hp);
    if (ioctl(hp->fd, HCISETAUTH, (void*) &dr) < 0)
	return make_herror(env, hp, errno);
    return make_ok(env, hp);
}

// set_encrypt(Hci::handle(), p2p|disable) -> ok ||
// set_encrypt(Hci::handle(), DevID::devid(), p2p|disable) -> ok
static ERL_NIF_TERM nif_set_encrypt(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    struct hci_dev_req dr;    
    handle_t* hp;
    int dev_id;    
    int dev_opt;
    int i;
    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!(i=get_dev_id(env, 3, argc, argv, hp, &dev_id)))
	return make_hbadarg(env, hp);
    dr.dev_id = dev_id;
    if (argv[i] == ATOM(p2p))
	dr.dev_opt = ENCRYPT_P2P;
    else if (argv[i] == ATOM(disabled))
	dr.dev_opt = ENCRYPT_DISABLED;
    else if (enif_get_int(env, argv[i], &dev_opt))
	dr.dev_opt = dev_opt ? ENCRYPT_P2P : ENCRYPT_DISABLED;
    else
	return make_hbadarg(env, hp);
    if (ioctl(hp->fd,  HCISETENCRYPT, (void*) &dr) < 0)
	return make_herror(env, hp, errno);
    return make_ok(env, hp);    
}

// set_ptype(Hci::handle(), Type::unint32())
// set_ptype(Hci::handle(), DevID::devid(), Type::unint32())
static ERL_NIF_TERM nif_set_ptype(ErlNifEnv* env, int argc,
				  const ERL_NIF_TERM argv[])
{
    struct hci_dev_req dr;    
    handle_t* hp;
    int dev_id;
    uint32_t dev_opt;
    int i;
    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!(i=get_dev_id(env, 3, argc, argv, hp, &dev_id)))
	return make_hbadarg(env, hp);
    if (!enif_get_uint(env, argv[i], &dev_opt))
	return make_hbadarg(env, hp);
    dr.dev_id = dev_id;
    dr.dev_opt = dev_opt;
    if (ioctl(hp->fd, HCISETPTYPE, (void*) &dr) < 0)
	return make_herror(env, hp, errno);    
    return make_ok(env, hp);    
}

static ERL_NIF_TERM nif_set_link_policy(ErlNifEnv* env, int argc,
					const ERL_NIF_TERM argv[])
{
    struct hci_dev_req dr;
    handle_t* hp;
    int dev_id;
    uint32_t dev_opt;
    int i;
    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!(i = get_dev_id(env, 3, argc, argv, hp, &dev_id)))
	return make_hbadarg(env, hp);
    if (!enif_get_uint(env, argv[i], &dev_opt))
	return make_hbadarg(env, hp);    
    dr.dev_id = dev_id;
    dr.dev_opt = dev_opt;
    if (ioctl(hp->fd, HCISETLINKPOL, (void*) &dr) < 0)
	return make_herror(env, hp, errno);    
    return make_ok(env, hp);    
}

static ERL_NIF_TERM nif_set_link_mode(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    struct hci_dev_req dr;    
    handle_t* hp;
    int dev_id;
    uint32_t dev_opt;
    int i;
    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!(i = get_dev_id(env, 3, argc, argv, hp, &dev_id)))
	return make_hbadarg(env, hp);
    if (!enif_get_uint(env, argv[i], &dev_opt))
	return make_hbadarg(env, hp);
    dr.dev_id = dev_id;
    dr.dev_opt = dev_opt;
    if (ioctl(hp->fd, HCISETLINKMODE, (void*) &dr) < 0)
	return make_herror(env, hp, errno);    
    return make_ok(env, hp);        
}

static ERL_NIF_TERM nif_set_acl_mtu(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    struct hci_dev_req dr;
    handle_t* hp;
    int dev_id;
    unsigned mtu, mpkt;
    int i;
    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!(i=get_dev_id(env, 4, argc, argv, hp, &dev_id)))
	return make_hbadarg(env, hp);
    if (!enif_get_uint(env, argv[i], &mpkt))
	return make_hbadarg(env, hp);
    if (!enif_get_uint(env, argv[i+1], &mtu))
	return make_hbadarg(env, hp);
    dr.dev_id = dev_id;
    dr.dev_opt = htobl(htobs(mpkt) | (htobs(mtu) << 16));
    if (ioctl(hp->fd, HCISETACLMTU, (void*) &dr) < 0)
	return make_herror(env, hp, errno);
    return make_ok(env, hp);    
}

static ERL_NIF_TERM nif_set_sco_mtu(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    struct hci_dev_req dr;    
    handle_t* hp;
    int dev_id;
    unsigned mtu, mpkt;
    int i;
	
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!(i=get_dev_id(env, 4, argc, argv, hp, &dev_id)))
	return make_hbadarg(env, hp);
    if (!enif_get_uint(env, argv[i], &mpkt))
	return make_hbadarg(env, hp);
    if (!enif_get_uint(env, argv[i+1], &mtu))
	return make_hbadarg(env, hp);
    dr.dev_id = dev_id;
    dr.dev_opt = htobl(htobs(mpkt) | (htobs(mtu) << 16));
    if (ioctl(hp->fd, HCISETSCOMTU, (void*) &dr) < 0)
	return make_herror(env, hp, errno);
    return make_ok(env, hp);
}

static ERL_NIF_TERM nif_block(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    handle_t* hp;
    bdaddr_t addr;
    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!get_bdaddr(env, argv[1], &addr))
	return make_hbadarg(env, hp);
    if (ioctl(hp->fd, HCIBLOCKADDR, (void*) &addr) < 0)
	return make_herror(env, hp, errno);
    return make_ok(env, hp);
}

static ERL_NIF_TERM nif_unblock(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    handle_t* hp;
    bdaddr_t addr;
    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (!get_bdaddr(env, argv[1], &addr))
	return make_hbadarg(env, hp);
    if (ioctl(hp->fd, HCIUNBLOCKADDR, (void*) &addr) < 0)
	return make_herror(env, hp, errno);
    return make_ok(env, hp);    
}

// inquiry(Hci::handle(), Timeout::unsigned(),
//         NumResp::unsigned(), Lap::0|<<>>|<<A,B,C>>, Flags::uint32())
// inquiry(Hci::handle(), DevID::devid(), Timeout::unsigned(),
//         NumResp::unsigned(), Lap::0|<<>>|<<A,B,C>>, Flags::uint32())
//
static ERL_NIF_TERM nif_inquiry(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    size_t sz = sizeof(struct hci_inquiry_req)+sizeof(inquiry_info)*255;
    uint8_t ibuf[sz];
    struct hci_inquiry_req* ir;
    inquiry_info* ip;
    handle_t* hp;
    int dev_id;
    ErlNifBinary lap;
    unsigned flags;
    unsigned timeout;
    unsigned num_rsp;
    int i, err, r;
    ERL_NIF_TERM list;
    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    ir = (struct hci_inquiry_req*) ibuf;
    
    if (!(i=get_dev_id(env, 6, argc, argv, hp, &dev_id)))
	return make_hbadarg(env, hp);
    ir->dev_id = dev_id;
    if (!enif_get_uint(env, argv[i], &timeout))
	return make_hbadarg(env, hp);
    ir->length = (timeout / 1280) + ((timeout % 1280) != 0);
    if (!enif_get_uint(env, argv[i+1], &num_rsp))
	return make_hbadarg(env, hp);
    ir->num_rsp = num_rsp;
    if (!enif_inspect_binary(env, argv[i+2], &lap))
	return make_hbadarg(env, hp);
    if (lap.size == 0) {
	ir->lap[0] = 0x33;
	ir->lap[1] = 0x8b;
	ir->lap[2] = 0x9e;
    }
    else if (lap.size == 3)
	memcpy(ir->lap, lap.data, 3);
    else
	return make_hbadarg(env, hp);
    if (!enif_get_uint(env, argv[i+3], &flags))
	return make_hbadarg(env, hp);
    ir->flags = flags;
    // set_nonblock(hp->fd, 0);
    r = ioctl(hp->fd, HCIINQUIRY, (void*) ir);
    err = errno;
    // set_nonblock(hp->fd, 1);
    if (r < 0)
	return make_herror(env, hp, err);
    ip = (inquiry_info*) (((uint8_t*) ir) + sizeof(struct hci_inquiry_req));
    
    list = enif_make_list(env, 0);
    for (i = ir->num_rsp-1; i >= 0; i--) {
	ERL_NIF_TERM h = make_inquiry_info(env, &ip[i]);
	list = enif_make_list_cell(env, h, list);
    }
    return make_result(env, hp, list);

}


static ERL_NIF_TERM nif_debug(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    return ATOM(ok);
}

    
// write(Handle::ref(), Data::binary())
static ERL_NIF_TERM nif_write(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    handle_t* hp;
    ErlNifBinary binary;
    int n = 0;

    if (!enif_inspect_iolist_as_binary(env,argv[1],&binary))
	return BADARG(env);    
    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    n = write(hp->fd, binary.data, binary.size);
    if (n < 0)
	return make_herror(env, hp, errno);
    return make_result(env, hp, enif_make_int(env, n));
}

static ERL_NIF_TERM nif_read(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    handle_t* hp;
    uint8_t* ptr;
    unsigned char buffer[MAX_HCI_BUF];
    ERL_NIF_TERM data;
    int n;

    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    n = read(hp->fd, (char*)buffer, sizeof(buffer));
    if (n < 0)
	return make_herror(env, hp, errno);
    ptr = enif_make_new_binary(env, n, &data);
    memcpy(ptr, buffer, n);
    return make_result(env, hp, data);
}


static ERL_NIF_TERM nif_select(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    ErlNifPid pid;    
    handle_t* hp;
    int mask;

    if (argv[1] == ATOM(read))
	mask = ERL_NIF_SELECT_READ;
    else if (argv[1] == ATOM(write))
	mask = ERL_NIF_SELECT_WRITE;
    else
	return BADARG(env);
    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }

    enif_self(env, &pid);
    enif_select(env, (ErlNifEvent)(intptr_t)(hp->fd),
		mask,
		hp, &pid, ATOM(undefined));
    return make_ok(env, hp);
}


// create all tracing NIFs
#ifdef NIF_TRACE

#undef NIF

static void trace_print_arg_list(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[])
{
    enif_fprintf(stdout, "(");
    if (argc > 0) {
	int i;
	if (enif_is_ref(env, argv[0])) {
	    // FIXME print object type if available
	    enif_fprintf(stdout, "%T", argv[0]);
	}
	else
	    enif_fprintf(stdout, "%T", argv[0]);
	for (i = 1; i < argc; i++)
	    enif_fprintf(stdout, ",%T", argv[i]);
    }
    enif_fprintf(stdout, ")");
}

#define NIF(name, arity, func)					\
static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]) \
{ \
    ERL_NIF_TERM result;					\
    enif_fprintf(stdout, "ENTER %s", (name));			\
    trace_print_arg_list(env, argc, argv);			\
    enif_fprintf(stdout, "\r\n");				\
    result = func(env, argc, argv);				\
    enif_fprintf(stdout, "  RESULT=%T\r\n", (result));		\
    enif_fprintf(stdout, "LEAVE %s\r\n", (name));		\
    return result;						\
}

NIF_LIST

#endif

static int load_atoms(ErlNifEnv* env)
{
    LOAD_ATOM(ok);
    LOAD_ATOM(error);
    LOAD_ATOM(undefined);
    LOAD_ATOM(select);
    LOAD_ATOM(read);
    LOAD_ATOM(write);
    LOAD_ATOM(no_such_handle);

    LOAD_ATOM(enabled);
    LOAD_ATOM(disabled);
    LOAD_ATOM(p2p);
    LOAD_ATOM(hci_filter);
    LOAD_ATOM(hci_dev_info);
    LOAD_ATOM(hci_dev_stats);
    LOAD_ATOM(hci_conn_info);
    LOAD_ATOM(inquiry_info);
    
    return 0;
}


static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    ErlNifResourceFlags tried;
    ErlNifResourceTypeInit cb;
    DEBUGF("load%s", "");

    cb.dtor = (ErlNifResourceDtor*) dtor;
    cb.stop = (ErlNifResourceStop*) stop;
    cb.down = (ErlNifResourceDown*) down;
    
    if ((hci_r =
	 enif_open_resource_type_x(env, "hci",
				   &cb, ERL_NIF_RT_CREATE,
				   &tried)) == NULL) {
	return -1;
    }
    if (load_atoms(env) < 0)
	return -1;

    *priv_data = 0;
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data,
		   void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    ErlNifResourceFlags tried;    
    ErlNifResourceTypeInit cb;
    
    DEBUGF("upgrade%s", "");

    cb.dtor = (ErlNifResourceDtor*) dtor;
    cb.stop = (ErlNifResourceStop*) stop;
    cb.down = (ErlNifResourceDown*) down;

    if ((hci_r = enif_open_resource_type_x(env, "hci", &cb,
					   ERL_NIF_RT_CREATE |
					   ERL_NIF_RT_TAKEOVER,
					   &tried)) == NULL) {
	return -1;
    }
    if (load_atoms(env) < 0)
	return -1;    
    *priv_data = *old_priv_data;
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
    UNUSED(env);
    UNUSED(priv_data);
    DEBUGF("unload%s", "");
}

ERL_NIF_INIT(bt_hci, nif_funcs, load, NULL, upgrade, unload)

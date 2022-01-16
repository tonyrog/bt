//
// SCO
//
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <poll.h>
#include <sys/socket.h>
#include <bluetooth/bluetooth.h>
#include <bluetooth/sco.h>

#include "erl_nif.h"
#include "erl_driver.h"
#include "bt_lib.h"

#define MAX_SCO_BUF 65536

//#define DEBUG
//#define NIF_TRACE

#define UNUSED(a) ((void) a)

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

// require env in context (ugly)
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

#define NIF_LIST \
    NIF("open_", 0,  nif_open)  \
    NIF("bind_", 2,  nif_bind)  \
    NIF("close", 1, nif_close) \
    NIF("listen_", 1, nif_listen) \
    NIF("connect_", 2,  nif_connect)  \
    NIF("accept_", 1, nif_accept) \
    NIF("get_conninfo", 1, nif_get_conninfo)	\
    NIF("getsockname", 1, nif_getsockname)	\
    NIF("getpeername", 1, nif_getpeername) \
    NIF("write_", 2, nif_write) \
    NIF("read_", 1,  nif_read) \
    NIF("select_", 2,  nif_select)

static ErlNifResourceType* sco_r;

typedef enum {
    UNDEFINED   = 0x00,
    OPEN        = 0x01,
    CLOSING     = 0x02,
    MONITOR     = 0x04,

    BOUND       = 0x10,    
    LISTEN      = 0x20,
    CONNECTED   = 0x40,
    CONNECTING  = 0x80,
} hstate_t;

typedef struct _handle_t {
    ErlNifMutex*  access_mtx;
    int           access_count;
    struct sockaddr_sco addr;      // peer address/connect/accept
    hstate_t      state;
    int           fd;
    ErlDrvMonitor mon;             // monitor when selecting    
} handle_t;

DECL_ATOM(ok);
DECL_ATOM(error);
DECL_ATOM(undefined);
DECL_ATOM(select);
DECL_ATOM(no_such_handle);

DECL_ATOM(sco_conninfo);

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


static void dtor(ErlNifEnv* env, handle_t* hp)
{
    UNUSED(env);

    DEBUGF("dtor: hp=%p fd=%d, state=0x%x", hp, hp->fd, hp->state);
    if (hp->fd >= 0) {
	close(hp->fd);
	hp->fd = -1;
	hp->state = UNDEFINED;
    }
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
    if (!enif_get_resource(env, arg, sco_r, (void **)&hp))
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
	if ((hp->state == CONNECTED) ||
	    (hp->state == CONNECTING) ||
	    (hp->state == LISTEN)) {
	    hp->state = CLOSING;
	    enif_select(env, (ErlNifEvent)hp->fd,
			ERL_NIF_SELECT_STOP,
			hp, NULL, ATOM(undefined));
	}
	else if (hp->fd >= 0) {
	    ERRORF("bt_sco: force closing %d", hp->fd); // for real?
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

static ERL_NIF_TERM nif_open(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    handle_t* hp;
    int fd;
    ERL_NIF_TERM ht;
    
    if ((fd = socket(PF_BLUETOOTH, SOCK_SEQPACKET, BTPROTO_SCO))  < 0)
	return make_error(env, errno);

    if ((hp = enif_alloc_resource(sco_r, sizeof(handle_t))) == NULL) {
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
    hp->access_mtx = enif_mutex_create("sco_access");
    hp->fd = fd;
    hp->state = OPEN;
    ht = enif_make_resource(env, hp);
    enif_release_resource(hp);
    return enif_make_tuple2(env, ATOM(ok), ht);
}

// listen(Handle::ref(), Address::btaddress())
static ERL_NIF_TERM nif_bind(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    handle_t* hp;    
    bdaddr_t bdaddr;
    struct sockaddr_sco addr = { 0 };

    if (!get_bdaddr(env, argv[1], &bdaddr))
	return BADARG(env);
    addr.sco_family = AF_BLUETOOTH;
    addr.sco_bdaddr = bdaddr;

    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    DEBUGF("nif_bind: hp=%p, close fd=%d, state=0x%x", hp, hp->fd, hp->state);
    
    if (bind(hp->fd, (struct sockaddr *)&addr, sizeof(addr)) < 0)
	return make_herror(env, hp, errno);
    hp->state |= BOUND;  // set under lock?
    return make_ok(env, hp);
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

// connect(Handle::ref(), Address::btaddress(), Psm::integer())
static ERL_NIF_TERM nif_connect(ErlNifEnv* env, int argc,
				const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    handle_t* hp;    
    int psm;
    bdaddr_t bdaddr;    

    if (!get_bdaddr(env, argv[1], &bdaddr))
	return BADARG(env);
    if (!enif_get_int(env, argv[2], &psm))
	return BADARG(env);
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {  // BOUND?
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    memset(&hp->addr, 0, sizeof(hp->addr));
    hp->addr.sco_family = AF_BLUETOOTH;
    hp->addr.sco_bdaddr = bdaddr;

    DEBUGF("nif_connect: hp=%p, fd=%d, state=0x%x", hp, hp->fd, hp->state);

    if (connect(hp->fd, (struct sockaddr *)&hp->addr, sizeof(hp->addr))  < 0) {
	if (errno == EINPROGRESS)
	    hp->state |= CONNECTING;
	return make_herror(env, hp, errno);
    }
    hp->state = (hp->state & ~CONNECTING) | (hp->state & CONNECTED);
    return make_ok(env, hp);
}


// accept(ListenHandle::ref()) -> {ok,{Handle,BdAddr}} | {error, Reason}
static ERL_NIF_TERM nif_accept(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    handle_t* hp;
    handle_t* hp1;
    socklen_t opt = sizeof(struct sockaddr_sco);
    ERL_NIF_TERM bdaddr;
    ERL_NIF_TERM ht;
    int fd;

    switch(get_handle(env, argv[0], &hp, LISTEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    fd = accept(hp->fd, (struct sockaddr*) &hp->addr, &opt);
    if (fd < 0)
	return make_herror(env, hp, errno);
    if ((hp1 = enif_alloc_resource(sco_r, sizeof(handle_t))) == NULL) {
	int err = errno;
	close(fd);
	return make_herror(env, hp, err);
    }
    memset(hp1, 0, sizeof(handle_t));
    hp1->access_mtx = enif_mutex_create("sco_access");
    hp1->fd = fd;
    hp1->state |= CONNECTED;
    hp1->addr = hp->addr;
    ht = enif_make_resource(env, hp1);
    enif_release_resource(hp1);
    bdaddr = make_bdaddr(env, &hp1->addr.sco_bdaddr);
    return make_result(env, hp, enif_make_tuple2(env, ht, bdaddr));
}


// listen(Handle::ref(), Address::btaddress(), Psm::integer())
static ERL_NIF_TERM nif_listen(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    handle_t* hp;    

    switch(get_handle(env, argv[0], &hp, BOUND, CONNECTING|CONNECTED)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    if (listen(hp->fd, 1) < 0)
	return make_herror(env, hp, errno);
    hp->state |= LISTEN;  // set under lock?
    return make_ok(env, hp);
}


static ERL_NIF_TERM nif_getsockname(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    handle_t* hp;
    struct sockaddr_sco addr;
    socklen_t len;
    ERL_NIF_TERM bdaddr;

    switch(get_handle(env, argv[0], &hp, BOUND, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    DEBUGF("nif_getsockname: hp=%p, fd=%d, state=0x%x",
	   hp, hp->fd, hp->state);    
    len = sizeof(addr);
    if (getsockname(hp->fd, (struct sockaddr *) &addr, &len) < 0)
	return make_herror(env, hp, errno);

    bdaddr = make_bdaddr(env, &addr.sco_bdaddr);
    return make_result(env, hp, bdaddr);
}

static ERL_NIF_TERM nif_getpeername(ErlNifEnv* env, int argc,
				    const ERL_NIF_TERM argv[])
{
    UNUSED(argc);    
    handle_t* hp;
    struct sockaddr_sco addr;
    socklen_t len;
    ERL_NIF_TERM bdaddr;

    switch(get_handle(env, argv[0], &hp, CONNECTED|CONNECTING, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    DEBUGF("nif_getpeername: hp=%p, fd=%d, state=0x%x", hp, hp->fd, hp->state);

    len = sizeof(addr);
    if (getpeername(hp->fd, (struct sockaddr *) &addr, &len) < 0)
	return make_herror(env, hp, errno);
    hp->state = (hp->state & ~CONNECTING) | CONNECTED;     // FIXME
    bdaddr = make_bdaddr(env, &addr.sco_bdaddr);
    return make_result(env, hp, bdaddr);
}

static ERL_NIF_TERM nif_get_conninfo(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    UNUSED(argc);    
    handle_t* hp;    
    struct sco_conninfo info;
    socklen_t optlen = sizeof(info);
    ERL_NIF_TERM dev_class;
    ERL_NIF_TERM res;

    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }    
    if (getsockopt(hp->fd, SOL_SCO, SCO_CONNINFO, &info, &optlen ) < 0)
	return make_herror(env, hp, errno);
    
    memcpy(enif_make_new_binary(env, 3, &dev_class), info.dev_class, 3);
    res = enif_make_tuple3(env,
			   ATOM(sco_conninfo),
			   enif_make_uint(env, info.hci_handle),
			   dev_class);
    return make_result(env, hp, res);
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
    
    switch(get_handle(env, argv[0], &hp, CONNECTED, 0)) {
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
    unsigned char buffer[MAX_SCO_BUF];
    ERL_NIF_TERM data;
    int n;

    switch(get_handle(env, argv[0], &hp, CONNECTED, 0)) {
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
    int mask, res;
    
    if (!get_select_mask(env, argv[1], &mask))
	return BADARG(env);    
    switch(get_handle(env, argv[0], &hp, OPEN, 0)) {
    case -1: return enif_make_badarg(env);
    case 0:  return make_bad_handle(env);
    default: break;
    }
    enif_self(env, &pid);
    if (mask & ERL_NIF_SELECT_CANCEL) {
	if (hp->state & MONITOR)
	    enif_demonitor_process(env, hp, &hp->mon);
	hp->state &= ~MONITOR;
    }
    else {
	if (!enif_monitor_process(env, hp, &pid, &hp->mon))
	    hp->state |= MONITOR;
    }
    res = enif_select(env, (ErlNifEvent)hp->fd,mask,hp,&pid,ATOM(undefined));
    done_handle(env, hp);
    return make_select_result(env, mask, res);    
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
    bt_lib_load_atoms(env);
    
    LOAD_ATOM(ok);
    LOAD_ATOM(error);
    LOAD_ATOM(undefined);
    LOAD_ATOM(select);
    LOAD_ATOM(no_such_handle);
    
    LOAD_ATOM(sco_conninfo);    
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
    
    if ((sco_r =
	 enif_open_resource_type_x(env, "sco",
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

    if ((sco_r = enif_open_resource_type_x(env, "sco", &cb,
					     ERL_NIF_RT_CREATE|
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

ERL_NIF_INIT(bt_sco, nif_funcs, load, NULL, upgrade, unload)

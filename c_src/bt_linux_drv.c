//
// Work in progress (linux) bluetooth driver
//

#include <sys/types.h>
#include <stdlib.h>
#include <stddef.h>
#include <unistd.h>
#include <string.h>
#include <memory.h>
#include <poll.h>
#include <errno.h>
#include <fcntl.h>

#include <bluetooth/bluetooth.h>
#include <bluetooth/l2cap.h>
#include <bluetooth/rfcomm.h>

#include "dthread/include/dlog.h"
#include "dthread/include/ddata.h"
#include "bt_drv.h"
#include "bt_poll.h"

#define RFCOMM_CHANNEL_ID_IS_VALID(x) (((x) >= 1) && ((x) <= 30))
#define PSM_IS_VALID(x) (((x) >= 0) && ((x) <= 0xffff))

#define PTR2INT(x) ((int)((long)(x)))
#define INT2PTR(x) ((void*)((long)(x)))

#define alloc_type(type) calloc(1, sizeof(type))


int set_nonblock(int fd)
{
   int flags;
#if defined(O_NONBLOCK)
    if (-1 == (flags = fcntl(fd, F_GETFL, 0)))
        flags = 0;
    return fcntl(fd, F_SETFL, flags | O_NONBLOCK);
#else
    flags = 1;
    return ioctl(fd, FIOBIO, &flags);
#endif
}


#define ERR_SHORT 1
#define ERR_LONG  2
/* type: 1=short-atom  2=string-long  3=both (encapsule in LIST ot TUPLE) */
static void ddata_put_io_error(ddata_t* data, int errnum, int type)
{
    if (errnum == 0) {
	if (type&1) ddata_put_atom(data, "Success");
	if (type&2) ddata_put_string(data, "OK");
    }
    else {
	if (type&1) ddata_put_atom(data, "Error"); // fixme
	if (type&2) ddata_put_string(data, strerror(errnum));	
    }
}

static void cleanup(subscription_t* s)
{
    DEBUGF("cleanup: %s", format_subscription(s));
    switch(s->type) {
    case INQUIRY: break;
    case REMOTE_NAME: break;
    case CONNECT: break;
    case SDP_QUERY: break;
    case SDP: break;
    case RFCOMM: break;
    case RFCOMM_LISTEN: break;
    case L2CAP: break;
    case L2CAP_LISTEN: break;
    default:  // warn?
	break;
    }
}

// setup "standard" reply buf, with initial 32 but size 
static void mesg_setup(ddata_t* data, uint8_t* buf, size_t size)
{
    ddata_init(data, buf, size, 0);
    ddata_put_UINT32(data, 0);
}

/* Send OK reply */
static void reply_ok(uint32_t cmdid)
{
    uint8_t buf[16];
    ddata_t data;

    mesg_setup(&data, buf, sizeof(buf));
    ddata_put_tag(&data, REPLY_OK);
    ddata_put_UINT32(&data, cmdid);
    ddata_send(&data, 1);
    ddata_final(&data);
}

/* Send ERROR reply */
static void reply_error(uint32_t cmdid, int err)
{
    uint8_t buf[128];
    ddata_t data;

    mesg_setup(&data, buf, sizeof(buf));
    ddata_put_tag(&data, REPLY_ERROR);
    ddata_put_UINT32(&data, cmdid);
    ddata_put_io_error(&data, err, ERR_SHORT);
    ddata_send(&data, 1);
    ddata_final(&data);
}

// simple event
static void send_event(uint32_t sid, const char* evtname)
{
    uint8_t buf[64];
    ddata_t data;

    mesg_setup(&data, buf, sizeof(buf));
    ddata_put_tag(&data, REPLY_EVENT);
    ddata_put_UINT32(&data, sid);
    ddata_put_atom(&data, evtname);
    ddata_send(&data, 1);
    ddata_final(&data);
}

static inline int get_address(ddata_t* data, bdaddr_t* val)
{
    if (ddata_r_avail(data) >= sizeof(bdaddr_t)) {
	memcpy(val, data->rd, sizeof(bdaddr_t));
	data->rd += sizeof(bdaddr_t);
	return 1;
    }
    return 0;
}

// CALLBACK 
static void rfcomm_running(struct pollfd* pfd, void* arg)
{
    subscription_t* s = arg;

    if (pfd->revents & POLLIN) {  // input ready
	DEBUGF("rfcomm_running: %d has input", PTR2INT(s->handle));
    }

    if (pfd->revents & POLLOUT) {  // output ready
	DEBUGF("rfcomm_running: %d may output", PTR2INT(s->handle));
    }
}

// CALLBACK 
static void rfcomm_connected(struct pollfd* pfd, void* arg)
{
    subscription_t* s = arg;
    (void) pfd;
    DEBUGF("rfcomm_connected: %d", PTR2INT(s->handle));
    reply_ok(s->cmdid);
    // FIXME: check error
    s->cmdid = 0;
    bt_poll_set_cb(PTR2INT(s->handle),  rfcomm_running);
    bt_poll_set_events(PTR2INT(s->handle), POLLIN);
}


void bt_command(bt_ctx_t* ctx, const uint8_t* src, uint32_t src_len)
{
    uint8_t  op = 0;
    uint32_t cmdid = 0;
    int      bt_error = 0;
    uint8_t  out_buf[2048];
    ddata_t data_in;
    ddata_t data_out;

    // dump subscription list
    if (dlog_debug_level == DLOG_DEBUG) {
	subscription_link_t* p = ctx->list.first;
	fprintf(stderr, "ctx.list = {");
	while(p) {
	    fprintf(stderr, " %s,", format_subscription(p->s));
	    p = p->next;
	}
	fprintf(stderr, "}\r\n");
    }

    ddata_r_init(&data_in, (uint8_t*)src, src_len, 0);
    mesg_setup(&data_out, out_buf, sizeof(out_buf));

    if (!ddata_get_uint8(&data_in, &op))
	goto badarg;
    if (!ddata_get_uint32(&data_in, &cmdid))
	goto badarg;

    switch (op) {
    case CMD_PING: {
	DEBUGF("CMD_PING cmdid=%d", cmdid);
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	ddata_put_string(&data_out, "pong");
	goto reply;
    }
    case CMD_DEBUG: {  // <<level>>
	int level;
	DEBUGF("CMD_DEBUG cmdid=%d", cmdid);
	if (!ddata_get_int32(&data_in, &level))
	    goto badarg;
	dlog_set_debug(level);
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	goto reply;
    }	

    case CMD_RFCOMM_OPEN: { /* id:32 bt-address(6) channel-id:8 */
	uint32_t sid;
	bdaddr_t bt_addr;
	struct sockaddr_rc addr;
	int sock;
	int r;
	uint8_t channel_id;
	subscription_t* s;

	DEBUGF("CMD_RFCOMM_OPEN cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if(!get_address(&data_in, &bt_addr))
	    goto badarg;
	if(!ddata_get_uint8(&data_in, &channel_id))
	    goto badarg;
	if (!RFCOMM_CHANNEL_ID_IS_VALID(channel_id))
	    goto badarg;
	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;

	if ((sock = socket(AF_BLUETOOTH, SOCK_STREAM, BTPROTO_RFCOMM)) < 0)
	    goto bt_error;

	addr.rc_family = AF_BLUETOOTH;
	addr.rc_channel = channel_id;
	bacpy(&addr.rc_bdaddr, &bt_addr);
	if (set_nonblock(sock) < 0) {
	    bt_error = errno;
	    close(sock);
	    goto bt_error;
	}
	if ((r = connect(sock, (struct sockaddr*)&addr, sizeof(addr))) < 0) {
	    if (errno != EINPROGRESS) {
		bt_error = errno;
		close(sock);
		goto bt_error;
	    }
	}
	if ((s = new_subscription(RFCOMM,sid,cmdid,NULL,cleanup)) == NULL) {
	    close(sock);
	    goto mem_error;
	}
	if (r < 0) // inprogress
	    bt_poll_add(sock, POLLOUT, rfcomm_connected, s);
	else
	    bt_poll_add(sock, POLLIN, rfcomm_running, s);
	s->handle = INT2PTR(sock);
	insert_last(&ctx->list, s);
	break;
    }

    case CMD_RFCOMM_CLOSE: { /* arguments: id:32 */
	uint32_t sid;
	subscription_link_t* link;

	DEBUGF("CMD_RFCOMM_CLOSE cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;

	if ((link = find_subscription_link(&ctx->list,RFCOMM,sid)) != NULL) {
	    subscription_t* s = link->s;
	    int sock;
	    s->cmdid = cmdid;
	    sock = PTR2INT(s->handle);
	    if (sock >= 0) {
		DEBUGF("RFCOMM_CLOSE: channel=%d", sock);
		bt_poll_del(sock);
		close(sock);
		unlink_subscription(link);
		goto done;
	    }
	    else if (s->accept != NULL) {
		listen_queue_t* lq = (listen_queue_t*)((s->accept)->opaque);
		remove_subscription(&lq->wait,RFCOMM,sid);
		unlink_subscription(link);
		goto ok;
	    }
	}
	else if ((link = find_subscription_link(&ctx->list,RFCOMM_LISTEN,sid)) != NULL) {
	    subscription_t* listen = link->s;
	    listen_queue_t* lq = (listen_queue_t*)listen->opaque;
	    subscription_link_t* link1;
	    /* remove all waiters */
	    while((link1=lq->wait.first) != NULL) {
		send_event(link1->s->id, "closed");
		unlink_subscription(link1);
	    }
	    unlink_subscription(link);
	    goto ok;
	}
	goto error;
    }


    case CMD_RFCOMM_SEND: { /* id:32, data/rest */
	uint32_t sid;
	subscription_t* s;
	int r;
	int len;
	int sock;

	DEBUGF("CMD_RFCOMM_SEND cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if ((s = find_subscription(&ctx->list,RFCOMM,sid)) == NULL)
	    goto badarg;
	if ((sock = PTR2INT(s->handle)) < 0)
	    goto badarg;
	s->cmdid = cmdid;

	if ((len = ddata_r_avail(&data_in)) > 0) {
	    if (s->out == NULL)
		s->out = ddata_new(data_in.rd, len);
	    else
		ddata_add(s->out, data_in.rd, len);
	    r = write(sock, s->out->rd, ddata_r_avail(s->out));
	    DEBUGF("CMD_RFCOMM_SEND wrote=%d", r);
	    if ((r >= 0) || ((r < 0) && (errno == EINPROGRESS))) {
		if (r < 0) r = 0;
		s->out->rd += r;
		if (ddata_r_avail(s->out) > 0)
		    bt_poll_set_events(PTR2INT(s->handle), POLLIN|POLLOUT);
		else {
		    ddata_reset(s->out);
		    s->cmdid = 0;
		    goto ok;
		}
	    }
	    else {
		s->cmdid = 0;
		goto error;
	    }
	}
	else {
	    s->cmdid = 0;
	    goto ok;
	}
	goto done;
    }


    default:
	DEBUGF("CMD_UNKNOWN = %d cmdid=%d", op, cmdid);
	goto badarg;
    }

    goto done;

ok:
    if (cmdid == 0)
	goto done;
    ddata_put_tag(&data_out, REPLY_OK);
    ddata_put_UINT32(&data_out, cmdid);
    goto reply;

mem_error:
    if (cmdid == 0)
	goto done;
    bt_error = ENOMEM;
    goto bt_error;

badarg:
    if (cmdid == 0)
	goto done;
    bt_error = EINVAL;
    goto bt_error;

error:
    bt_error = errno;
bt_error:
/* reset, just in case something was inserted */
    ddata_reset(&data_out);
    ddata_put_UINT32(&data_out, 0);
    ddata_put_tag(&data_out, REPLY_ERROR);
    ddata_put_UINT32(&data_out, cmdid);
    ddata_put_io_error(&data_out, bt_error, ERR_SHORT);
reply:
    ddata_send(&data_out, 1);
done:
    ddata_final(&data_out);
}

// CALLBACK 
void read_callback(struct pollfd* pfd, void* data)
{
    bt_ctx_t* ctx = (bt_ctx_t*) data;
    int fd = pfd->fd;
    int n;

    DEBUGF("read_callback");

    if (ctx->pbuf_len < sizeof(ctx->pbuf)) {
	int r = sizeof(ctx->pbuf) - ctx->pbuf_len;
	if ((n = read(fd, ctx->pbuf+ctx->pbuf_len, r)) < 0)
	    goto error;
	if (n == 0)
	    goto closed;
	ctx->pbuf_len += n;
	DEBUGF("READ: %d pbuf_len=%d", n, ctx->pbuf_len);
	if (ctx->pbuf_len == sizeof(ctx->pbuf)) {
	    ctx->len = (ctx->pbuf[0]<<24) + (ctx->pbuf[1]<<16) +
		(ctx->pbuf[2]<<8) + ctx->pbuf[3];
	    DEBUGF("READ: %d packet len=%d", n, ctx->len);
	    if (ctx->len > 0) {
		ctx->remain = ctx->len;
		ctx->packet = (uint8_t*) malloc(ctx->len);
		ctx->ptr = ctx->packet;
	    }
	    else {
		ctx->remain = 0;
		ctx->pbuf_len = 0;
	    }
	}
    }
    else {
	if ((n = read(fd, (void*)ctx->ptr, ctx->remain)) < 0)
	    goto error;
	if (n == 0)
	    goto closed;
	DEBUGF("READ: %d packet bytes", n);
	ctx->remain -= n;
	DEBUGF("PACKET: remain=%d", ctx->remain);
	ctx->ptr += n;
	if (ctx->remain == 0) {
	    bt_command(ctx, ctx->packet, ctx->len);
	    free(ctx->packet);
	    ctx->packet = NULL;
	    ctx->len = 0;
	    ctx->pbuf_len = 0;
	}
    }
    return;

error:
    DEBUGF("pipe read error",0);
    exit(1);

closed:
    DEBUGF("eof clean-up",0);
    exit(0);
}

static void  main_loop(bt_ctx_t* ctx)
{
    (void) ctx;
    
    while(1) {
	int r = bt_poll(-1);
	if (r < 0) {
	    DEBUGF("poll error %s", strerror(errno));
	}
    }
}

int main(int argc, char** argv)
{
    bt_ctx_t bt_info;
    (void) argc;
    (void) argv;

    dlog_init();
    dlog_set_debug(DLOG_DEFAULT);
    memset(&bt_info, 0, sizeof(bt_ctx_t));

    bt_poll_add(0, POLLIN, read_callback, &bt_info);
    main_loop(&bt_info);

    dlog_finish();
    DEBUGF("terminate",0);
    exit(0);
}

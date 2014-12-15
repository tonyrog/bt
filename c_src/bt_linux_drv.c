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

#include <bluetooth/bluetooth.h>
#include <bluetooth/l2cap.h>
#include <bluetooth/rfcomm.h>

#include "dlog.h"
#include "ddata.h"
#include "bt_drv.h"

#define alloc_type(type) calloc(1, sizeof(type))

typedef void (*event_cb_t)(struct pollfd*, void*);

int    poll_sz  = 0;     // allocated size 
int    poll_n = 0;       // used len 
struct pollfd* poll_fds = NULL;
event_cb_t* poll_cb = NULL;
void** poll_data = NULL;



void add_pollfd(int fd, short events, 
		void (*cb)(struct pollfd* pfd,void* data), 
		void* data)
{
    int i = poll_n;
    if (i == poll_sz) {
	size_t new_sz = 2*poll_sz+1;
	poll_fds  = realloc(poll_fds, new_sz*sizeof(struct pollfd));
	poll_cb   = realloc(poll_cb, new_sz*sizeof(event_cb_t));
	poll_data = realloc(poll_data, new_sz*sizeof(void*));
	poll_sz = new_sz;
    }
    poll_fds[i].fd = fd;
    poll_fds[i].events = events;
    poll_fds[i].revents = 0;
    poll_cb[i] = cb;
    poll_data[i] = data;
    poll_n++;
}

void swap_pollfd(int i, int j)
{
    if (i != j) {
	struct pollfd fds = poll_fds[i];
	event_cb_t cb     = poll_cb[i];
	void* data        = poll_data[i];

	poll_fds[i]  = poll_fds[j];
	poll_cb[i]   = poll_cb[j];
	poll_data[i] = poll_data[j];
	
	poll_fds[j]  = fds;
	poll_cb[j]   = cb;
	poll_data[j] = data;
    }
}

// find and remove the pollfd
void del_pollfd(int fd)
{
    int j;
    for (j = poll_n-1; j >= 0; j--) {
	if (poll_fds[j].fd == fd) {
	    swap_pollfd(j, poll_n-1);
	    poll_n--;
	    return;
	}
    }
}

int do_poll(int timeout)
{
    int r,r0;

    r = r0 = poll(poll_fds, poll_n, timeout);
    if (r > 0) {
	int i = 0;
	while((r > 0) && (i < (int)poll_n)) {
	    if ((poll_fds[i].revents & poll_fds[i].events) != 0) {
		int j = poll_n-1;
		// swap i and the last item, this make the poll set less
		// sensitive to starvation and easier to remove in the callback
		swap_pollfd(i, j);
		(*poll_cb[j])(&poll_fds[j], poll_data[j]);
		r--;
	    }
	    else
		i++;
	}
    }
    return r0;
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

    ddata_init(&data_in, (uint8_t*)src, src_len, 0, 0);
    ddata_init(&data_out, out_buf, sizeof(out_buf), sizeof(uint32_t), 0);

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
    goto error;

badarg:
    if (cmdid == 0)
	goto done;
    bt_error = EINVAL;

error:
/* reset, just in case something was inserted */
    ddata_reset(&data_out, sizeof(uint32_t));
    ddata_put_tag(&data_out, REPLY_ERROR);
    ddata_put_UINT32(&data_out, cmdid);
    ddata_put_io_error(&data_out, bt_error, ERR_SHORT);
reply:
    ddata_send(&data_out, 1);
done:
    ddata_final(&data_out);
}


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
	int r = do_poll(-1);
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
    memset(&bt_info, 0, sizeof(bt_ctx_t));

    add_pollfd(0, POLLIN, read_callback, &bt_info);
    main_loop(&bt_info);

    dlog_finish();
    DEBUGF("terminate",0);
    exit(0);
}

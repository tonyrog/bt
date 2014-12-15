//
// Work in progress (linux) bluetooth driver
//
#include <sys/types.h>
#include <stdlib.h>
#include <stddef.h>
#include <unistd.h>
#include <string.h>
#include <memory.h>

#include <bluetooth/bluetooth.h>
#include <bluetooth/l2cap.h>
#include <bluetooth/rfcomm.h>

#include "dlog.h"
#include "ddata.h"
#include "bt_drv.h"

#define alloc_type(type) calloc(1, sizeof(type))

static void  main_loop(bt_ctx_t* ctx)
{


}

int main(int argc, char** argv)
{
    bt_ctx_t bt_info;

    dlog_init();
    memset(&bt_info, 0, sizeof(bt_ctx_t));

    main_loop(&bt_info);

    dlog_finish();
    DEBUGF("terminate",0);
    exit(0);
}

#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$<.d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

CFLAGS+=-std=c99 -Wall -O3
LDFLAGS+=-shared

OBJS =
LIBS = 

all: ../priv/bt_l2cap_nif.so ../priv/bt_rfcomm_nif.so ../priv/hci_drv.so

../priv/bt_l2cap_nif.so: bt_l2cap_nif.o
	$(CC) $(LDFLAGS) bt_l2cap_nif.o $(LIBS) -o $@

../priv/bt_rfcomm_nif.so: bt_rfcomm_nif.o
	$(CC) $(LDFLAGS) bt_rfcomm_nif.o $(LIBS) -o $@

../priv/hci_drv.so: hci_drv.o
	$(CC) $(LDFLAGS) hci_drv.o $(LIBS) -o $@

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

.%.d:	;

-include .*.d

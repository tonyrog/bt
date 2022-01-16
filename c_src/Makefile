#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$<.d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

CFLAGS+=-std=c99 -Wall -O3
LDFLAGS+=-shared

OBJS = bt_lib.o
LIBS =
ALL = 	../priv/bt_hci_nif.so \
	../priv/bt_l2cap_nif.so \
	../priv/bt_rfcomm_nif.so \
	../priv/bt_sco_nif.so

all: $(ALL)

../priv/bt_hci_nif.so: bt_hci_nif.o $(OBJS)
	$(CC) $(LDFLAGS) bt_hci_nif.o $(OBJS) $(LIBS) -o $@

../priv/bt_l2cap_nif.so: bt_l2cap_nif.o $(OBJS)
	$(CC) $(LDFLAGS) bt_l2cap_nif.o $(OBJS) $(LIBS) -o $@

../priv/bt_rfcomm_nif.so: bt_rfcomm_nif.o $(OBJS)
	$(CC) $(LDFLAGS) bt_rfcomm_nif.o $(OBJS) $(LIBS) -o $@

../priv/bt_sco_nif.so: bt_sco_nif.o $(OBJS)
	$(CC) $(LDFLAGS) bt_sco_nif.o $(OBJS) $(LIBS) -o $@

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

.%.d:	;

-include .*.d

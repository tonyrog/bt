-ifndef(__AVDTP_HRL__).
-define(__AVDTP_HRL__, true).

-type uint16() :: 0..16#ffff.

-define(AVDTP_DISCOVER,		    16#01).
-define(AVDTP_GET_CAPABILITIES,	    16#02).
-define(AVDTP_SET_CONFIGURATION,    16#03).
-define(AVDTP_GET_CONFIGURATION,    16#04).
-define(AVDTP_RECONFIGURE,	    16#05).
-define(AVDTP_OPEN,                 16#06).
-define(AVDTP_START,                16#07).
-define(AVDTP_CLOSE,                16#08).
-define(AVDTP_SUSPEND,              16#09).
-define(AVDTP_ABORT,                16#0A).
-define(AVDTP_SECURITY_CONTROL,     16#0B).
-define(AVDTP_GET_ALL_CAPABILITIES, 16#0C).
-define(AVDTP_DELAY_REPORT,         16#0D).

-define(AVDTP_PKT_TYPE_SINGLE,   16#00).
-define(AVDTP_PKT_TYPE_START,    16#01).
-define(AVDTP_PKT_TYPE_CONTINUE, 16#02).
-define(AVDTP_PKT_TYPE_END,      16#03).

-define(AVDTP_MSG_TYPE_COMMAND,    16#00).
-define(AVDTP_MSG_TYPE_GEN_REJECT, 16#01).
-define(AVDTP_MSG_TYPE_ACCEPT,     16#02).
-define(AVDTP_MSG_TYPE_REJECT,     16#03).

-define(AVDTP(Label,MessageType,RFA,SId,Data),
	<<Label:4,?AVDTP_PKT_TYPE_SINGLE:2,MessageType:2,
	  RFA:2,SId:6,Data/binary>>).

-define(AVDTP_START(Label,MessageType,NPackets,RFA,SId,Data),
	<<Label:4,?AVDTP_PKT_TYPE_START:2,MessageType:2,
	  NPackets:8,
	  RFA:2,SId:6,Data/binary>>).

-define(AVDTP_CONTINUE(Label,MessageType,RFA,SId,Data),
	<<Label:4,?AVDTP_PKT_TYPE_CONTINUE:2,MessageType:2,
	  RFA:2,SId:6,Data/binary>>).

-define(AVDTP_END(Label,MessageType,RFA,SId,Data),
	<<Label:4,?AVDTP_PKT_TYPE_END:2,MessageType:2,
	  RFA:2,SId:6,Data/binary>>).

-record(seid_info, 
	{
	 seid,        %% unsigned:6 
	 inuse,       %% unsigned:1
	 media_type,  %% unsigned:4
	 type         %% type:1
	}).

-define(SEID_INFO(Seid,Inuse,RFA1,MediaType,Type,RFA2),
	<<Seid:6, Inuse:1, RFA1:1, 
	  MediaType:4, Type:1, RFA2:3>>).

-define(SEID(Seid, RFA1), <<Seid:6, RFA1:2>>).


-endif.

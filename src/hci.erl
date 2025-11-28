%%% @author Tony Rogvall <tony@up13>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    HCI commands
%%% @end
%%% Created : 18 Apr 2015 by Tony Rogvall <tony@up13>

-module(hci).

-export([open/0, open/1]).
-export([close/1]).
-export([get_devices/0]).
-export([i/0]).
-export([scan/0, scan/1, scan/2]).
%%
-export([create_connection/6]).
-export([disconnect/4]).
-export([read_remote_name/1,read_remote_name/3]).
-export([read_local_name/0, read_local_name/2]).
-export([send/4]).
-export([call/5]).
%%-export([call/6]).
-export([call/7]).
-export([wait/6]).
-export([dev_class/1]).
-export([decode_status/1]).
-export([cname/1]).
%% -compile(export_all).

-include("../include/bt.hrl").
-include("../include/hci.hrl").
-include("hci_api.hrl").
-include("bt_log.hrl").

-define(GIAC, <<16#33,16#8B,16#9E>>).  %% General Inquiry access code
-define(LIAC, <<16#00,16#8B,16#9E>>).  %% limited Inquiry access code

-define(DEFAULT_TIMEOUT, 2000).
-define(INQUIRY_TIMEOUT, 10000).

-type hci_handle() :: bt_hci:handle().
-type hci_reason() :: bt_hci:reason().
-type hci_devid()  :: bt_hci:devid().
-type hci_channel()  :: bt_hci:channel().

-define(map_from_record(Name, Record),
	map_from_record_(record_info(fields, Name),
			 record_info(size, Name)-1,
			 Record)).

map_from_record_(Fields, Size, Record) when is_list(Fields), is_tuple(Record) ->
    maps:from_list([{F,element(I+1,Record)} ||
		       {F,I} <- lists:zip(Fields, lists:seq(1, Size))]).


get_devices() ->
    {ok,Hci} = bt_hci:open(),
    try get_devices_(Hci) of
	L -> L
    after
	close(Hci)
    end.

get_devices_(Hci) ->
    {ok,Devs} = bt_hci:get_dev_list(Hci),
    get_devices_(Hci, Devs).

get_devices_(Hci, [{DevID,_}|Ds]) ->
    case bt_hci:get_dev_info(Hci, DevID) of
	{ok,Info} ->
	    [?map_from_record(hci_dev_info,Info) |
	     get_devices_(Hci, Ds)];
	_Error ->
	    get_devices_(Hci, Ds)
    end;
get_devices_(_Hci, []) ->
    [].

find_device_(Hci, Name) ->
    {ok,Ds} = bt_hci:get_dev_list(Hci),
    find_device_(Hci, Name, Ds).

find_device_(Hci, Name, [{DevID,_}|Ds]) ->
    case bt_hci:get_dev_info(Hci, DevID) of
	{ok,Info} ->
	    if Info#hci_dev_info.name =:= Name; Name =:= default ->
		    ?map_from_record(hci_dev_info,Info);
	       true ->
		    find_device_(Hci, Name, Ds)
	    end;
	_Error ->
	    find_device_(Hci, Name, Ds)
    end;
find_device_(_Hci, _Name, []) ->
    false.

-spec open(Name::string()|default) ->
	  {ok, hci_handle()} | {error, hci_reason()}.
%%open(DevID) when is_integer(DevID) ->
%%    {ok,H} = bt_hci:open(),
%%    bind_(H, DevID, ?HCI_CHANNEL_RAW);
open(Name) when is_list(Name); Name =:= default ->
    {ok,Hci} = bt_hci:open(),
    case find_device_(Hci, Name) of
	false ->
	    bt_hci:close(Hci),
	    {error, enoent};
	DevInfo ->
	    bind_(Hci, maps:get(dev_id, DevInfo), ?HCI_CHANNEL_RAW)
    end.

-spec open() -> {ok, hci_handle()} | {error, hci_reason()}.
open() ->
    {ok,Hci} = bt_hci:open(),
    case bt_hci:get_dev_list(Hci) of
	{ok,[]} -> {error, enoent};
	{ok,[{DevID,_}|_]} -> bind_(Hci, DevID, ?HCI_CHANNEL_RAW);
	{error,_}=Error -> Error
    end.

-spec bind_(hci_handle(), hci_devid(), hci_channel()) -> 
	  {ok, hci_handle()} | {error, hci_reason()}.
	  
bind_(Hci, DevID, Channel) ->
    case bt_hci:bind(Hci, DevID, Channel) of
	ok -> {ok,Hci};
	Error -> Error
    end.

close(Hci) ->
    bt_hci:close(Hci).

%% dump information about bluetooth devices
i() ->
    {ok,Hci} = bt_hci:open(),
    case bt_hci:get_dev_list(Hci) of
	{ok,Devs} ->
	    lists:foreach(
	      fun({DevID,_DevFlags}) ->
		      case bt_hci:get_dev_info(Hci, DevID) of
			  {ok, Info} ->
			      io:format("~s\n", 
					[hci_util:format_hci_dev_info(Info)]);
			  Error ->
			      io:format("error: ~p\n", [Error])
		      end
	      end, Devs);
	{error,_} = Error ->
	    Error
    end.

-type inquiry_info_t() :: 
	#{
	  name => string(),  %% optional lookup names
	  bdaddr => bdaddr_t(),
	  pscan_rep_mode => uint8_t(),
	  pscan_period_mode => uint8_t(),
	  pscan_mode => uint8_t(),
	  dev_class => <<_:3>>,
	  clock_offset => uint16_t()
	 }.


-spec scan() -> {ok,[inquiry_info_t()]} | {error,hci_reason()}.
scan() ->
    scan(default, ?INQUIRY_TIMEOUT).

-spec scan(Timeout::integer()) ->
	  {ok,[inquiry_info_t()]} | {error,hci_reason()}.
scan(Timeout) when is_integer(Timeout) ->
    scan(default, Timeout);
scan(HciName) when is_list(HciName) ->
    scan(HciName, ?INQUIRY_TIMEOUT).

-spec scan(HciName::string()|default, Timeout::integer()) ->
	  {ok,[inquiry_info_t()]} | {error,hci_reason()}.
scan(HciName, Timeout) when is_integer(Timeout) ->
    case open(HciName) of
	{ok,Hci} ->
	    case bt_hci:inquiry(Hci, Timeout, 10, ?GIAC, 0) of
		{ok,Is} ->
		    {ok, make_inquiry_info(Hci,Is)};
		{error,_} = Error -> Error
	    end;
	{error,_} = Error ->
	    Error
    end.

make_inquiry_info(Hci, [I|Is]) ->
    [make_inq_info_(Hci, I) | make_inquiry_info(Hci, Is)];
make_inquiry_info(_Hci, []) ->
    [].

make_inq_info_(Hci, I) ->
    Info = ?map_from_record(inquiry_info, I),
    Addr = maps:get(bdaddr, Info),
    AddrStr = bt_util:format_address(Addr),
    io:format("~s: ~p\n", [AddrStr, Info]),
    case read_remote_name(Hci, Info, ?DEFAULT_TIMEOUT) of
	{ok,RemoteName} ->
	    io:format("~s : ~s\n", [RemoteName, AddrStr]),
	    Info#{ name => RemoteName };
	_Error ->
	    io:format("warning: unable to read name for device ~s\n", [AddrStr]),
	    Info
    end.

majors() -> 
    {misc, computer, phone, net_access, audio_video,
     peripheral, imaging, wearable, toy}.
	   
%% minor computer type
computers() ->
    {misc, desktop, server, laptop, handheld, palm, wearable}.

%% minor phone type
phones() ->
    {misc, cell, cordless, smart, wired, isdn, sim_card_reader_for}.

audio_video() ->
    {misc, headset, handsfree, reserved, microphone, loudspeaker,
     headphones, portable_audio, car_audio, set_top_box,
     hifi_audio, video_tape_recorder, video_camera,
     camcorder, video_display_and_loudspeaker, video_conferencing, 
     reserved, game_toy}.

peripherals() -> 
    {misc, joystick, gamepad, remote_control, sensing_device,
     digitiser_tablet, card_reader}.

wearables() ->
    {misc, wrist_watch, pager, jacket, helmet, glasses}.

toys() ->
    {misc, robot, vehicle, character, controller, game}.

%%
flag(Flags, Bit, Value) when Flags band Bit =:= Bit ->
    Value;
flag(_, _, _) ->
    [].

elem(I, Tag, Tuple) when I > tuple_size(Tuple) -> {Tag,I};
elem(I, _Tag, Tuple) -> element(I+1, Tuple).

dev_class(_DevClass = <<A0,A1,A2>>) ->
    <<Flags:11,Major:5,Minor:6,_:2>> = <<A2,A1,A0>>,
    %%io:format("flags=0x~3.16.0B, major=~w, minor=~w\n",[Flags,Major,Minor]),
    {flag(Flags,16#01,[position]) ++
     flag(Flags,16#02,[net]) ++
     flag(Flags,16#04,[render]) ++
     flag(Flags,16#08,[capture]) ++
     flag(Flags,16#10,[obex]) ++
     flag(Flags,16#20,[audio]) ++
     flag(Flags,16#40,[phone]) ++
     flag(Flags,16#80,[info]),
     case Major of
	 1 -> elem(Minor, computer, computers());
	 2 -> elem(Minor, phones, phones());
	 3 -> {usage,{Minor,56}};
	 4 -> elem(Minor, audio_video, audio_video());
	 5 -> [elem(Minor band 16#0f, peripheral, peripherals())|
	       flag(Minor,16#10,[with_keyboard]) ++
		   flag(Minor,16#20,[with_pointing_device])];
	 6 ->
	     flag(Minor, 16#02, [with_display]) ++
		 flag(Minor, 16#04, [with_camera]) ++
		 flag(Minor, 16#08, [with_scanner]) ++
		 flag(Minor, 16#10, [with_printer]);
	 7 -> elem(Minor, wearable, wearables());
	 8 -> elem(Minor, toy, toys());
	_ -> []
     end,
     elem(Major, major, majors())}.

create_connection(Hci, Bdaddr, Pkt_type, Clock_offset, Role_switch, Timeout) ->
    Pscan_rep_mode = 16#02,
    Pscan_mode = 0,
    case call(Hci,?OGF_LINK_CTL,?OCF_CREATE_CONN,
	      <<?create_conn_cp_bin(Bdaddr,Pkt_type,Pscan_rep_mode,Pscan_mode,Clock_offset,Role_switch)>>,
	      ?EVT_CONN_COMPLETE, undefined, Timeout) of
	{ok, #evt_conn_complete{status=0,handle=Handle}} ->
	    {ok, Handle};
	{ok, #evt_conn_complete{status=Status}} -> %% fixme: decode status
	    {error, decode_status(Status)};
	Error ->
	    Error
    end.

disconnect(Hci, Handle, Reason, Timeout) ->
    case call(Hci, ?OGF_LINK_CTL, ?OCF_DISCONNECT, 
	      <<?disconnect_cp_bin(Handle,Reason)>>,
	      ?EVT_DISCONN_COMPLETE, undefined, Timeout) of
	{ok, #evt_disconn_complete{status=0,handle=Handle}} ->
	    ok;
	{ok, #evt_disconn_complete{status=Status,handle=Handle}} ->
	    {error,decode_status(Status)};
	Error ->
	    Error
    end.

read_remote_name(Bdaddr) ->
    with_socket(fun(Hci) -> read_remote_name(Hci,Bdaddr,?DEFAULT_TIMEOUT) end).
			
read_remote_name(Hci, InquiryInfo, Timeout) when is_map(InquiryInfo) ->
    read_remote_name_(Hci, InquiryInfo, Timeout);
read_remote_name(Hci, Bdaddr, Timeout) when ?is_bt_address(Bdaddr) ->
    read_remote_name_(Hci,#{ bdaddr => Bdaddr },Timeout);
read_remote_name(Hci, <<A,B,C,D,E,F>>, Timeout) ->
    read_remote_name_(Hci,#{ bdaddr => {A,B,C,D,E,F} },Timeout);
read_remote_name(Hci, Bdaddr, Timeout) when is_list(Bdaddr) ->
    case bt_util:getaddr(Bdaddr) of
	{ok,Addr} ->
	    read_remote_name_(Hci, #{ bdaddr => Addr }, Timeout);
	Error ->
	    Error
    end.
    
read_remote_name_(Hci, InquiryInfo = #{ bdaddr := Bdaddr0 }, Timeout) ->
    Pscan_rep_mode = maps:get(pscan_rep_mode, InquiryInfo, 16#02),
    Pscan_mode     = maps:get(pscan_mode, InquiryInfo, 16#00),
    Clock_offset    = case maps:get(clock_offset,InquiryInfo,undefined) of
			  undefined -> 16#0000;
			  Offset -> Offset bor 16#8000
		      end,
    Bdaddr = list_to_binary(lists:reverse(tuple_to_list(Bdaddr0))),
    %% Bdaddr = list_to_binary(tuple_to_list(Bdaddr0)),
    case call(Hci,?OGF_LINK_CTL,?OCF_REMOTE_NAME_REQ,
	      <<?remote_name_req_cp_bin(Bdaddr,Pscan_rep_mode,Pscan_mode,
					Clock_offset)>>,
	      ?EVT_REMOTE_NAME_REQ_COMPLETE,undefined,Timeout) of
	{ok, #evt_remote_name_req_complete { status = 0, name = Name }} ->
	    {ok, cname(Name)};
	{ok, #evt_remote_name_req_complete { status = Status }} ->
	    {error, decode_status(Status)};
	Error -> Error
    end.

read_local_name() ->
    with_socket(fun(Hci) -> read_local_name(Hci, ?DEFAULT_TIMEOUT) end).

read_local_name(Hci, Timeout) ->
    case call(Hci,?OGF_HOST_CTL,?OCF_READ_LOCAL_NAME,<<>>,-1,undefined,Timeout) of
	{ok, <<?read_local_name_rp_bin(0,Name)>>} ->
	    {ok, hci_util:c_string(Name)};
	{ok, <<?read_local_name_rp_bin(Status,_Name)>>} ->
	    {error, decode_status(Status)};
	Error ->
	    Error
    end.


%% sending data and calling procedures over HCI socket

send(Hci,Opcode,Data) ->
    Pkt = <<?HCI_COMMAND_PKT,Opcode:16/little,
	    (byte_size(Data)):8,Data/binary>>,
    R = bt_hci:write(Hci,Pkt),
    ?debug("send ~p = ~p", [Pkt,R]),
    R.

send(Hci, OGF, OCF, Data) ->
    send(Hci, ?cmd_opcode_pack(OGF,OCF), Data).

call(Hci,OGF,OCF,Data,undefined) ->
    call(Hci,OGF,OCF,Data,-1,undefined,?DEFAULT_TIMEOUT);
call(Hci,OGF,OCF,Data,Decode) when is_function(Decode) -> 
    call(Hci,OGF,OCF,Data,-1,Decode,?DEFAULT_TIMEOUT).

%%call(Hci,OGF,OCF,Data,Event,Timeout) when is_integer(Event), 
%%					  is_integer(Timeout) ->
%%    call(Hci,OGF,OCF,Data,Event,undefined,Timeout).

call(Hci,OGF,OCF,Data,Event,Decode,Timeout) when 
      (is_integer(Event) andalso Event >= -1) andalso
      (Decode =:= undefined orelse is_function(Decode,1)) ->
    Opcode = ?cmd_opcode_pack(OGF,OCF),
    {ok,OldFilter} = bt_hci:get_filter(Hci),
    ?debug("call: saved_filter = ~p", 
	   [bt_hci:decode_filter(OldFilter)]),
    Events = if Event =:= -1 -> []; true -> [Event] end,
    NewFilter = bt_hci:make_filter(Opcode,
				    [?HCI_EVENT_PKT],
				    [?EVT_CMD_STATUS,
				     ?EVT_CMD_COMPLETE,
				     ?EVT_LE_META_EVENT
				    | Events]),
    ?debug("call: new_filter = ~p", 
	 [bt_hci:decode_filter(NewFilter)]),
    case bt_hci:set_filter(Hci, NewFilter) of
	ok ->
	    {ok,_} = send(Hci,Opcode,Data),
	    Reply = wait(Hci,Opcode,Event,Decode,10,Timeout),
	    bt_hci:set_filter(Hci, OldFilter),
	    Reply;
	Error ->
	    Error
    end.

wait(Hci,Opcode,Event,Decode,Try,Timeout) ->
    ?debug("Start timer timeout=~w", [Timeout]),
    TRef = start_timer(Timeout),
    Result = wait_(Hci,Opcode,Event,Decode,Try,TRef),
    case Result of
	timeout ->
	    {error, timeout};
	Result ->
	    cancel_timer(TRef),
	    Result
    end.

wait_(_Hci,_Opcode,_Event,_Decode,0,TRef) ->
    cancel_timer(TRef),
    {error, timeout};

wait_(Hci,Opcode,Event,Decode,Try,TRef) ->
    ok = bt_hci:select(Hci, read),
    receive
	{select,Hci,undefined,ready_input} ->
	    response_(Hci,Opcode,Event,Decode,Try,TRef);
	{timeout,TRef,_} ->
	    cancel_select(Hci, read),
	    timeout;
	Other ->
	    ?error("hci:wait/6 got ~p\n", [Other]),
	    error
    end.

cancel_select(Hci, Mode) ->
    case bt_hci:select(Hci, [cancel,Mode]) of
	{ok,cancelled}  -> ok;
	_ -> 
	    %% flush the message
	    receive
		{select,Hci,undefined,_Ready} -> ok
	    after 0 -> ok
	    end
    end.
    
response_(Hci,Opcode,Event,Decode,Try,TRef) ->
    case bt_hci:read(Hci) of
	{ok, _Data0 = <<_:8,Evt:8,Plen:8,Packet:Plen/binary,_/binary>>} ->
	    ?debug("Got data ~p", [_Data0]),
	    case Evt of
		?EVT_CMD_STATUS ->
		    ?debug("got cmd_status event=~p, evt=~p, packet=~p",
			   [Evt,Event,Packet]),
		    case Packet of
			<<?evt_cmd_status_bin(0,_Ncmd,Opcode),_R/binary>> ->
			    if Event =/= -1, Evt =/= Event ->
				    wait_(Hci,Opcode,Event,Decode,Try-1,TRef);
			       true ->
				    ok
			    end;
			<<?evt_cmd_status_bin(Status,_Ncmd,Opcode),_R/binary>> ->
			    {error, decode_status(Status)};
			<<?evt_cmd_status_bin(_S,_Ncmd,_Opcode),_R/binary>> ->
			    wait_(Hci,Opcode,Event,Decode,Try-1,TRef)
		    end;
		?EVT_CMD_COMPLETE ->
		    ?debug("got cmd_complete event=~p, evt=~p, packet=~p",
			 [Evt,Event,Packet]),
		    case Packet of
			<<?evt_cmd_complete_bin(_Ncmd,Opcode),R/binary>> ->
			    reply(Decode, R);
			<<?evt_cmd_complete_bin(_Ncmd,_Opcode),_R/binary>> ->
			    wait_(Hci,Opcode,Event,Decode,Try-1,TRef)
		    end;

		?EVT_REMOTE_NAME_REQ_COMPLETE when Evt =:= Event ->
		    ?debug("got remote_name_req_complete"),
		    {ok, hci_api:decode_evt_remote_name_req_complete(Packet)};
		?EVT_LE_META_EVENT ->
		    ?debug("got evt_le_meta_event"),
		    case Packet of
			<<?evt_le_meta_event_bin(SEvt,_D1),LePacket/binary>> ->
			    if SEvt =:= Event ->
				    Le = hci_api:decode_le(SEvt, LePacket),
				    {ok,Le};
			       true ->
				    wait_(Hci,Opcode,Event,Decode,Try-1,TRef)
			    end
		    end;
		_ ->
		    try hci_api:decode(Evt,Packet) of
			ReplyEvent ->
			    ?debug("decoded event"),
			    {ok, ReplyEvent}
		    catch
			error:Reason:StackTrace ->
			    ?error("decode error: ~p ~p", 
				   [Reason, StackTrace]),
			    {error, Reason}
		    end
	    end;
	{ok, _Data0} ->
	    ?debug("Got data ~p", [_Data0]),
            wait_(Hci,Opcode,Event,Decode,Try-1,TRef);
        Error = {error,_} ->
            Error
    end.

reply(undefined, Data) ->
    {ok,Data};
reply(Decode, Data) ->
    {ok, Decode(Data)}.

cname(<<0,_/binary>>) -> [];
cname(<<C,Cs/binary>>) -> [C|cname(Cs)];
cname(<<>>) -> [].


start_timer(0) -> undefined;
start_timer(Timeout) -> erlang:start_timer(Timeout, self(), timeout).

cancel_timer(undefined) -> false;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef),
    receive {timeout,TRef,_} -> ok
    after 0 -> ok
    end.

with_socket(Fun) ->
    {ok,S} = open(),
    try Fun(S) of
	Result ->
	    Result
    catch
	error:Error:Stack ->
	    ?error("hci: with_socket crash: ~p", [Error]),
	    lists:foreach(fun(Item) -> print_item(Item) end, Stack),
	    {error,Error}
    after
	close(S)
    end.

print_item({Module,Function,ArityArgs,Location}) ->
    File = proplists:get_value(file,Location,""),
    Line = proplists:get_value(line,Location,0),
    {Arity,Args} = 
	if is_list(ArityArgs) -> {length(ArityArgs),ArityArgs};
	   is_integer(ArityArgs) -> {ArityArgs, []}
	end,
    io:format("  ~s:~w: ~s:~s/~w ~p\n", [File,Line,Module,Function,Arity,Args]).

decode_status(Code) ->
    case Code of
	0 -> ok;
	?HCI_UNKNOWN_COMMAND -> unknown_command;
	?HCI_NO_CONNECTION -> no_connection;
	?HCI_HARDWARE_FAILURE -> hardware_failure;
	?HCI_PAGE_TIMEOUT -> page_timeout;
	?HCI_AUTHENTICATION_FAILURE -> authentication_failure;
	?HCI_PIN_OR_KEY_MISSING -> pin_or_key_missing;
	?HCI_MEMORY_FULL -> memory_full;
	?HCI_CONNECTION_TIMEOUT -> connection_timeout;
	?HCI_MAX_NUMBER_OF_CONNECTIONS -> max_number_of_connections;
	?HCI_MAX_NUMBER_OF_SCO_CONNECTIONS -> max_number_of_sco_connections;
	?HCI_ACL_CONNECTION_EXISTS -> acl_connection_exists;
	?HCI_COMMAND_DISALLOWED -> command_disallowed;
	?HCI_REJECTED_LIMITED_RESOURCES -> rejected_limited_resources;
	?HCI_REJECTED_SECURITY -> rejected_security;
	?HCI_REJECTED_PERSONAL -> rejected_personal;
	?HCI_HOST_TIMEOUT -> host_timeout;
	?HCI_UNSUPPORTED_FEATURE -> unsupported_feature;
	?HCI_INVALID_PARAMETERS -> invalid_parameters;
	?HCI_OE_USER_ENDED_CONNECTION -> oe_user_ended_connection;
	?HCI_OE_LOW_RESOURCES -> oe_low_resources;
	?HCI_OE_POWER_OFF -> oe_power_off;
	?HCI_CONNECTION_TERMINATED -> connection_terminated;
	?HCI_REPEATED_ATTEMPTS -> repeated_attempts;
	?HCI_PAIRING_NOT_ALLOWED -> pairing_not_allowed;
	?HCI_UNKNOWN_LMP_PDU -> unknown_lmp_pdu;
	?HCI_UNSUPPORTED_REMOTE_FEATURE -> unsupported_remote_feature;
	?HCI_SCO_OFFSET_REJECTED -> sco_offset_rejected;
	?HCI_SCO_INTERVAL_REJECTED -> sco_interval_rejected;
	?HCI_AIR_MODE_REJECTED -> air_mode_rejected;
	?HCI_INVALID_LMP_PARAMETERS -> invalid_lmp_parameters;
	?HCI_UNSPECIFIED_ERROR -> unspecified_error;
	?HCI_UNSUPPORTED_LMP_PARAMETER_VALUE -> unsupported_lmp_parameter_value;
	?HCI_ROLE_CHANGE_NOT_ALLOWED -> role_change_not_allowed;
	?HCI_LMP_RESPONSE_TIMEOUT -> lmp_response_timeout;
	?HCI_LMP_ERROR_TRANSACTION_COLLISION -> lmp_error_transaction_collision;
	?HCI_LMP_PDU_NOT_ALLOWED -> lmp_pdu_not_allowed;
	?HCI_ENCRYPTION_MODE_NOT_ACCEPTED -> encryption_mode_not_accepted;
	?HCI_UNIT_LINK_KEY_USED -> unit_link_key_used;
	?HCI_QOS_NOT_SUPPORTED -> qos_not_supported;
	?HCI_INSTANT_PASSED -> instant_passed;
	?HCI_PAIRING_NOT_SUPPORTED -> pairing_not_supported;
	?HCI_TRANSACTION_COLLISION -> transaction_collision;
	?HCI_QOS_UNACCEPTABLE_PARAMETER -> qos_unacceptable_parameter;
	?HCI_QOS_REJECTED -> qos_rejected;
	?HCI_CLASSIFICATION_NOT_SUPPORTED -> classification_not_supported;
	?HCI_INSUFFICIENT_SECURITY -> insufficient_security;
	?HCI_PARAMETER_OUT_OF_RANGE -> parameter_out_of_range;
	?HCI_ROLE_SWITCH_PENDING -> role_switch_pending;
	?HCI_SLOT_VIOLATION -> slot_violation;
	?HCI_ROLE_SWITCH_FAILED -> role_switch_failed;
	?HCI_EIR_TOO_LARGE -> eir_too_large;
	?HCI_SIMPLE_PAIRING_NOT_SUPPORTED -> simple_pairing_not_supported;
	?HCI_HOST_BUSY_PAIRING -> host_busy_pairing;
	_ -> {hci_status,Code}
    end.

	    
    

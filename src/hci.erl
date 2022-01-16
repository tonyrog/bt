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
-export([send/4, call/6, wait/5]).
-export([dev_class/1]).
%% -compile(export_all).

-include("../include/bt.hrl").
-include("../include/hci_drv.hrl").
-include("hci_api.hrl").


-ifdef(debug).
-define(dbg(F), io:format((F))).
-define(dbg(F,A), io:format((F),(A))).
-else.
-define(dbg(F), ok).
-define(dbg(F,A), ok).
-endif.

-define(GIAC, <<16#33,16#8B,16#9E>>).  %% General Inquiry access code
-define(LIAC, <<16#00,16#8B,16#9E>>).  %% limited Inquiry access code

-define(DEFAULT_TIMEOUT, 5000).
-define(INQUIRY_TIMEOUT, 10000).

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

	    
open(DevID) when is_integer(DevID) ->
    {ok,H} = bt_hci:open(),
    open1_(H, DevID);
open(Name) when is_list(Name); Name =:= default ->
    {ok,Hci} = bt_hci:open(),
    case find_device_(Hci, Name) of
	false ->
	    bt_hci:close(),
	    {error, enoent};
	DevInfo ->
	    open1_(Hci, maps:get(dev_id, DevInfo))
    end.

open() ->
    {ok,Hci} = bt_hci:open(),
    case bt_hci:get_dev_list(Hci) of
	{ok,[]} -> {error, enoent};
	{ok,[{DevID,_}|_]} -> open1_(Hci, DevID);
	{error,_}=Error -> Error
    end.

open1_(Hci, DevID) ->
    case bt_hci:bind(Hci, DevID) of
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
	Error ->
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


-spec scan() -> [inquiry_info_t()].


scan() ->    
    scan(default, ?INQUIRY_TIMEOUT).

-spec scan(Timeout::integer()) -> [inquiry_info_t()].

scan(Timeout) when is_integer(Timeout) ->
    scan(default, Timeout);
scan(Name) when is_list(Name) ->
    scan(Name, ?INQUIRY_TIMEOUT).

scan(Name, Timeout) ->
    case open(Name) of
	{ok,Hci} ->
	    case bt_hci:inquiry(Hci, Timeout, 10, ?GIAC, 0) of
		{ok,Is} ->
		    lists:foldl(
		      fun(I,Acc) ->
			      Info = ?map_from_record(inquiry_info, I),
			      Addr = maps:get(bdaddr, Info),
			      AddrStr = bt:format_address(Addr),
			      io:format("~s: ~p\n", [AddrStr, Info]),
			      case read_remote_name(Hci, Info, ?DEFAULT_TIMEOUT) of
				  {ok,Name} ->
				      io:format("~s : ~s\n", [Name, AddrStr]),
				      [Info#{ name => Name } | Acc];
				  _Error ->
				      io:format("warning: unable to read name for device ~s\n", [AddrStr]),
				      [Info|Acc]
			      end
		      end, [], Is);
		Error -> Error
	    end;
	Error ->
	    Error
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
	 5 -> elem(Minor band 16#0f, peripheral, peripherals()) ++
		  flag(Minor,16#10,[with_keyboard]) ++
		  flag(Minor,16#20,[with_pointing_device]);
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
	      ?EVT_CONN_COMPLETE, Timeout) of
	{ok, <<?evt_conn_complete_bin(0,Handle,_Bdaddr,_Link_type,_Encr_mode)>>} ->
	    {ok, Handle};
	{ok, <<?evt_conn_complete_bin(Status,_Handle,_Bdaddr,_Link_type,_Encr_mode)>>} ->
	    %% fixme: decode status
	    {error, Status};
	Error ->
	    Error
    end.

disconnect(Hci, Handle, Reason, Timeout) ->
    case call(Hci, ?OGF_LINK_CTL, ?OCF_DISCONNECT, 
	      <<?disconnect_cp_bin(Handle,Reason)>>,
	      ?EVT_DISCONN_COMPLETE, Timeout) of
	{ok, <<?evt_disconn_complete_bin(0,Handle,Reason)>>} ->
	    ok;
	{ok, <<?evt_disconn_complete_bin(_Status,Handle,Reason)>>} ->
	    {error,eio};
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
	      ?EVT_REMOTE_NAME_REQ_COMPLETE,Timeout) of
	{ok, <<?evt_remote_name_req_complete_bin(0,_Bdaddr,Name)>>} ->
	    {ok,hci_util:c_string(Name)};
	{ok, <<?evt_remote_name_req_complete_bin(_Status,_Bdaddr,_Name)>>} ->
	    {error, eio};
	Error -> Error
    end.

read_local_name() ->
    with_socket(fun(Hci) -> read_local_name(Hci, ?DEFAULT_TIMEOUT) end).

read_local_name(Hci, Timeout) ->
    case call(Hci,?OGF_HOST_CTL,?OCF_READ_LOCAL_NAME,<<>>,0,Timeout) of
	{ok, <<?read_local_name_rp_bin(0,Name)>>} ->
	    {ok, hci_util:c_string(Name)};
	{ok, <<?read_local_name_rp_bin(_Status,_Name)>>} ->
	    {error, eio};
	Error ->
	    Error
    end.


%% sending data and calling procedures over HCI socket

send(Hci,Opcode,Data) ->
    Pkt = <<?HCI_COMMAND_PKT,Opcode:16/little,
	    (byte_size(Data)):8,Data/binary>>,
    R = bt_hci:write(Hci,Pkt),
    ?dbg("send ~p = ~p\n", [Pkt,R]),
    R.

send(Hci, OGF, OCF, Data) ->
    send(Hci, ?cmd_opcode_pack(OGF,OCF), Data).

call(Hci,OGF,OCF,Data,Event,Timeout) ->
    Opcode = ?cmd_opcode_pack(OGF,OCF),
    {ok,OldFilter} = bt_hci:get_filter(Hci),
    %% ?dbg("call: saved_filter = ~p\n", [OldFilter]),
    NewFilter = bt_hci:make_filter(Opcode,
				    [?HCI_EVENT_PKT],
				    [?EVT_CMD_STATUS,
				     ?EVT_CMD_COMPLETE,
				     ?EVT_LE_META_EVENT,
				     Event]),
    %% ?dbg("call: new_filter = ~p\n", [NewFilter]),
    case bt_hci:set_filter(Hci, NewFilter) of
	ok ->
	    {ok,_} = send(Hci,Opcode,Data),
	    Reply = wait(Hci,Opcode,Event,10,Timeout),
	    bt_hci:set_filter(Hci, OldFilter),
	    Reply;
	Error ->
	    Error
    end.

wait(Hci,Opcode,Event,Try,Timeout) ->
    io:format("Start timer timeout=~w\n", [Timeout]),
    TRef = start_timer(Timeout),
    Result = wait_(Hci,Opcode,Event,Try,TRef),
    case Result of
	timeout ->
	    {error, timedout};
	Result ->
	    cancel_timer(TRef),
	    Result
    end.

wait_(_Hci,_Opcode,_Event,0,TRef) ->
    cancel_timer(TRef),
    {error, timedout};

wait_(Hci,Opcode,Event,Try,TRef) ->
    ok = bt_hci:select(Hci, read),
    receive
	{select,Hci,undefined,ready_input} ->
	    response_(Hci,Opcode,Event,Try,TRef);
	{timeout,TRef,_} ->
	    cancel_select(Hci, read),
	    timeout;
	Other ->
	    io:format("hci:wait/5 got ~p\n", [Other]),
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
    

response_(Hci,Opcode,Event,Try,TRef) ->
    case bt_hci:read(Hci) of
	{ok, _Data0 = <<_:8,Evt:8,Plen:8,Packet:Plen/binary,_/binary>>} ->
	    ?dbg("Got data ~p\n", [_Data0]),
	    case Evt of
		?EVT_CMD_STATUS ->
		    ?dbg("got cmd_status\n", []),
		    case Packet of
			<<?evt_cmd_status_bin(0,_Ncmd,Opcode),R/binary>> ->
			    if Evt =/= Event ->
				    wait_(Hci,Opcode,Event,Try-1,TRef);
			       true ->
				    {ok,R}
			    end;
			<<?evt_cmd_status_bin(_Status,_Ncmd,Opcode),_R/binary>> ->
			    {error, eio};  %% other status?
			<<?evt_cmd_status_bin(_S,_Ncmd,_Opcode),_R/binary>> ->
			    wait_(Hci,Opcode,Event,Try-1,TRef)
		    end;
		?EVT_CMD_COMPLETE ->
		    ?dbg("got cmd_complete\n", []),
		    case Packet of
			<<?evt_cmd_complete_bin(_Ncmd,Opcode),R/binary>> ->
			    {ok,R};
			<<?evt_cmd_complete_bin(_Ncmd,_Opcode),_R/binary>> ->
			    wait_(Hci,Opcode,Event,Try-1,TRef)
		    end;

		?EVT_REMOTE_NAME_REQ_COMPLETE when Evt =:= Event ->
		    ?dbg("got remote_name_req_complete\n", []),
		    case Packet of
			<<?evt_remote_name_req_complete_bin(_Status,_Bdaddr,_Name)>> ->
			    %% fixme: check Bdaddr!
			    {ok,Packet}
		    end;
		?EVT_LE_META_EVENT ->
		    ?dbg("got evt_le_meta_event\n", []),
		    case Packet of
			<<?evt_le_meta_event_bin(SEvt,_D1),LePacket/binary>> ->
			    if SEvt =:= Event ->
				    {ok,LePacket};
			       true ->
				    wait_(Hci,Opcode,Event,Try-1,TRef)
			    end
		    end;
		_ ->
		    ?dbg("got event ~p\n", [Evt]),
		    {ok,Packet}
	    end;
	{ok, _Data0} ->
	    ?dbg("Got data ~p\n", [_Data0]),
            wait_(Hci,Opcode,Event,Try-1,TRef);
        Error = {error,_} ->
            Error
    end.

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
	    io:format("hci: with_socket crash: ~p\n", [Error]),
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


    

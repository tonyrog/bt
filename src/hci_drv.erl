%%% @author Tony Rogvall <tony@up13>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%     hci_drv
%%% @end
%%% Created :  5 Apr 2015 by Tony Rogvall <tony@up13>

-module(hci_drv).

-export([i/0]).
-export([open/0]).
-export([bind/2]).
-export([close/1]).
-export([send/2]).
-export([activate/1]).
-export([deactivate/1]).
-export([activate/2]).
-export([debug/2]).
-export([dev_up/2]).
-export([dev_down/2]).
-export([dev_reset/2]).  %% down,up?
-export([dev_restat/2]). %% reset statistics
-export([get_dev_list/1]).
-export([get_dev_info/2]).
-export([get_conn_list/2]).
-export([get_conn_info/3]).
-export([get_auth_info/2]).
-export([set_raw/2]).
-export([set_auth/3]).
-export([set_encrypt/3]).
-export([set_ptype/3]).
-export([set_link_policy/3]).
-export([set_link_mode/3]).
-export([set_scan/3]).
-export([set_acl_mtu/4]).
-export([set_sco_mtu/4]).
-export([block/2]).
-export([unblock/2]).

-export([set_filter/2]).
-export([get_filter/1]).
-export([set_filter_ptype/2]).
-export([clr_filter_ptype/2]).
-export([set_filter_event/2]).
-export([clr_filter_event/2]).
-export([set_filter_opcode/2]).
-export([make_filter/3]).

-include("../include/hci_drv.hrl").

%% deugging
-compile(export_all).

-define(CMD_ACTIVE,         1).
-define(CMD_DEBUG,          2).
-define(CMD_BIND,           3).
-define(CMD_GETFILTER,      4).
-define(CMD_SETFILTER,      5).

-define(CMD_HCIDEVUP,	   201).
-define(CMD_HCIDEVDOWN,	   202).
-define(CMD_HCIDEVRESET,	   203).
-define(CMD_HCIDEVRESTAT,   204).
-define(CMD_HCIGETDEVLIST,  210).
-define(CMD_HCIGETDEVINFO,  211).
-define(CMD_HCIGETCONNLIST, 212).
-define(CMD_HCIGETCONNINFO, 213).
-define(CMD_HCIGETAUTHINFO, 215).
-define(CMD_HCISETRAW,	   220).
-define(CMD_HCISETSCAN,	   221).
-define(CMD_HCISETAUTH,	   222).
-define(CMD_HCISETENCRYPT,  223).
-define(CMD_HCISETPTYPE,   224).
-define(CMD_HCISETLINKPOL,  225).
-define(CMD_HCISETLINKMODE, 226).
-define(CMD_HCISETACLMTU,   227).
-define(CMD_HCISETSCOMTU,   228).
-define(CMD_HCIBLOCKADDR,   230).
-define(CMD_HCIUNBLOCKADDR, 231).
-define(CMD_HCIINQUIRY,	   240).


-define(DLOG_DEBUG,     7).
-define(DLOG_INFO,      6).
-define(DLOG_NOTICE,    5).
-define(DLOG_WARNING,   4).
-define(DLOG_ERROR,     3).
-define(DLOG_CRITICAL,  2).
-define(DLOG_ALERT,     1).
-define(DLOG_EMERGENCY, 0).
-define(DLOG_NONE,     -1).

%% dump information about bluetooth devices
i() ->
    Hci = open(),
    case get_dev_list(Hci) of
	{ok,Devs} ->
	    lists:foreach(
	      fun({DevID,_DevFlags}) ->
		      case get_dev_info(Hci, DevID) of
			  {ok, Info} ->
			      io:format("~s\n", [format_hci_dev_info(Info)]);
			  Error ->
			      io:format("error: ~p\n", [Error])
		      end
	      end, Devs);
	Error ->
	    Error
    end.

-spec open() -> hci_socket_t().
open() ->
    Driver = "hci_drv",
    Path = code:priv_dir(bt),
    io:format("load_driver '~s' from: '~s'\n", [Driver, Path]),
    case erl_ddll:load_driver(Path, Driver) of
	ok ->
	    erlang:open_port({spawn_driver, Driver}, [binary]);
	{error,Error} ->
	    io:format("Error: ~s\n", [erl_ddll:format_error_int(Error)]),
	    erlang:error(Error)
    end.

%% Close the HCI socket
-spec close(Hci::hci_socket_t()) -> boolean().
close(Hci) when is_port(Hci) ->
    erlang:port_close(Hci).

%% Bind HCI socket to device
-spec bind(Hci::hci_socket_t(), DevID::hci_devid_t()) -> ok | {error,posix()}.

bind(Hci, DevID) when is_port(Hci), is_integer(DevID) ->
    port_call(Hci, ?CMD_BIND, <<DevID:32/signed>>).

%% Bring the device UP
-spec dev_up(Hci::hci_socket_t(), DevID::hci_devid_t()) -> ok | {error,posix()}.

dev_up(Hci,DevID) when is_port(Hci), is_integer(DevID) ->
    port_call(Hci, ?CMD_HCIDEVUP, <<DevID:32/signed>>).

%% Bring the device DOWN
-spec dev_down(Hci::hci_socket_t(), DevID::hci_devid_t()) -> ok | {error,posix()}.

dev_down(Hci,DevID) when is_port(Hci), is_integer(DevID) ->
    port_call(Hci, ?CMD_HCIDEVDOWN, <<DevID:32/signed>>).

%% Reset the device, maybe do down/up? as seen in library code elsewhere
-spec dev_reset(Hci::hci_socket_t(), DevID::hci_devid_t()) -> ok | {error,posix()}.

dev_reset(Hci,DevID) when is_port(Hci), is_integer(DevID) ->
    port_call(Hci, ?CMD_HCIDEVRESET, <<DevID:32/signed>>).

%% Reset device statistics
-spec dev_restat(Hci::hci_socket_t(), DevID::hci_devid_t()) -> ok | {error,posix()}.

dev_restat(Hci,DevID) when is_port(Hci), is_integer(DevID) ->
    port_call(Hci, ?CMD_HCIDEVRESTAT, <<DevID:32/signed>>).

%% Get list of [{device(),options()}] 
-spec get_dev_list(Hci::hci_socket_t()) ->
			  {ok,[{DevID::hci_devid_t(), Opt::integer()}]} |
			  {error,posix()}.
get_dev_list(Hci) when is_port(Hci) ->
    case port_call(Hci, ?CMD_HCIGETDEVLIST, <<>>) of
	{ok, DevList} ->
	    {ok,
	     [{DevID,DevOpt} || 
		 <<DevID:16, DevOpt:32>>
		     <= DevList  ]};
	Error ->
	    Error
    end.

%% get device info
-spec get_dev_info(Hci::hci_socket_t(), DevID::hci_devid_t()) ->
			  {ok,[#hci_dev_info{}]} | {error,posix()}.

get_dev_info(Hci, DevID) ->
    case port_call(Hci, ?CMD_HCIGETDEVINFO, <<DevID:32/signed>>) of
	{ok, Info} ->
	    {ok, decode_hci_dev_info(Info)};
	Error ->
	    Error
    end.

%% get device info
-spec get_conn_list(Hci::hci_socket_t(), DevID::hci_devid_t()) ->
			   {ok,[#hci_conn_info{}]} | {error,posix()}.
get_conn_list(Hci, DevID) ->
    case port_call(Hci, ?CMD_HCIGETCONNLIST, <<DevID:32/signed>>) of
	{ok, Data} ->
	    {ok, decode_hci_conn_list(Data)};
	Error ->
	    Error
    end.

-spec get_conn_info(Hci::hci_socket_t(), Addr::bdaddr_t(), Type::uint8_t()) ->
			   {ok,#hci_conn_info{}} | {error,posix()}.

get_conn_info(Hci, {A,B,C,D,E,F}, Type) ->
    case port_call(Hci, ?CMD_HCIGETCONNINFO, <<A,B,C,D,E,F,Type>>) of
	{ok, Info} ->
	    {ok, decode_hci_conn_info(Info)};
	Error ->
	    Error
    end.

-spec get_auth_info(Hci::hci_socket_t(), Addr::bdaddr_t()) ->
			   {ok, Type::uint8_t()} | {error,posix()}.

get_auth_info(Hci, {A,B,C,D,E,F}) ->
    port_call(Hci, ?CMD_HCIGETAUTHINFO, <<A,B,C,D,E,F>>).

%% Set raw processing?
-spec set_raw(Hci::hci_socket_t(), DevID::hci_devid_t()) -> ok | {error,posix()}.

set_raw(Hci,DevID) when is_port(Hci), is_integer(DevID) ->
    port_call(Hci, ?CMD_HCISETRAW, <<DevID:32/signed>>).

%% Enable/Disable authentication
-spec set_auth(Hci::hci_socket_t(), DevID::hci_devid_t(), DoAuth::boolean()) ->
		      ok | {error,posix()}.
set_auth(Hci,DevID,DoAuth) when is_port(Hci), is_integer(DevID),
				is_boolean(DoAuth) ->
    port_call(Hci, ?CMD_HCISETAUTH, <<DevID:32/signed,DoAuth:8>>).

%% Enable/Disable authentication
-spec set_encrypt(Hci::hci_socket_t(), DevID::hci_devid_t(), DoAuth::boolean()) ->
			 ok | {error,posix()}.
set_encrypt(Hci,DevID,DoEncrypt) when is_port(Hci), is_integer(DevID),
				      is_boolean(DoEncrypt) ->
    port_call(Hci, ?CMD_HCISETENCRYPT, <<DevID:32/signed,DoEncrypt:8>>).

%% Set device packet type
-spec set_ptype(Hci::hci_socket_t(),DevID::hci_devid_t(),PType::[string()]) ->
		       ok | {error,posix()}.
set_ptype(Hci,DevID,PType0) when is_port(Hci), is_integer(DevID),
				 is_list(PType0) ->
    PType = set_bits(PType0, kv_pkt_ptype()),
    port_call(Hci, ?CMD_HCISETPTYPE, <<DevID:32/signed,PType:32>>).

-spec set_link_policy(Hci::hci_socket_t(),DevID::hci_devid_t(),Pol::string()) ->
			     ok | {error,posix()}.
set_link_policy(Hci,DevID,Pol0) when is_port(Hci), is_integer(DevID) ->
    LinkPolicy = find_enum_value(Pol0, kv_link_policy()),
    port_call(Hci, ?CMD_HCISETLINKPOL, <<DevID:32/signed,LinkPolicy:32>>).

-spec set_link_mode(Hci::hci_socket_t(),DevID::hci_devid_t(),Mode::string()) ->
			     ok | {error,posix()}.
set_link_mode(Hci,DevID,Mode0) when is_port(Hci), is_integer(DevID) ->
    LinkMode = find_enum_value(Mode0, kv_link_mode()),
    port_call(Hci, ?CMD_HCISETLINKMODE, <<DevID:32/signed,LinkMode:32>>).

-spec set_scan(Hci::hci_socket_t(),DevID::hci_devid_t(),Scan::string()) ->
		      ok | {error,posix()}.
set_scan(Hci,DevID,Scan0) ->
    Scan = find_enum_value(Scan0, kv_scan()),
    port_call(Hci, ?CMD_HCISETSCAN, <<DevID:32/signed,Scan:32>>).

-spec set_acl_mtu(Hci::hci_socket_t(),DevID::hci_devid_t(),
		  Mtu::integer(),Mpkt::integer()) ->
			 ok | {error,posix()}.
set_acl_mtu(Hci,DevID,Mtu,Mpkt) ->
    port_call(Hci, ?CMD_HCISETACLMTU, <<DevID:32/signed,Mtu:16,Mpkt:16>>).

-spec set_sco_mtu(Hci::hci_socket_t(),DevID::hci_devid_t(),
		  Mtu::integer(),Mpkt::integer()) ->
			 ok | {error,posix()}.
set_sco_mtu(Hci,DevID,Mtu,Mpkt) ->
    port_call(Hci, ?CMD_HCISETSCOMTU, <<DevID:32/signed,Mtu:16,Mpkt:16>>).

%% The Hci socket must be bound before this operation
-spec block(Hci::hci_socket_t(), Addr::bdaddr_t()) ->
		   ok | {error,posix()}.
block(Hci, {A,B,C,D,E,F}) ->
    port_call(Hci, ?CMD_HCIBLOCKADDR, <<A,B,C,D,E,F>>).

%% The Hci socket must be bound before this operation
-spec unblock(Hci::hci_socket_t(), Addr:: all | bdaddr_t()) ->
		     ok | {error,posix()}.
unblock(Hci, all) ->
    unblock(Hci, {0,0,0,0,0,0});
unblock(Hci, {A,B,C,D,E,F}) ->
    port_call(Hci, ?CMD_HCIUNBLOCKADDR, <<A,B,C,D,E,F>>).

-spec deactivate(Hci::hci_socket_t()) -> ok | {error,posix()}.

deactivate(Hci) when is_port(Hci) ->
    activate(Hci, 0).    

-spec activate(Hci::hci_socket_t()) -> ok | {error,posix()}.
activate(Hci) when is_port(Hci) ->
    activate(Hci, -1).

-spec activate(Hci::hci_socket_t(),N::integer()) -> ok | {error,posix()}.
activate(Hci, N) when is_port(Hci),
		      is_integer(N), N >= -1, N < 16#7fffffff ->
    port_call(Hci, ?CMD_ACTIVE, <<N:32>>).

-spec debug(Hci::hci_socket_t(), Level::level_t()) -> ok.
debug(Hci,Level) when is_port(Hci), is_atom(Level) ->
    L = level(Level),
    port_call(Hci, ?CMD_DEBUG, <<L:32>>).

set_filter_ptype(T, F = #hci_filter { type_mask = M}) ->
    Bit = if T =:= ?HCI_VENDOR_PKT -> 1;
	      true ->  1 bsl (T band ?HCI_FLT_TYPE_BITS)
	   end,
    F#hci_filter { type_mask = M bor Bit }.

clr_filter_ptype(T, F = #hci_filter { type_mask = M}) ->
    Bit = if T =:= ?HCI_VENDOR_PKT -> 1;
	      true ->  1 bsl (T band ?HCI_FLT_TYPE_BITS)
	   end,
    F#hci_filter { type_mask = M band (bnot Bit) }.

set_filter_event(E, F = #hci_filter { event_mask = M}) ->
    Bit = 1 bsl (E band ?HCI_FLT_EVENT_BITS),
    F#hci_filter { event_mask = M bor Bit }.

clr_filter_event(E, F = #hci_filter { event_mask = M}) ->
    Bit = 1 bsl (E band ?HCI_FLT_EVENT_BITS),
    F#hci_filter { event_mask = M band (bnot Bit) }.

set_filter_opcode(Opcode,  F = #hci_filter { }) ->
    F#hci_filter { opcode = Opcode }.

make_filter(Opcode, Ts, Es) when is_integer(Opcode),
				 is_list(Ts),
				 is_list(Es) ->
    Type_mask = make_bits(Ts, 0) band ?HCI_FLT_TYPE_BITS,
    Event_mask = make_bits(Es, 0) band ?HCI_FLT_EVENT_BITS,
    #hci_filter { type_mask = Type_mask,
		  event_mask = Event_mask,
		  opcode = Opcode }.

make_bits([255|Ns], Bits) ->  %% ?HCI_VENDOR_PKT! -> 1
    make_bits(Ns, Bits bor 1);
make_bits([Nr|Ns], Bits) when is_integer(Nr), Nr >= 0 ->
    make_bits(Ns, Bits bor (1 bsl Nr));
make_bits([], Bits) ->
    Bits.


-spec set_filter(Hci::hci_socket_t(), Filter::#hci_filter{}) ->
			ok | {error,posix()}.
set_filter(Hci,Filter) when is_port(Hci), is_record(Filter, hci_filter) ->
    port_call(Hci, ?CMD_SETFILTER, encode_hci_filter(Filter)).

-spec get_filter(Hci::hci_socket_t()) ->
			{ok, Filter::#hci_filter{}} | {error,posix()}.

get_filter(Hci) when is_port(Hci) ->
    case port_call(Hci, ?CMD_GETFILTER, <<>>) of
	{ok, Data} -> {ok, decode_hci_filter(Data)};
	Error -> Error
    end.

-spec send(Hci::hci_socket_t(), Command::iolist()) ->
		  boolean().
send(Hci, Command) ->
    erlang:port_command(Hci, Command).

port_call(Hci, Cmd, Data) ->
    case erlang:port_control(Hci, Cmd, Data) of
	<<0>> ->
	    ok;
	<<255,E/binary>> -> 
	    {error, erlang:binary_to_atom(E, latin1)};
	<<254,E/binary>> -> 
	    {error, binary_to_list(E)};
	<<1,Y:8>> -> {ok,Y};
	<<1,Y:16/native-unsigned>> -> {ok, Y};
	<<1,Y:32/native-unsigned>> -> {ok, Y};
	<<1,Y:64/native-unsigned>> -> {ok, Y};
	<<2,X:32/native-unsigned,Y:32/native-unsigned>> -> {ok,{X,Y}};
	<<3,X/binary>> -> {ok,X};
	<<4,X/binary>> -> {ok,binary_to_list(X)}
    end.
	
%% convert symbolic to numeric level
level(true)  -> ?DLOG_DEBUG;
level(false) -> ?DLOG_NONE;
level(debug) -> ?DLOG_DEBUG;
level(info)  -> ?DLOG_INFO;
level(notice) -> ?DLOG_NOTICE;
level(warning) -> ?DLOG_WARNING;
level(error) -> ?DLOG_ERROR;
level(critical) -> ?DLOG_CRITICAL;
level(alert) -> ?DLOG_ALERT;
level(emergency) -> ?DLOG_EMERGENCY;
level(none) -> ?DLOG_NONE.

%%
decode_hci_dev_info(
  <<Dev_id:16, Name0:8/binary, A,B,C,D,E,F,
    Flags:32,  Type:8, Features:8/binary,
    Pkt_type:32, Link_policy:32, Link_mode:32, 
    Acl_mtu:16, Acl_pkts:16, 
    Sco_mtu:16, Sco_pkts:16,
    Stat/binary>>) ->
    Name = c_string(Name0),
    BdAddr = {A,B,C,D,E,F},
    S = decode_hci_dev_stats(Stat),
    #hci_dev_info {
       dev_id = Dev_id,
       name = Name,
       bdaddr = BdAddr,
       flags = Flags,
       type = Type,
       features = Features,
       pkt_type = Pkt_type,
       link_policy = Link_policy,
       link_mode = Link_mode,
       acl_mtu = Acl_mtu,
       acl_pkts = Acl_pkts,
       sco_mtu = Sco_mtu,
       sco_pkts = Sco_pkts,
       stat = S
      }.

%% 10*4 = 40 bytes
decode_hci_dev_stats(
  << Err_rx:32, Err_tx:32,
     Cmd_tx:32, Evt_rx:32,
     Acl_tx:32, Acl_rx:32,
     Sco_tx:32, Sco_rx:32,
     Byte_rx:32,  Byte_tx:32>>) ->
    #hci_dev_stats {
       err_rx = Err_rx,
       err_tx = Err_tx,
       cmd_tx = Cmd_tx,
       evt_rx = Evt_rx,
       acl_tx = Acl_tx,
       acl_rx = Acl_rx,
       sco_tx = Sco_tx,
       sco_rx = Sco_rx,
       byte_rx = Byte_rx,
       byte_tx = Byte_tx }.

decode_hci_conn_info(<<Handle:16,
		       A,B,C,D,E,F,
		       Type:8, Out:8,
		       State:16, Link_mode:32>>) ->
    #hci_conn_info { handle=Handle, bdaddr={A,B,C,D,E,F},
		     type=Type, out=Out,
		     state=State, link_mode = Link_mode }.

decode_hci_conn_list(<<Item:16/binary, List/binary>>) ->
    [decode_hci_conn_info(Item) | decode_hci_conn_list(List)];
decode_hci_conn_list(<<>>) ->
    [].

encode_hci_filter(#hci_filter { type_mask = Type_mask,
				event_mask = Event_mask,
				opcode = Opcode }) ->
    Event0 = Event_mask band 16#ffffffff,
    Event1 = (Event_mask bsr 32) band 16#ffffffff,
    <<Type_mask:32, Event0:32, Event1:32, Opcode:16>>.

decode_hci_filter(<<Type_mask:32, Event0:32, Event1:32, Opcode:16>>) ->
    Event_mask = Event0 + (Event1 bsl 32),
    #hci_filter { type_mask = Type_mask,
		  event_mask = Event_mask,
		  opcode = Opcode }.

format_hci_dev_info(#hci_dev_info {
		      dev_id = _Dev_id,
		      name = Name,
		      bdaddr = BdAddr,
		      flags = Flags,
		      type = Type,
		      features = _Features,
		      pkt_type = _Pkt_type,
		      link_policy = _Link_policy,
		      link_mode = _Link_mode,
		      acl_mtu = Acl_mtu,
		      acl_pkts = Acl_pkts,
		      sco_mtu = Sco_mtu,
		      sco_pkts = Sco_pkts,
		      stat = S}) ->
    %% what field?
    Bus = find_enum_name(Type band 16#0f, kv_dev_bus(), "UNKNOWN"), 
    Type1 = find_enum_name((Type band 16#30) bsr 4, kv_dev_type(), "UNKNOWN"),
    io_lib:format(
      "~s:	Type: ~s  Bus: ~s\n"
      "\tBD Address: ~s  ACL MTU: ~w:~w  SCO MTU: ~w:~w\n"
      "\t~s\n"
      "~s\n",
      [Name, Type1, Bus,
       bt:format_address(BdAddr),
       Acl_mtu, Acl_pkts, Sco_mtu, Sco_pkts,
       format_bits(Flags, kv_hci_dev_info_flags()),
       format_hci_dev_stats(S)]).
    

format_hci_dev_stats(#hci_dev_stats {
			 err_rx = Err_rx,
			 err_tx = Err_tx,
			 cmd_tx = Cmd_tx,
			 evt_rx = Evt_rx,
			 acl_tx = Acl_tx,
			 acl_rx = Acl_rx,
			 sco_tx = Sco_tx,
			 sco_rx = Sco_rx,
			 byte_rx = Byte_rx,
			 byte_tx = Byte_tx }) ->
    io_lib:format(
      "\tRX bytes:~w acl:~w sco:~w events:~w errors:~w\n"
      "\tTX bytes:~w acl:~w sco:~w commands:~w errors:~w",
      [Byte_rx, Acl_rx, Sco_rx, Evt_rx, Err_rx,
       Byte_tx, Acl_tx, Sco_tx, Cmd_tx, Err_tx]).

format_enum(Value, Kv) ->
    case lists:keyfind(Value, 2, Kv) of
        false -> "";
        {Name, Value} -> Name
    end.

format_bits(Value, KvList) ->
    format_bits_(Value, KvList, []).

format_bits_(0, _Kv, Acc) ->
    string:join(lists:reverse(Acc), ",");
format_bits_(Value, [{Key,Bits}|Kv], Acc) ->
    if Value band Bits =:= Bits ->
            format_bits_(Value band (bnot Bits),Kv,[Key|Acc]);
       true ->
            format_bits_(Value, Kv, Acc)
    end;
format_bits_(Value, [], Acc) ->
    Acc1 = [integer_to_list(Value) | Acc],
    string:join(lists:reverse(Acc1), ",").

%% given a list of atoms/strings/integers
%% build a bitmask from the values

set_bits(Names, Flags) ->
    set_bits(Names, Flags, 0).

set_bits([Name|Names], Flags, Acc) when is_atom(Name) ->
    case lists:keyfind(atom_to_list(Name), 1, Flags) of
	false ->
	    exit(badarg);
	{_, Bit} ->
	    set_bits(Names, Flags, Bit bor Acc)
    end;
set_bits([Name|Names], Flags, Acc) when is_list(Name) ->
    case lists:keyfind(Name, 1, Flags) of
	false ->
	    exit(badarg);
	{_, Bit} ->
	    set_bits(Names, Flags, Bit bor Acc)
    end;
set_bits([Bits|Names], Flags, Acc) when is_integer(Bits) ->
    set_bits(Names, Flags, Bits bor Acc);
set_bits([], _Flags, Acc) ->
    Acc.

find_enum_name(Value, Flags, Default) when is_integer(Value) ->
    case lists:keyfind(Value, 2, Flags) of
	false -> Default;
	{Name, _Value} -> Name
    end.

find_enum_name(Value, Flags) ->
    case find_enum_name(Value, Flags, undefined) of
	undefined -> erlang:error(badarg);
	Name -> Name
    end.
	    
find_enum_value(Name, Flags, Default) when is_atom(Name) ->
    case lists:keyfind(atom_to_list(Name), 1, Flags) of
	false -> Default;
	{_, Value} -> Value
    end;
find_enum_value(Name, Flags, Default) when is_list(Name) ->
    case lists:keyfind(Name, 1, Flags) of
	false -> Default;
	{_, Value} -> Value
    end.

find_enum_value(Name, Flags) ->
    case find_enum_value(Name, Flags, undefined) of
	undefined -> erlang:error(badarg);
	Value -> Value
    end.

c_string(Data) when is_binary(Data) ->
    c_string_(binary_to_list(Data));
c_string(Data) when is_list(Data) ->
    c_string_(Data).

c_string_([0|_]) -> [];
c_string_([H|T]) -> [H|c_string_(T)].

kv_dev_type() ->
[
 {"BR/EDR",      ?HCI_BREDR},
 {"AMP",         ?HCI_AMP}
].

kv_dev_bus() ->
[
 {"VIRTUAL",?HCI_VIRTUAL},
 {"USB", ?HCI_USB },
 {"PCCARD", ?HCI_PCCARD },
 {"UART", ?HCI_UART },
 {"RS232",?HCI_RS232},
 {"PCI", ?HCI_PCI},
 {"SDIO", ?HCI_SDIO}
].
    

%% scan modes
kv_scan() ->
[
 { "OFF",       ?SCAN_DISABLED },
 { "ISCAN",     ?SCAN_INQUIRY },
 { "PSCAN",     ?SCAN_PAGE },
 { "PISCAN",    ?SCAN_PAGE bor ?SCAN_INQUIRY }
].

%% HCI device flags
kv_hci_dev_info_flags() ->
[
 { "UP",      (1 bsl ?HCI_UP) },
 { "INIT",    (1 bsl ?HCI_INIT) },
 { "RUNNING", (1 bsl ?HCI_RUNNING) },
 { "PSCAN",   (1 bsl ?HCI_PSCAN) },
 { "ISCAN",   (1 bsl ?HCI_ISCAN) },
 { "AUTH",    (1 bsl ?HCI_AUTH) },
 { "ENCRYPT", (1 bsl ?HCI_ENCRYPT) },
 { "INQUIRY", (1 bsl ?HCI_INQUIRY) },
 { "RAW",     (1 bsl ?HCI_RAW) }
].

kv_pkt_ptype() ->
[
 { "DM1",   ?HCI_DM1  },
 { "DM3",   ?HCI_DM3  },
 { "DM5",   ?HCI_DM5  },
 { "DH1",   ?HCI_DH1  },
 { "DH3",   ?HCI_DH3  },
 { "DH5",   ?HCI_DH5  },
 { "HV1",   ?HCI_HV1  },
 { "HV2",   ?HCI_HV2  },
 { "HV3",   ?HCI_HV3  },
 { "2-DH1", ?HCI_2DH1 },
 { "2-DH3", ?HCI_2DH3 },
 { "2-DH5", ?HCI_2DH5 },
 { "3-DH1", ?HCI_3DH1 },
 { "3-DH3", ?HCI_3DH3 },
 { "3-DH5", ?HCI_3DH5 }
].

kv_sco_ptype() ->
[
 { "HV1",   16#0001   },
 { "HV2",   16#0002   },
 { "HV3",   16#0004   },
 { "EV3",   ?HCI_EV3  },
 { "EV4",   ?HCI_EV4  },
 { "EV5",   ?HCI_EV5  },
 { "2-EV3", ?HCI_2EV3 },
 { "2-EV5", ?HCI_2EV5 },
 { "3-EV3", ?HCI_3EV3 },
 { "3-EV5", ?HCI_3EV5 }
].

kv_link_policy() ->
[
 { "NONE",       0               },
 { "RSWITCH",    ?HCI_LP_RSWITCH  },
 { "HOLD",       ?HCI_LP_HOLD     },
 { "SNIFF",      ?HCI_LP_SNIFF    },
 { "PARK",       ?HCI_LP_PARK     }
].

kv_link_mode() ->
[
 { "NONE",       0               },
 { "ACCEPT",     ?HCI_LM_ACCEPT   },
 { "MASTER",     ?HCI_LM_MASTER   },
 { "AUTH",       ?HCI_LM_AUTH     },
 { "ENCRYPT",    ?HCI_LM_ENCRYPT  },
 { "TRUSTED",    ?HCI_LM_TRUSTED  },
 { "RELIABLE",   ?HCI_LM_RELIABLE },
 { "SECURE",     ?HCI_LM_SECURE   }
].

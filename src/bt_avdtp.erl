%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    AVCT
%%% @end
%%% Created : 18 Jan 2022 by Tony Rogvall <tony@rogvall.se>

-module(bt_avdtp).

-export([discover/2]).
-export([get_capabilities/3]).
-export([get_all_capabilities/3]).
-export([set_configuration/5]).
-export([get_configuration/3]).
-export([reconfigure/5]).
-export([open/3]).
-export([start/3]).
-export([close/3]).
-export([suspend/3]).
-export([abort/3]).
-export([security_control/4]).
-export([delay_report/4]).

-export([command/4]).

-export([test/1, test_jbl/0, test_jabra/0]).

-include("../include/avdtp.hrl").
%%
%% struct discover_resp {
%% 	struct seid_info seps[0];
%% } __attribute__ ((packed));
%%
%% struct getcap_resp {
%% 	uint8_t caps[0];
%% } __attribute__ ((packed));
%%
%% struct start_req {
%% 	struct seid first_seid;
%% 	struct seid other_seids[0];
%% } __attribute__ ((packed));
%%
%% struct suspend_req {
%% 	struct seid first_seid;
%% 	struct seid other_seids[0];
%% } __attribute__ ((packed));
%%
%% struct seid_rej {
%% 	uint8_t error;
%% } __attribute__ ((packed));
%%
%% struct conf_rej {
%% 	uint8_t category;
%% 	uint8_t error;
%% } __attribute__ ((packed));
%%
%% struct seid_req {
%% 	uint8_t acp_seid:6;
%% 	uint8_t rfa0:2;
%% } __attribute__ ((packed));
%%
%% struct setconf_req {
%% 	uint8_t acp_seid:6;
%% 	uint8_t rfa0:2;
%% 	uint8_t int_seid:6;
%% 	uint8_t rfa1:2;
%% 	uint8_t caps[0];
%% } __attribute__ ((packed));
%%
%% struct stream_rej {
%% 	uint8_t acp_seid:6;
%% 	uint8_t rfa0:2;
%% 	uint8_t error;
%% } __attribute__ ((packed));
%%
%% struct reconf_req {
%% 	uint8_t acp_seid:6;
%% 	uint8_t rfa0:2;
%% 	uint8_t serv_cap;
%% 	uint8_t serv_cap_len;
%% 	uint8_t caps[0];
%% } __attribute__ ((packed));
%%
%% struct delay_req {
%% 	uint8_t acp_seid:6;
%% 	uint8_t rfa0:2;
%% 	uint16_t delay;
%% } __attribute__ ((packed));

test_jbl() ->
    test("FC:A8:9A:A9:10:30").

test_jabra() ->
    test("50:C2:ED:5A:06:ED").

test(Address) ->
    {ok,PortList} = bt_sdp:protocol_port(Address, "AudioSink"),
    {value,{_,L2Port}} = lists:keysearch("L2CAP", 1, PortList),
    {value,{_,_AVDTP}} = lists:keysearch("AVDTP", 1, PortList),    
    {ok, S} = l2cap:connect(Address, L2Port),
    INT = 1,
    Trans = 2,
    io:format("INT = ~w\n", [INT]),
    {ok,[Info|_]} = discover(S, Trans+1),
    io:format("Into = ~w\n", [Info]),
    ACP = Info#seid_info.seid,
    io:format("ACP = ~w\n", [ACP]),
%%    {ok,Caps} = get_all_capabilities(S, Trans+2, ACP),
%%    {ok,Caps} = get_capabilities(S, Trans+2, ACP),
%%    io:format("Caps = ~w\n", [Caps]),
%%    [A,B|_] = Caps,
    %% {ok,SetConfigRes} = set_configuration(S, Trans+3, ACP, INT, [A,B]),
    %% io:format("SetConfigRes = ~w\n", [SetConfigRes]),
    {ok,OpenRes} = open(S, Trans+4, ACP),
    io:format("OpenRes = ~w\n", [OpenRes]),
    {ok,StartRes} = start(S, Trans+5, ACP),
    io:format("StartRes = ~w\n", [StartRes]),
%%    {ok,SuspendRes} = suspend(S, Trans+6, Info#seid_info.seid),
%%    {ok,CloseRes} = close(S, Trans+6, <<>>),
    l2cap:close(S),
    [{info,Info},
     {open,OpenRes},
     {start,StartRes}
     %%{suspend,SuspendRes},
     %%{close,CloseRes}
    ].

discover(Socket, Trans) ->
    case command(Socket, Trans, ?AVDTP_DISCOVER, <<>>) of
	{ok,Reply} ->
	    {ok,[#seid_info{seid=Seid,inuse=Inuse,
			    media_type=MediaType,type=Type} ||
		    ?SEID_INFO(Seid,Inuse,_,MediaType,Type,_) <=
			Reply]};
	Error ->
	    Error
    end.

get_capabilities(Socket, Trans, ACP) ->
    case command(Socket, Trans, ?AVDTP_GET_CAPABILITIES, <<ACP:6, 0:2>>) of
	{ok, Caps} ->
	    {ok, decode_capabilities(Caps)};
	Error ->
	    Error
    end.

get_all_capabilities(Socket, Trans, ACP) ->
    case command(Socket, Trans, ?AVDTP_GET_ALL_CAPABILITIES, <<ACP:6, 0:2>>) of
	{ok, Caps} ->
	    {ok, decode_capabilities(Caps)};
	Error ->
	    Error
    end.	    
	    
set_configuration(Socket, Trans, ACP, INT, Caps) ->
    CapsBin = encode_capabilities(Caps),
    Data = <<ACP:6, 0:2, INT:6, 0:2, CapsBin/binary>>,
    io:format("set_configuration: data=~w\n", [Data]),
    command(Socket, Trans, ?AVDTP_SET_CONFIGURATION, Data).

get_configuration(Socket, Trans, ACP) ->
    Data = <<ACP:6, 0:2>>,
    command(Socket, Trans, ?AVDTP_GET_CONFIGURATION, Data).

reconfigure(Socket, Trans, ACP, INT, Caps) ->
    Data = <<ACP:6, 0:2, INT:6, 0:2, Caps/binary>>,    
    command(Socket, Trans, ?AVDTP_RECONFIGURE, Data).

open(Socket, Trans, ACP) ->
    command(Socket, Trans,  ?AVDTP_OPEN, <<ACP:6, 0:2>>).

start(Socket, Trans, ACP) when is_integer(ACP) ->
    command(Socket, Trans, ?AVDTP_START, <<ACP:6, 0:2>>);
start(Socket, Trans, List=[_ACP|_Other]) ->
    command(Socket, Trans, ?AVDTP_START,
	    << << ID:6, 0:2>> || ID <- List >>).

close(Socket, Trans, ACP) ->
    command(Socket, Trans, ?AVDTP_CLOSE, <<ACP:6, 0:2>>).

suspend(Socket, Trans, ACP) when is_integer(ACP) ->
    command(Socket, Trans, ?AVDTP_SUSPEND, <<ACP:6, 0:2>>);
suspend(Socket, Trans, List=[_ACP|_Other]) ->
    command(Socket, Trans, ?AVDTP_SUSPEND, 
	    << << ID:6, 0:2>> || ID <- List >>).

abort(Socket, Trans, ACP) ->
    command(Socket, Trans, ?AVDTP_ABORT, <<ACP:6, 0:2>>).

security_control(Socket, Trans, ACP, Data) ->
    command(Socket, Trans, ?AVDTP_SECURITY_CONTROL,
	    <<ACP:6, 0:2, Data/binary>>).

delay_report(Socket, Trans, ACP, Delay) when is_integer(Delay) ->
    command(Socket, Trans, ?AVDTP_DELAY_REPORT, 
	    <<ACP:6, 0:2, Delay:16>>).


command(Socket, Trans, Sid, Data) ->
    l2cap:send(Socket, ?AVDTP(Trans, ?AVDTP_MSG_TYPE_COMMAND, 
			      0, Sid, Data)),
    response(Socket, Trans, Sid).

response(Socket, Trans, Sid) ->
    case l2cap:recv(Socket, 1000) of
	E = {error,_} -> E;
	{ok, ?AVDTP(Trans,?AVDTP_MSG_TYPE_ACCEPT, _, Sid, Data)} ->
	    io:format("Accept: Trans=~w, sid=~w, size=~w, data=~w\n", 
		      [Trans, Sid, byte_size(Data), Data]),
	    {ok, Data};
	{ok, ?AVDTP_START(Trans,?AVDTP_MSG_TYPE_ACCEPT,N,0,Sid,Data)} ->
	    io:format("Start: N=~w, size=~w\n", [N, byte_size(Data)]),
	    response_loop(Socket, Trans, Sid, N, N-1, [Data]);
	{ok, ?AVDTP(Trans,?AVDTP_MSG_TYPE_REJECT, _, Sid, Data)} ->
	    {error, {reject, Data}};
	{ok, ?AVDTP(Trans,?AVDTP_MSG_TYPE_GEN_REJECT, _, Sid, Data)} ->
	    {error, {gen_reject, Data}};
	{ok, Packet} ->
	    io:format("unexpected data ~p\n", [Packet]),
	    {error, Packet}
    end.

response_loop(Socket, Trans, Sid, N, I, Acc) ->
    case l2cap:recv(Socket, 1000) of
	E = {error,_} -> E;
	{ok, ?AVDTP_END(Trans,?AVDTP_MSG_TYPE_ACCEPT, _, Sid, Data)} ->
	    %% end packet is less or equal to start/continue packets in size!
	    io:format("End: N=~w, I=~w, size=~w\n", [N, I, byte_size(Data)]),
	    {ok, list_to_binary(lists:reverse([Data|Acc]))};
	{ok, ?AVDTP_CONTINUE(Trans,?AVDTP_MSG_TYPE_ACCEPT,_,Sid,Data)} ->
	    %% continue packets are equal in size
	    io:format("Continue: N=~w, I=~w, size=~w\n",[N,I,byte_size(Data)]),
	    response_loop(Socket, Trans, Sid, N, I-1, [Data|Acc]);
	{ok, ?AVDTP(Trans,?AVDTP_MSG_TYPE_REJECT, _, Sid, Data)} ->
	    io:format("Reject: N=~w, I=~w\n", [N, I]),
	    {error, {reject, Data}};
	{ok, ?AVDTP(Trans,?AVDTP_MSG_TYPE_GEN_REJECT, _, Sid, Data)} ->
	    io:format("GenReject: N=~w, I=~w\n", [N, I]),
	    {error, {gen_reject, Data}};
	{ok, Packet} ->
	    io:format("unexpected data ~p\n", [Packet]),
	    {error, Packet}
    end.


decode_capabilities(Bin) ->
    [{decode_sc(SC),Elem} || <<SC:8, LOSC:8, Elem:LOSC/binary>> <= Bin ].

encode_capabilities(Capa) ->
    << <<(encode_sc(SC)):8, (byte_size(Elem)):8, Elem/binary >> ||
	{SC,Elem} <- Capa >>.

decode_sc(1) -> media_transport;
decode_sc(2) -> reporting;
decode_sc(3) -> recovery;
decode_sc(4) -> content_protection;
decode_sc(5) -> header_protection;
decode_sc(6) -> multiplexing;
decode_sc(7) -> media_codec;
decode_sc(8) -> delay_reporting;
decode_sc(N) -> {rfd,N}.

encode_sc(media_transport) -> 1;
encode_sc(reporting) -> 2;
encode_sc(recovery) -> 3;
encode_sc(content_protection) -> 4;
encode_sc(header_protection) -> 5;
encode_sc(multiplexing) -> 6;
encode_sc(media_codec) -> 7;
encode_sc(delay_reporting) -> 8;
encode_sc({rfd,N}) -> N;
encode_sc(N) when is_integer(N) -> N.





    

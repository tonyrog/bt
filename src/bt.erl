%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2006 - 2014, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% File    : bt.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : Bluetooth utilities
%%% Created : 31 Jan 2006 by Tony Rogvall <tony@iMac.local>

-module(bt).

-export([i/0, i/1]).
-export([s/1, s/2]).
-export([scan/1, scan/3]).
-export([getaddr/1, getaddr_by_name/1]).
-export([service_info/1, service_info/2]).
-export([rfcomm_channel/2]).
-export([decode_service/1]).


-import(lists, [foreach/2, map/2]).

-include("../include/bt.hrl").
-include("../include/uuid.hrl").
-include("../include/sdp.hrl").

%%
%% Dump device information
%%
i() ->
    {ok, Devices} = bt_drv:devices(),
    foreach(fun(A) -> i(A) end, Devices).

i(paired) ->
    {ok, Devices} = bt_drv:paired_devices(),
    foreach(fun(A) -> i(A) end, Devices);
i(favorite) ->
    {ok, Devices} = bt_drv:favorite_devices(),
    foreach(fun(A) -> i(A) end, Devices);
i(BtAddr) ->
    {ok,A} = getaddr(BtAddr),
    case bt_drv:device_info(A, [inquiry,update]) of
	{ok, [{inquiry,?never},{update,_}]} ->
	    io:format("Address: ~s\n", [format_address(A)]);
	{ok, [{inquiry,_InQuiry},{update,Update}]}->
	    io:format("Address: ~s\n", [format_address(A)]),
	    {ok,DevInfo} = bt_drv:device_info(A,[name,
						 is_paired,
						 is_favorite,
						 is_connected,
						 class]),
	    foreach(fun({class,Value}) ->
			    {Service,Major,Minor} = bt_drv:decode_class(Value),
			    io:format("  major: ~p\n", [Major]),
			    io:format("  minor: ~p\n", [Minor]),
			    io:format("service: ~p\n", [Service]);
		       ({What,Value}) ->
			    io:format("  ~p: ~p\n", [What,Value])
		    end, DevInfo),
	    if Update == ?never ->
		    ok;
	       true ->
		    {ok,SdpInfo} = bt_drv:service_info(A),
		    io:format("  Profiles:"),
		    foreach(
		      fun(Service) ->
			      As = map(fun(A1) -> bt_sdp:decode(A1) end, Service),
			      case lists:keysearch(256, 1, As) of
				  false -> ok;
				  {value,{_,{text,Name}}} -> io:format(" ~s,", [Name])
			      end
		      end, SdpInfo),
		    io:format("\n")
	    end
    end.
%%
%% Dump service information
%% s(Addr [UUID])
%%
%% Addr is either the Bluetooth address (as string or tuple) or
%%  the name of the device (a bit slow)
%% UUID is UUID16 | UUID32 | UUID128 | Symbolic-Name
%%
s(Addr) ->
    case bt_drv:service_info(Addr,<<>>) of
	{ok, Info} when is_list(Info) ->
	    foreach(
	      fun(Service) when is_list(Service) ->
		      s_serv(Service),
		      io:format("\n")
	      end, Info);
	Error ->
	    Error
    end.

s(Addr, UUID) when is_binary(UUID), size(UUID) > 0 ->
    case bt_drv:service_info(Addr,UUID) of
	{ok, Service} ->
	    s_serv(Service);
	Error ->
	    Error
    end;
s(Addr, Name) when is_list(Name) ->
    UUID = bt_sdp:string_to_uuid(Name),
    case bt_drv:service_info(Addr,UUID) of
	{ok, Service} ->
	    s_serv(Service);
	Error ->
	    Error
    end.

s_serv(Attributes=[A1|_]) when is_binary(A1) ->
    Attrs0 = map(fun(A) -> bt_sdp:decode(A) end, Attributes),
    LanguageBase = get_language_base(Attrs0, 16#0100),
    Name = case lists:keysearch(?ATTR_ServiceName(LanguageBase), 1, Attrs0) of
	       false -> "Unknown";
	       {value,{_,{text,Nm}}} -> Nm
	   end,
    io:format("Service Name: ~s\n", [Name]),
    Attrs = delete_attributes(Attrs0,[?ATTR_ServiceName(LanguageBase),
				      ?ATTR_LanguageBaseAttributeIDList]),
    foreach(
      fun ({ID,Value}) ->
	      io:format("  ~s: ~s\n", [bt_sdp:attribute_to_string(ID),
				       bt_sdp:value_to_string(Value)])
      end, Attrs).

delete_attributes(Attrs, [ID | IDs]) ->
    delete_attributes(lists:keydelete(ID, 1, Attrs), IDs);
delete_attributes(Attrs, []) -> Attrs.

get_language_base(Attrs, Default) ->
    case lists:keysearch(?ATTR_LanguageBaseAttributeIDList, 1, Attrs) of
	{value,{_,{sequence,[{uint16,_Lang},
			     {uint16,_Encoding},
			     {uint16,Base} | _]}}} ->
	    Base;  %% FIXME return a list of Base's
	_ -> Default
    end.

%%
%% Decode all services on a device
%%

service_info(Addr) ->
    case bt_drv:service_info(Addr,<<>>) of
	{ok, ServiceList} when is_list(hd(ServiceList)) ->
	    map(fun(Attributes) ->
			decode_service(Attributes)
		end, ServiceList);
	Error ->
	    Error
    end.
%%
%% 
%%
service_info(Addr, Service) when is_list(Service) ->
    service_info(Addr, bt_sdp:string_to_uuid(Service));
service_info(Addr, UUID) when is_binary(UUID), size(UUID) > 0 ->
    case bt_drv:service_info(Addr,UUID) of
	{ok, Service} when is_binary(hd(Service)) ->
	    decode_service(Service);
	Error ->
	    Error
    end.
%%
%% Extract rfcomm channel for the given service 
%%
rfcomm_channel(Addr, Service) when is_list(Service) ->
    rfcomm_channel(Addr, bt_sdp:string_to_uuid(Service));
rfcomm_channel(Addr, UUID) when is_binary(UUID), size(UUID) > 0 ->
    case bt_drv:service_info(Addr,UUID) of
	{ok, Service} when is_binary(hd(Service)) ->
	    As = decode_service(Service),
	    case lists:keysearch(?ATTR_ProtocolDescriptorList, 1, As) of
		{value,{_, {sequence,[{sequence,["L2CAP"]},
				      {sequence,["RFCOMM",Channel]}|_]}}} ->
		    {ok,Channel};
		_ ->
		    {error, no_channel}
	    end;
	Error -> Error
    end.
    
%%
%% Decode a binary encoded SDP list:
%% [ binary(<attribute><value>) ] in SDP format
%%    

decode_service(Attributes) when is_binary(hd(Attributes)) ->
    Attrs = map(fun(A) -> bt_sdp:decode(A) end, Attributes),
    map(fun({ID,Value}) -> 
		{ID, bt_sdp:decode_sdp_value(Value)}
	end, Attrs).

%%
%% getaddr(Name) -> {ok,Addr} | {error, Reason}
%%
%% Convert address into a bluetooth address
%% either {A,B,C,D,E,F}
%%    or  "AA-BB-CC-DD-EE-FF"  (hex)
%%    or  "Name" in case the name is resolve
%%
getaddr(Addr) when ?is_bt_address(Addr) ->
    {ok, Addr};
getaddr(Addr) when is_list(Addr) ->
    case string:tokens(Addr, "-") of
	[As,Bs,Cs,Ds,Es,Fs] ->
	    Res = (catch lists:map(fun(Hx) -> erlang:list_to_integer(Hx,16) end,
				   [As,Bs,Cs,Ds,Es,Fs])),
	    case Res of
		{'EXIT',_} ->
		    getaddr_by_name(Addr);
		[A,B,C,D,E,F] ->
		    {ok,{A,B,C,D,E,F}}
	    end;
	_ ->
	    getaddr_by_name(Addr)
    end;
getaddr(Addr) when is_list(Addr) ->
    getaddr_by_name(Addr);
getaddr(_) ->
    {error, einval}.
%%
%% getaddr_by_name(Name) -> {ok,Addr} | {error, Reason}
%% Find address by name (may be wastlty improved)
%%
getaddr_by_name(Name) ->
    {ok,Devices} = bt_drv:devices(),
    getaddr_by_name(Devices, Name).

getaddr_by_name([A|As], Name) ->
    case bt_drv:device_info(A, [name]) of
	{ok,[{name,Name}]} ->
	    {ok, A};
	_ ->
	    getaddr_by_name(As, Name)
    end;
getaddr_by_name([], _Name) ->
    {error, einval}.

%%
%% Inquiry scan: 
%%   Note that the Fun can not make meaning full remote calls
%%   while inquiry is running.
%% 
%%
scan(Timeout) ->
    scan(Timeout, fun(Addr,Acc) -> {continue,[Addr|Acc]} end, []).

scan(Timeout, Fun, Acc) ->
    case bt_drv:inquiry_start(Timeout) of
	{ok,Ref} ->
	    receive
		{bt,Ref,started} ->
		    scan_loop(Ref, Fun, Acc)
	    end;
	Error ->
	    Error
    end.

scan_loop(Ref, Fun, Acc) ->
    receive
	{bt,Ref,{device,Addr}} ->
	    case Fun(Addr,Acc) of
		{continue,Acc1} -> 
		    scan_loop(Ref, Fun, Acc1);
		{stop,Acc1} ->
		    bt_drv:inquiry_stop(Ref),
		    {ok,Acc1}
	    end;
	{bt,Ref, stopped} ->
	    {ok,Acc}
    end.

%%
%% Format bluetooth address into a hex string
%%
format_address(A) when ?is_bt_address(A) ->
    lists:flatten(
      io_lib:format(
	"~2.16.0B-"
	"~2.16.0B-"
	"~2.16.0B-"
	"~2.16.0B-"
	"~2.16.0B-"
	"~2.16.0B", tuple_to_list(A))).

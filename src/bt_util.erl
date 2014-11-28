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

-module(bt_util).

-export([getaddr/1]).
-export([getaddr_by_name/1]).

-include("../include/bt.hrl").
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

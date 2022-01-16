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
%%% File    : l2cap.erl
%%% Author  : Tony Rogvall <tony@pbook.synap.se>
%%% Description : L2CAP wrapper
%%% Created :  19 Jul 2006 by Tony Rogvall <tony@pbook.synap.se>

-module(l2cap).

-export([connect/2, connect/3, connect/4, connect/5,
	 close/1, 
	 send/2, send/3,
	 recv/1, recv/2,
	 listen/1, listen/2,
	 accept/1, accept/2]).

%% combined open/bind(any)/connect
connect(Address, Psm) ->
    connect(Address, Psm, infinity).
connect(Address, Psm, Timeout) ->
    [#{ bdaddr := Addr}|_] = hci:get_devices(),
    connect(Addr, 0, Address, Psm, Timeout).

connect(AdapterAddress, LocalPsm, Address, Psm) ->
    connect(AdapterAddress, LocalPsm, Address, Psm, infinity).

connect(AdapterAddress, LocalPsm, Address, Psm, Timeout) ->
    case bt_l2cap:open_() of
	{ok,L2CAP} -> 
	    case bt_l2cap:bind_(L2CAP, AdapterAddress, LocalPsm) of
		ok ->
		    TRef = start_timer(Timeout),
		    Result = async_connect(L2CAP, Address, Psm, TRef),
		    cancel_timer(TRef),
		    Result;
		Error ->
		    Error
	    end;
	Error -> Error
    end.

%% fixme: timeout
async_connect(L2CAP, Address, Psm, TRef) ->
    case bt_l2cap:connect_(L2CAP, Address, Psm) of
	{error, einprogress} ->
	    case bt_l2cap:select_(L2CAP, write) of
		ok ->
		    receive
			{select,L2CAP,undefined,_Ready} ->
			    case bt_l2cap:getpeername(L2CAP) of
				{ok, _} ->
				    {ok, L2CAP};
				Error ->
				    bt_l2cap:close(L2CAP),
				    Error
			    end;
			{timeout,TRef,_} ->
			    cancel_select(L2CAP, write),
			    {error, timeout}
		    end;
		Error ->
		    Error
	    end;
	ok -> {ok, L2CAP};
	Error -> Error
    end.

close(L2CAP) ->
    bt_l2cap:close(L2CAP).

send(L2CAP, Data) ->
    send(L2CAP, Data, infinity).
send(L2CAP, Data, Timeout) ->
    TRef = start_timer(Timeout),
    Result = async_write(L2CAP, iolist_to_binary(Data), TRef),
    cancel_timer(TRef),
    Result.
    
%% FIXME: timeout
async_write(_L2CAP, <<>>, _TRef) ->
    ok;
async_write(L2CAP, Data, TRef) ->
    case bt_l2cap:write_(L2CAP, Data) of
	{ok, _N} ->
	    ok;
	{error, eagain} ->
	    case bt_l2cap:select_(L2CAP, write) of
		ok ->
		    receive
			{select,L2CAP,undefined,_Ready} ->
			    %% io:format("write continue\n", []),
			    async_write(L2CAP, Data, TRef);
			{timeout,TRef,_} ->
			    cancel_select(L2CAP, write),
			    {error,timeout}
		    end;
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

cancel_select(L2CAP, Mode) ->
    case bt_l2cap:select_(L2CAP, [cancel,Mode]) of
	{ok,cancelled}  -> ok;
	_ -> 
	    %% flush the message
	    receive
		{select,L2CAP,undefined,_Ready} -> ok
	    after 0 -> ok
	    end
    end.


%% combined open/listen	    
listen(Psm) ->
    listen({0,0,0,0,0,0}, Psm).

listen(AdapterAddr, Psm) ->
    case bt_l2cap:open_() of
	{ok,L2CAP} -> 
	    case bt_l2cap:bind_(L2CAP, AdapterAddr, Psm) of
		ok ->
		    case bt_l2cap:listen_(L2CAP) of
			ok -> {ok,L2CAP};
			Error -> 
			    bt_l2cap:close(L2CAP),
			    Error
		    end;
		Error -> Error
	    end;    
	Error ->
	    Error
    end.

accept(L2CAP) ->
    accept(L2CAP, infinity).

accept(L2CAP, Timeout) ->
    TRef = start_timer(Timeout),
    Result = async_accept(L2CAP, TRef),
    cancel_timer(TRef),
    Result.

async_accept(L2CAP, TRef) ->
    case bt_l2cap:accept_(L2CAP) of
	{error, eagain} ->
	    case bt_l2cap:select_(L2CAP, read) of
		ok ->
		    receive
			{select,L2CAP,undefined,_Ready} ->
			    %% io:format("accept continue\n", []),
			    async_accept(L2CAP, TRef);
			{timeout,TRef,_} ->
			    cancel_select(L2CAP, read),
			    {error, timeout}
		    end;
		Error ->
		    Error
	    end;
	{ok, {CL2CAP, _AddrPsm}} ->
	    io:format("accepted connection from ~p\n", [_AddrPsm]),
	    {ok, CL2CAP};
	Error ->
	    Error
    end.

recv(L2CAP) ->
    recv(L2CAP, infinity).
recv(L2CAP, Timeout) ->
    TRef = start_timer(Timeout),
    Result = async_recv(L2CAP, TRef),
    cancel_timer(TRef),
    Result.
    
async_recv(L2CAP, TRef) ->
    case bt_l2cap:read_(L2CAP) of
	{error, eagain} ->
	    case bt_l2cap:select_(L2CAP, read) of
		ok ->
		    receive
			{select,L2CAP,undefined,_Ready} ->
			    %% io:format("read continue\n", []),
			    async_recv(L2CAP, TRef);
			{timeout,TRef,_} ->
			    cancel_select(L2CAP, read),
			    {error, timeout}
		    end;
		Error ->
		    Error
	    end;
	{ok, Data} ->
	    {ok, Data};
	Error ->
	    Error
    end.

start_timer(infinity) -> undefined;
start_timer(0) -> undefined;
start_timer(Timeout) -> erlang:start_timer(Timeout, self(), timeout).

cancel_timer(undefined) -> false;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef),
    receive {timeout,TRef,_} -> ok
    after 0 -> ok
    end.

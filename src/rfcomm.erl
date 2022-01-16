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
%%% File    : rfcomm.erl
%%% Author  : Tony Rogvall <tony@pbook.synap.se>
%%% Description : RFCOMM wrapper
%%% Created :  2 Feb 2006 by Tony Rogvall <tony@pbook.synap.se>

-module(rfcomm).

-export([connect/2, connect/3, connect/4, connect/5,
	 close/1, 
	 send/2, send/3, 
	 recv/1, recv/2,
	 listen/1, listen/2, 
	 accept/1, accept/2]).

connect(Address, Channel) ->
    connect(Address, Channel, infinity).

connect(Address, Channel, Timeout) ->
    [#{ bdaddr := Addr}|_] = hci:get_devices(),
    connect(Addr, 0, Address, Channel, Timeout).

connect(AdapterAddress, LocalChannel, Address, Channel) ->
    connect(AdapterAddress, LocalChannel, Address, Channel, infinity).

connect(AdapterAddress, LocalChannel, Address, Channel, Timeout) ->
    case bt_rfcomm:open_() of
	{ok,RFCOMM} -> 
	    case bt_rfcomm:bind_(RFCOMM, AdapterAddress, LocalChannel) of
		ok ->
		    TRef = start_timer(Timeout),
		    Result = async_connect(RFCOMM, Address, Channel, TRef),
		    cancel_timer(TRef),
		    Result;

		Error ->
		    Error
	    end;
	Error -> Error
    end.

%% fixme: timeout
async_connect(RFCOMM, Address, Channel, TRef) ->
    case bt_rfcomm:connect_(RFCOMM, Address, Channel) of
	{error, einprogress} ->
	    case bt_rfcomm:select_(RFCOMM, write) of
		ok ->
		    receive
			{select,RFCOMM,undefined,_Ready} ->
			    case bt_rfcomm:getpeername(RFCOMM) of
				{ok, _} ->
				    {ok, RFCOMM};
				Error ->
				    bt_rfcomm:close(RFCOMM),
				    Error
			    end;
			{timeout,TRef,_} ->
			    cancel_select(RFCOMM, write),
			    {error, timeout}
		    end;
		Error ->
		    Error
	    end;
	ok -> {ok, RFCOMM};
	Error -> Error
    end.

close(RFComm) ->
    bt_rfcomm:close(RFComm).


send(RFCOMM, Data) ->
    send(RFCOMM, Data, infinity).
send(RFCOMM, Data, Timeout) ->
    TRef = start_timer(Timeout),
    Result = async_write(RFCOMM, iolist_to_binary(Data), TRef),
    cancel_timer(TRef),
    Result.

async_write(_RFCOMM, <<>>, _TRef) ->
    ok;
async_write(RFCOMM, Data, TRef) ->
    case bt_rfcomm:write_(RFCOMM, Data) of
	{ok, N} ->
	    if N =:= byte_size(Data) ->
		    ok;
	       true ->
		    <<_:N/binary, Data1/binary>> = Data,
		    async_write(RFCOMM, Data1, TRef)
	    end;
	{error, eagain} ->
	    case bt_rfcomm:select_(RFCOMM, write) of
		ok ->
		    receive
			{select,RFCOMM,undefined,_Ready} ->
			    %% io:format("write continue\n", []),
			    async_write(RFCOMM, Data, TRef);
			{timeout,TRef,_} ->
			    cancel_select(RFCOMM, write),
			    {error,timeout}
		    end;
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

cancel_select(RFCOMM, Mode) ->
    case bt_rfcomm:select_(RFCOMM, [cancel,Mode]) of
	{ok,cancelled}  -> ok;
	_ -> 
	    %% flush the message
	    receive
		{select,RFCOMM,undefined,_Ready} -> ok
	    after 0 -> ok
	    end
    end.

%% combined open/listen	    
listen(Channel) ->
    listen({0,0,0,0,0,0}, Channel).

listen(AdapterAddr, Channel) ->
    case bt_rfcomm:open_() of
	{ok,RFCOMM} -> 
	    case bt_rfcomm:bind_(RFCOMM, AdapterAddr, Channel) of
		ok ->
		    case bt_rfcomm:listen_(RFCOMM) of
			ok -> {ok,RFCOMM};
			Error -> 
			    bt_rfcomm:close(RFCOMM),
			    Error
		    end;
		Error -> Error
	    end;    
	Error ->
	    Error
    end.

accept(RFCOMM) ->
    accept(RFCOMM, infinity).

accept(RFCOMM, Timeout) ->
    TRef = start_timer(Timeout),
    Result = async_accept(RFCOMM, TRef),
    cancel_timer(TRef),
    Result.

async_accept(RFCOMM, TRef) ->
    case bt_rfcomm:accept_(RFCOMM) of
	{error, eagain} ->
	    case bt_rfcomm:select_(RFCOMM, read) of
		ok ->
		    receive
			{select,RFCOMM,undefined,_Ready} ->
			    %% io:format("accept continue\n", []),
			    async_accept(RFCOMM, TRef);
			{timeout,TRef,_} ->
			    cancel_select(RFCOMM, read),
			    {error, timeout}
		    end;
		Error ->
		    Error
	    end;
	{ok, {CRFCOMM, _AddrChan}} ->
	    io:format("accepted connection from ~p\n", [_AddrChan]),
	    {ok, CRFCOMM};
	Error ->
	    Error
    end.

recv(RFCOMM) ->
    recv(RFCOMM, infinity).
recv(RFCOMM, Timeout) ->
    TRef = start_timer(Timeout),
    Result = async_recv(RFCOMM, TRef),
    cancel_timer(TRef),
    Result.

async_recv(RFCOMM, TRef) ->
    case bt_rfcomm:read_(RFCOMM) of
	{error, eagain} ->
	    case bt_rfcomm:select_(RFCOMM, read) of
		ok ->
		    receive
			{select,RFCOMM,undefined,_Ready} ->
			    %% io:format("read continue\n", []),
			    async_recv(RFCOMM, TRef);
			{timeout,TRef,_} ->
			    cancel_select(RFCOMM, read),
			    {error, timeout}
		    end;
		Error ->
		    Error
	    end;
	{ok, <<>>} -> %% detect close?
	    {error, closed};
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

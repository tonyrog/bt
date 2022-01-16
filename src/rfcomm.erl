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

-export([connect/2, connect/4, close/1, send/2, recv/1, 
	 listen/1, listen/2, accept/1, accept/2]).

connect(Address, Channel) ->
    [#{ bdaddr := Addr}|_] = hci:get_devices(),
    connect(Addr, 0, Address, Channel).

connect(AdapterAddress, LocalChannel, Address, Channel) ->
    case bt_rfcomm:open_() of
	{ok,RFCOMM} -> 
	    case bt_rfcomm:bind_(RFCOMM, AdapterAddress, LocalChannel) of
		ok ->
		    async_connect(RFCOMM, Address, Channel);
		Error ->
		    Error
	    end;
	Error -> Error
    end.

%% fixme: timeout
async_connect(RFCOMM, Address, Channel) ->
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
			    end
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
    async_write(RFCOMM, iolist_to_binary(Data)).

async_write(_RFCOMM, <<>>) ->
    ok;
async_write(RFCOMM, Data) ->
    case bt_rfcomm:write_(RFCOMM, Data) of
	{ok, N} ->
	    if N =:= byte_size(Data) ->
		    ok;
	       true ->
		    <<_:N/binary, Data1/binary>> = Data,
		    async_write(RFCOMM, Data1)
	    end;
	{error, eagain} ->
	    case bt_rfcomm:select_(RFCOMM, write) of
		ok ->
		    %% FIXME: timeout
		    receive
			{select,RFCOMM,undefined,_Ready} ->
			    io:format("write continue\n", []),
			    async_write(RFCOMM, Data)
		    end;
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

%% combined open/listen	    
listen(Channel) ->
    listen({0,0,0,0,0,0}, Channel).

listen(AdapterAddr, Channel) ->
    case bt_rfcomm:open_() of
	{ok,RFCOMM} -> 
	    case bt_rfcomm:bind_(RFCOMM, AdapterAddr, Channel) of
		ok ->
		    case bt_rfcomm:listen(RFCOMM) of
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

accept(RFCOMM, infinity) ->
    async_accept(RFCOMM, infinity);
accept(RFCOMM, Timeout) when is_integer(Timeout), Timeout > 0 ->
    async_accept(RFCOMM, Timeout).

async_accept(RFCOMM, Timeout) ->
    case bt_rfcomm:accept_(RFCOMM) of
	{error, eagain} when Timeout =:= 0 ->
	    {error, timeout};
	{error, eagain} ->
	    case bt_rfcomm:select_(RFCOMM, read) of
		ok ->
		    receive
			{select,RFCOMM,undefined,_Ready} ->
			    io:format("accept continue\n", []),
			    Timeout1 = if is_integer(Timeout) ->
					       Timeout div 2;
					  true ->
					       Timeout
				       end,
			    async_accept(RFCOMM, Timeout1) %% FIXME
		    after
			%% fixme: flush! if not {ok, cancelled}
			Timeout ->
			    bt_rfomm:select_(RFCOMM, [cancel,read]),
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
    async_recv(RFCOMM, infinity).

async_recv(RFCOMM, Timeout) ->
    case bt_rfcomm:read_(RFCOMM) of
	{error, eagain} when Timeout =:= 0 ->
	    {error, timeout};
	{error, eagain} ->
	    case bt_rfcomm:select_(RFCOMM, read) of
		ok ->
		    receive
			{select,RFCOMM,undefined,_Ready} ->
			    io:format("read continue\n", []),
			    Timeout1 = if is_integer(Timeout) ->
					       Timeout div 2;
					  true ->
					       Timeout
				       end,
			    async_recv(RFCOMM, Timeout1) %% FIXME
		    after
			%% fixme: flush! if not {ok, cancelled}
			Timeout ->
			    bt_rfomm:select_(RFCOMM, [cancel,read]),
			    {error, timeout}
		    end;
		Error ->
		    Error
	    end;
	{ok, <<>>} ->
	    {error, closed};
	{ok, Data} ->
	    {ok, Data};
	Error ->
	    Error
    end.

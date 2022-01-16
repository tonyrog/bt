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

-export([open/2, open/4, close/1, send/2, recv/1, 
	 listen/1, listen/2, accept/1, accept/2]).

%% combined open/bind(any)/connect
open(Address, Psm) ->
    [#{ bdaddr := Addr}|_] = hci:get_devices(),
    open(Addr, 0, Address, Psm).
open(AdapterAddress, LocalPsm, Address, Psm) ->
    case bt_l2cap:open_() of
	{ok,L2CAP} -> 
	    case bt_l2cap:bind_(L2CAP, AdapterAddress, LocalPsm) of
		ok ->
		    async_connect(L2CAP, Address, Psm);
		Error ->
		    Error
	    end;
	Error -> Error
    end.

%% fixme: timeout
async_connect(L2CAP, Address, Psm) ->
    case bt_l2cap:connect_(L2CAP, Address, Psm) of
	{error, einprogress} ->
	    case bt_l2cap:select_(L2CAP, write) of
		ok ->
		    %% FIXME: timeout
		    receive
			{select,L2CAP,undefined,_Ready} ->
			    case bt_l2cap:getpeername(L2CAP) of
				{ok, _} ->
				    {ok, L2CAP};
				Error ->
				    bt_l2cap:close(L2CAP),
				    Error
			    end
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
    async_write(L2CAP, iolist_to_binary(Data)).

async_write(_L2CAP, <<>>) ->
    ok;
async_write(L2CAP, Data) ->
    case bt_l2cap:write_(L2CAP, Data) of
	{ok, N} ->
	    if N =:= byte_size(Data) ->
		    ok;
	       true ->
		    <<_:N/binary, Data1/binary>> = Data,
		    async_write(L2CAP, Data1)
	    end;
	{error, eagain} ->
	    case bt_l2cap:select_(L2CAP, write) of
		ok ->
		    %% FIXME: timeout
		    receive
			{select,L2CAP,undefined,_Ready} ->
			    io:format("write continue\n", []),
			    async_write(L2CAP, Data)
		    end;
		Error ->
		    Error
	    end;
	Error ->
	    Error
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

accept(L2CAP, infinity) ->
    async_accept(L2CAP, infinity);
accept(L2CAP, Timeout) when is_integer(Timeout), Timeout > 0 ->
    async_accept(L2CAP, Timeout).

async_accept(L2CAP, Timeout) ->
    case bt_l2cap:accept_(L2CAP) of
	{error, eagain} when Timeout =:= 0 ->
	    {error, timeout};
	{error, eagain} ->
	    case bt_l2cap:select_(L2CAP, read) of
		ok ->
		    receive
			{select,L2CAP,undefined,_Ready} ->
			    io:format("accept continue\n", []),
			    Timeout1 = if is_integer(Timeout) ->
					       Timeout div 2;
					  true ->
					       Timeout
				       end,
			    async_accept(L2CAP, Timeout1) %% FIXME
		    after
			Timeout ->
			    bt_l2cap:select_(L2CAP, [cancel,read]),
			    {error, timeout}
		    end;
		Error ->
		    Error
	    end;
	{ok, CL2CAP} ->
	    {ok, CL2CAP};
	Error ->
	    Error
    end.

recv(L2CAP) ->
    async_recv(L2CAP, infinity).

async_recv(L2CAP, Timeout) ->
    case bt_l2cap:read_(L2CAP) of
	{error, eagain} when Timeout =:= 0 ->
	    {error, timeout};
	{error, eagain} ->
	    case bt_l2cap:select_(L2CAP, read) of
		ok ->
		    receive
			{select,L2CAP,undefined,_Ready} ->
			    io:format("read continue\n", []),
			    Timeout1 = if is_integer(Timeout) ->
					       Timeout div 2;
					  true ->
					       Timeout
				       end,
			    async_recv(L2CAP, Timeout1) %% FIXME
		    after
			Timeout ->
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

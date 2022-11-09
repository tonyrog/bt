%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Simple RF comm monitor
%%% @end
%%% Created :  4 May 2022 by Tony Rogvall <tony@rogvall.se>

-module(bt_mon).

-export([rfcomm/2]).
-export([l2cap/2]).

%% rfcomm monitor loop
rfcomm(Address, Channel) ->
    case rfcomm:connect(Address, Channel, 10000) of
	{ok, Ref} ->
	    Res = rfcomm_loop(Ref),
	    rfcomm:close(Ref),
	    Res;
	Error ->
	    Error
    end.

rfcomm_loop(Ref) ->
    case bt_rfcomm:read_(Ref) of
	{error, eagain} ->
	    case bt_rfcomm:select_(Ref, read) of
		ok ->
		    receive
			{select,Ref,undefined,_Ready} ->
			    %% io:format("read continue\n", []),
			    rfcomm_loop(Ref)
		    end;
		Error ->
		    Error
	    end;
	{ok, <<>>} -> %% detect close?
	    {error, closed};
	{ok, Data} ->
	    io:format("rfcomm: ~p\n", [Data]),
	    rfcomm_loop(Ref);
	Error ->
	    Error
    end.

%% l2cap monitor loop
l2cap(Address, Psm) ->
    case l2cap:connect(Address, Psm, 10000) of
	{ok, Ref} ->
	    Res = l2cap_loop(Ref),
	    rfcomm:close(Ref),
	    Res;
	Error ->
	    Error
    end.

l2cap_loop(Ref) ->
    case bt_l2cap:read_(Ref) of
	{error, eagain} ->
	    case bt_l2cap:select_(Ref, read) of
		ok ->
		    receive
			{select,Ref,undefined,_Ready} ->
			    %% io:format("read continue\n", []),
			    l2cap_loop(Ref)
		    end;
		Error ->
		    Error
	    end;
	{ok, Data} ->
	    io:format("l2cap: ~p\n", [Data]),
	    l2cap_loop(Ref);
	Error ->
	    Error
    end.

	    


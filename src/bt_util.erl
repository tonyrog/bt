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
-export([format_address/1]).
-export([uuid_to_string/1]).
-export([string_to_uuid/1]).
-export([string_to_addr/1]).

-include("../include/bt.hrl").
%%
%% getaddr(Name) -> {ok,Addr} | {error, Reason}
%%
%% Convert address into a bluetooth address
%% either {A,B,C,D,E,F}
%%    or  "AA-BB-CC-DD-EE-FF"  (hex)
%%    or  "AA:BB:CC:DD:EE:FF"  (hex)
%%    or  "Name" in case the name is resolve
%%
getaddr(Addr) when ?is_bt_address(Addr) ->
    {ok, Addr};
getaddr(Addr) when is_list(Addr) ->
    case string:tokens(Addr, ":-") of
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

string_to_addr(Addr) when is_list(Addr) ->
    XParts = string:tokens(Addr, ":-"),
    try [erlang:list_to_integer(Hx,16) || Hx <- XParts] of
	[A,B,C,D,E,F] ->
	    {ok,{A,B,C,D,E,F}}
    catch
	error:_ ->
	    {error, einval}
    end.

%%
%% getaddr_by_name(Name) -> {ok,Addr} | {error, Reason}
%% Find address by name (may be wastlty improved)
%%
getaddr_by_name(Name) ->
    getaddr_by_name_(string:to_lower(Name), 
		     string:tokens(os:cmd("bluetoothctl devices"), "\n")).

getaddr_by_name_(Name, [Line|Ls]) ->
    case string:tokens(Line, " ") of
	["Device", Addr | Ns] -> 
	    case string:to_lower(string:join(Ns, " ")) of
		Name -> string_to_addr(Addr);
		_ -> getaddr_by_name_(Name, Ls)
	    end;
	_ -> getaddr_by_name_(Name, Ls)
    end;
getaddr_by_name_(_Name, []) ->	
    {error, enoent}.


%% convert uuid to string format
uuid_to_string(UUID) when ?is_uuid(UUID) ->
    ?UUID(TLow,TMid,THigh,Clock,Node) = UUID,
    Fmt = 
	io_lib:format("~8.16.0B-~4.16.0B-~4.16.0B-~4.16.0B-~12.16.0B",
		      [TLow,TMid,THigh,Clock,Node]),
    lists:flatten(Fmt).

%% convert uuid string format to bina
string_to_uuid([X1,X2,X3,X4,X5,X6,X7,X8,$-,
		Y1,Y2,Y3,Y4,$-,
		Z1,Z2,Z3,Z4,$-,
		C1,C2,C3,C4,$-,
		N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12]) ->
    TimeLow = erlang:list_to_integer([X1,X2,X3,X4,X5,X6,X7,X8],16),
    TimeMid  = erlang:list_to_integer([Y1,Y2,Y3,Y4],16),
    TimeHigh = erlang:list_to_integer([Z1,Z2,Z3,Z4],16),
    Clock    = erlang:list_to_integer([C1,C2,C3,C4],16),
    Node     = erlang:list_to_integer([N1,N2,N3,N4,N5,N6,N7,N8,
				       N9,N10,N11,N12],16),
    ?UUID(TimeLow,TimeMid,TimeHigh,Clock,Node);
string_to_uuid(_) ->
    erlang:error(bad_arg).

%%
%% Format bluetooth address into a hex string
%%
format_address(A) when ?is_bt_address(A) ->
    case os:type() of
	{unix,darwin} ->
	    format_address_(A, $-);
	_ ->
	    format_address_(A, $:)
    end;
format_address(<<A,B,C,D,E,F>>) ->
    format_address({A,B,C,D,E,F}).
		
format_address_({A,B,C,D,E,F}, S) ->
    [hexh(A),hexl(A),S,hexh(B),hexl(B),S,hexh(C),hexl(C),S,
     hexh(D),hexl(D),S,hexh(E),hexl(E),S,hexh(F),hexl(F)].

hexl(A) -> hex1(A band 16#f).
hexh(A) -> hex1((A bsr 4) band 16#f).

hex1(A) when A < 10 -> A+$0;
hex1(A) -> (A-10)+$a.

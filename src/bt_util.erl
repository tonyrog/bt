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
-export([uuid_to_little/1]).

-export([string_to_addr/1]).
-export([maps_take/3]).
-export([maps_find/3]).

-include("../include/bt.hrl").

%% convert various formats into bt_mac format
addr_to_mac(Addr) when ?is_bt_mac(Addr) ->
    Addr;
addr_to_mac(<<F,E,D,C,B,A>>) ->
    {A,B,C,D,E,F};
addr_to_mac([A1,A0,$:,B1,B0,$:,C1,C0,$:,D1,D0,$:,E1,E0,$:,F1,F0]) ->
    {list_to_integer([A1,A0], 16),
     list_to_integer([B1,B0], 16),
     list_to_integer([C1,C0], 16),
     list_to_integer([D1,D0], 16),
     list_to_integer([E1,E0], 16),
     list_to_integer([F1,F0], 16)};
addr_to_mac([A1,A0,$-,B1,B0,$-,C1,C0,$-,D1,D0,$-,E1,E0,$-,F1,F0]) ->
    {list_to_integer([A1,A0], 16),
     list_to_integer([B1,B0], 16),
     list_to_integer([C1,C0], 16),
     list_to_integer([D1,D0], 16),
     list_to_integer([E1,E0], 16),
     list_to_integer([F1,F0], 16)}.

%% remove me!
%% getaddr(Name) -> Addr | {error, Reason}
%%
%% Convert address into a bluetooth address
%% either {A,B,C,D,E,F}
%%    or  "AA-BB-CC-DD-EE-FF"  (hex)
%%    or  "AA:BB:CC:DD:EE:FF"  (hex)
%%    or  "Name" in case the name is resolve
%%
getaddr(Addr) ->
    try addr_to_mac(Addr) of
	Mac -> Mac
    catch
	error:_ ->
	    if is_list(Addr) ->
		    getaddr_by_name(Addr);
	       true ->
		    {error, einval}
	    end
    end.

-spec string_to_addr(String::string()) -> bt_mac().

string_to_addr([A1,A0,$:,B1,B0,$:,C1,C0,$:,D1,D0,$:,E1,E0,$:,F1,F0]) ->
    {list_to_integer([A1,A0], 16),
     list_to_integer([B1,B0], 16),
     list_to_integer([C1,C0], 16),
     list_to_integer([D1,D0], 16),
     list_to_integer([E1,E0], 16),
     list_to_integer([F1,F0], 16)};
string_to_addr([A1,A0,$-,B1,B0,$-,C1,C0,$-,D1,D0,$-,E1,E0,$-,F1,F0]) ->
    {list_to_integer([A1,A0], 16),
     list_to_integer([B1,B0], 16),
     list_to_integer([C1,C0], 16),
     list_to_integer([D1,D0], 16),
     list_to_integer([E1,E0], 16),
     list_to_integer([F1,F0], 16)};
string_to_addr(_) ->
    {error, einval}.



%% Find address by name (may be wastlty improved)
-spec getaddr_by_name(Name::string()) -> bt_mac() | {error, Reason::term()}.
getaddr_by_name(Name) ->
    getaddr_by_name_(string:to_lower(Name), 
		     string:tokens(os:cmd("bluetoothctl devices"), "\n")).

getaddr_by_name_(Name, [Line|Ls]) ->
    case string:tokens(Line, " ") of
	["Device", Addr | Ns] -> 
	    case string:to_lower(string:join(Ns, " ")) of
		Name ->
		    string_to_addr(Addr);
		_ ->
		    getaddr_by_name_(Name, Ls)
	    end;
	_ -> getaddr_by_name_(Name, Ls)
    end;
getaddr_by_name_(_Name, []) ->	
    {error, enoent}.


-spec uuid_to_string(uuid()) -> string().
%% convert uuid to string format
uuid_to_string(<<N:16>>) ->
    uuid_to_string_(?BT_UUID16(N));
uuid_to_string(<<N:32>>) ->
    uuid_to_string_(?BT_UUID32(N));
uuid_to_string(UUID) when ?is_uuid(UUID) ->
    uuid_to_string_(UUID).

uuid_to_string_(UUID) when ?is_uuid(UUID) ->
    ?UUID(TLow,TMid,THigh,Clock,Node) = UUID,
    Fmt = 
	io_lib:format("~8.16.0B-~4.16.0B-~4.16.0B-~4.16.0B-~12.16.0B",
		      [TLow,TMid,THigh,Clock,Node]),
    lists:flatten(Fmt).

    
-spec string_to_uuid(string()) -> uuid().
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

%% swap bytes 
uuid_to_little(<<UUID:16>>) -> <<UUID:16/little>>;
uuid_to_little(<<UUID:32>>) -> <<UUID:32/little>>;
uuid_to_little(<<UUID:128>>) -> <<UUID:128/little>>.

%%
%% Format bluetooth address into a hex string
%%
format_address(A) when ?is_bt_mac(A); ?is_bt_bmac(A) ->
    case os:type() of
	{unix,darwin} ->
	    format_address_(A, $-);
	_ ->
	    format_address_(A, $:)
    end.
		
format_address_({A,B,C,D,E,F}, S) ->
    [hex2(A),S,hex2(B),S,hex2(C),S,hex2(D),S,hex2(E),S,hex2(F)];
format_address_(<<F,E,D,C,B,A>>,S) -> %% binary format is reversed (wire)
    [hex2(A),S,hex2(B),S,hex2(C),S,hex2(D),S,hex2(E),S,hex2(F)].


%% assume A is byte return two characters
-spec hex2(X::byte()) -> [char()].
hex2(A) -> tl(integer_to_list(A+16#100, 16)).


%% locate a map with in a list of maps and return a
%% the map as value and remove the matching map from list
%% return the new list
-spec maps_take(Value::term(), Key::term(), MapList::[map()]) ->
	  {value, M::map(), MapList1::[map()]} | false.

maps_take(Value, Key, MapList) ->
    maps_take_(Value, Key, MapList, []).

maps_take_(Value, Key, [M|Ms], Acc) ->
    case maps:find(Key, M) of
	{ok,Value} ->
	    {value, M, lists:reverse(Acc, Ms)};
	_ ->
	    maps_take_(Value, Key, Ms, [M|Acc])
    end;
maps_take_(_Value, _Key, [], _) ->
    false.

%% locate a map with in a list of maps
-spec maps_find(Value::term(), Key::term(), MapList::[map()]) ->
	  M::map() | false.
maps_find(Value, Key, MapList) ->
    maps_find_(Value, Key, MapList).

maps_find_(Value, Key, [M|Ms]) ->
    case maps:find(Key, M) of
	{ok,Value} ->
	    M;
	_ ->
	    maps_find_(Value, Key, Ms)
    end;
maps_find_(_Value, _Key, []) ->
    false.

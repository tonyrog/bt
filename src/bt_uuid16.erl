%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Blueooth 16 bit uuid assigned numbers
%%% @end
%%% Created : 18 Jan 2022 by Tony Rogvall <tony@rogvall.se>

-module(bt_uuid16).

-export([to_string/1]).
-export([from_string/1]).

to_string(Key) when is_integer(Key), Key >= 0, Key =< 16#ffff ->
    case lookup_in_table([protocol_identifiers,
			  service_class,
			  service_class,
			  characteristics_uuids,
			  units,
			  member_uuids], <<"uuid">>, Key) of
	{ok, Object} ->
	    maps:get(<<"uuid">>, Object);
	{error, enoent} ->
	    undefined
    end.

from_string(Name) when is_list(Name) ->
    Key = unicode:characters_to_binary(Name),
    case lookup_in_table([protocol_identifiers,
			  service_class,
			  service_class,
			  characteristics_uuids,
			  units,
			  member_uuids], <<"name">>, Key) of
	{ok, Object} ->
	    maps:get(<<"name">>, Object);
	{error, enoent} ->
	    undefined
    end.

lookup_in_table([T|Ts], KeyName, Key) ->
    case bt_db:lookup(T, KeyName, Key) of
	{ok, Object} -> Object;
	{error, enoent} -> lookup_in_table(Ts, KeyName, Key)
    end;
lookup_in_table([], _, _) ->
    {error, enoent}.

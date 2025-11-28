#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% Create oui.db from out.term (created with make_out.sh)
%%
%% write binary blob 128 bytes per record
%% <<OUI:24, Len:8, Name:Len, 0...>>
%% max name length = 128-3-1 = 124, padded with zeros
%%
%% FIXME: split in two files!
%% 1 oui.db.idx  <<Oui:24>> -> <<Offset:32>> (Out sorted)
%% 2 out.db while inserting strings keep only the first string!
%%          and let offsets point there!
%% 

main(_Args) ->
    {ok, OUIList0} = file:consult("oui.term"),
    %% make oui.db searchable with binary search
    OUIList = lists:keysort(1, OUIList0),
    Data = 
	[ begin
	      Name1 = encode_name(Name, 124),
	      Len   = byte_size(Name1),
	      Pad = 124 - Len,
	      <<OUI:24, Len, Name1/binary, 0:Pad/unit:8>>
	  end || {OUI, Name} <- OUIList],
    file:write_file("oui.db", Data).

%% must iterate and truncate since utf8.... 
encode_name(Name, MaxLen) ->
    Bin = unicode:characters_to_binary(Name),
    if byte_size(Bin) =< MaxLen ->
	    Bin;
       true ->
	    %% remove last character (truncate) and try again
	    Len = length(Name),
	    Name1 = lists:sublist(Name, 1, Len-1),
	    encode_name(Name1, MaxLen)
    end.

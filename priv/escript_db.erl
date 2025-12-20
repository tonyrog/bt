-module(escript_db).
-export([main/1]).

main([]) -> main(["list"]);
main(["help"|_]) -> 
    usage_db();
main(["list"|_]) ->
    list_db();
main(["info"|_]) -> 
    info_db();
main(["layout"|_]) -> 
    layout_db();
main(["find", KeyName, Key |_]) ->
    find_db(list_to_binary(string:trim(KeyName)), Key);
main(["find", Key |_]) -> 
    find_db(undefined, Key);
main([Command|_]) ->
    io:format("escript_db: unknown command ~s\n", [Command]),
    usage_db().

usage_db() ->
    io:format("usage: <db> [<command>]\n"),
    io:format(" command:\n"),
    io:format("    list       -- list all entries\n"),
    io:format("    info       -- display info\n"),
    io:format("    layout     -- display layout\n"),
    io:format("    find <key> -- display data base entry\n"),
    io:format("    find <index-name> <key> -- display data base entry\n"),
    io:format("    help       -- print help\n"),
    ok.

open_db() ->
    {ok,Fd} = file:open(escript:script_name(), [read,binary]),
    {ok,EndPos0}  = file:position(Fd, {eof,-4}),
    {ok, <<IndPos1:32/big,InitPos:32/big>>} = file:pread(Fd, {eof,-8}, 8),
    EndPos = if IndPos1 =:= 0 -> EndPos0 - 4;
		true -> IndPos1-4
	     end,
    {ok, _} = file:position(Fd, InitPos),
    {ok, DbType} = read_object(Fd, atom),
    %% io:format("db_type = ~p\n", [DbType]),
    KeyNames = read_key_list(Fd, []),
    %% io:format("keys = ~p\n", [KeyNames]),
    {ok, StartPos} = file:position(Fd, cur),
    Size = if IndPos1 =:= 0 -> 
		   EndPos - StartPos;
	      true ->
		   IndPos1 - StartPos
	   end,
    %% io:format("size = ~p\n", [Size]),
    {Fd, [{db_type, DbType},{keys,KeyNames},
	  {start_pos,StartPos},{size,Size}]}.

read_key_list(Fd, Acc) ->
    case read_object(Fd, string) of
	eof -> lists:reverse(Acc);
	{ok, Name} ->
	    read_key_list(Fd, [Name|Acc])
    end.

find_db(KeyName, Key) ->
    {Fd,Info} = open_db(),
    KeyList = proplists:get_value(keys, Info, []),
    KeyName1 = 
	if KeyName =:= undefined ->
		case KeyList of
		    [K1|_] -> K1;
		    [] ->
			io:format("error: no index tables exist\n", []),
			usage_db(),
			erlang:halt(1)
		end;
	   true ->
		case lists:member(KeyName, KeyList) of
		    true -> KeyName;
		    false ->
			io:format("warning: keyname ~s has no index\n", 
				  [KeyName]),
			KeyName
		end
	end,
    Key1 = key(KeyName1, Key),
    iter_db(Fd, 
	    fun(Entry) ->
		    case proplists:get_value(KeyName1, Entry) of
			Key1 -> io:format("~1000p\n", [Entry]), false;
			_ -> true
		    end
	    end).

list_db() ->
    {Fd,_} = open_db(),
    iter_db(Fd, fun(Entry) ->
			io:format("~1000p\n", [Entry]), true
		end).

iter_db(Fd, Fun) ->
    case read_object(Fd, term) of
	eof -> ok;
	{ok, Entry} ->
	    case Fun(Entry) of
		true -> iter_db(Fd, Fun);
		false -> ok
	    end;
	{error,Reason} ->
	    io:format("file:read error ~p\n", [Reason]),
	    erlang:halt(1)
    end.

info_db() ->
    {_Fd, Info} = open_db(),
    io:format("db_type: ~p\n", [proplists:get_value(db_type,Info)]),
    io:format("keys: ~p\n", [proplists:get_value(keys,Info)]),
    io:format("start_pos: ~p\n", [proplists:get_value(start_pos,Info)]),
    io:format("size: ~p\n", [proplists:get_value(size,Info)]),
    ok.

layout_db() ->
    io:format("escript_db: layout\n"),
    io:put_chars(
"<<\"#!/usr/bin/env escript\\n\">>
<<\"FOR1\",BeamBin/binary>>
InitPos:
  CbType/string
KeyNames:
  KeyName1/string
  KeyName2/string
  ...
  KeyNameN/string
  0:32

StartPos:
Pos1:  Object1
Pos2:  Object2
...
PosN:  ObjectN
EndPos: 0:32

IndPos1:
  Index11/ind16, Index12/ind16, ... Index1N/ind16
IndPos2:
  Index21/ind16, Index22/ind16, ... Index2N/ind16
...
IndPosK:
  IndexK1/ind16, IndexK2/ind16, ... IndexKN/ind16
IndEnd: 0:32
IndPosk:32, ..., IndPos2:32, IndPos1:32
InitPos:32

Where (big-endian>
  ind16    = <<Key:16, Offset:32>>
  string   = <<Size:32, String:Size/binary>>
  ojbect   = <<Size:32, Object:Size/binary>>
").

read_object(Fd, DbType) ->
    case file:read(Fd, 4) of
	{ok, <<0:32/big>>} ->
	    eof;
	{ok, <<Size:32/big>>} ->
	    case file:read(Fd, Size) of
		{ok, Bin} ->
		    {ok,decode(Bin, DbType)};
		Error ->
		    Error
	    end;
	Error -> Error
    end.

decode(Bin, term) -> binary_to_term(Bin);
decode(Bin, json) -> json:decode(Bin);
decode(Bin, cson) -> cson:decode(Bin);
decode(Bin, erlang) ->
    {ok,Ts,_} = erl_scan:string(unicode:binary_to_characters(Bin)++"."),
    {ok, [Term]} = erl_parse:parse_exprs(Ts),
    Term;
decode(Bin, atom) -> binary_to_atom(Bin);
decode(Bin, string) -> Bin.

key(<<"uuid">>, Arg) -> numeric_key(Arg);
key(_, Arg) ->  numeric_key(Arg).

numeric_key("0x"++String) -> num(String, 16);
numeric_key("16#"++String) -> num(String, 16);
numeric_key(String=[$0|_]) -> num(String, 8);
numeric_key(String=[C|_]) when C >= $1, C =< $9 -> num(String, 10);
numeric_key(String) -> str(String).

str(String) ->
    list_to_binary(string:trim(String)).

num(String, Base) ->
    String1 = string:trim(String),
    try list_to_integer(String1, Base) of
	Int ->  Int
    catch
	error:_ -> String1
    end.

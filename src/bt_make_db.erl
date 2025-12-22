%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%    Make db files for lookup 
%%% @end
%%% Created :  1 Dec 2025 by Tony Rogvall <tony@rogvall.se>

-module(bt_make_db).

-export([start/1]).
-export([create/4]).

-compile(export_all).

start([FilenameAtom,ListID0|KeyNames0]) ->
    Filename = atom_to_list(FilenameAtom),
    ListID = atom_to_binary(ListID0),
    KeyNameList = [atom_to_binary(KeyName) || KeyName <- KeyNames0],
    create(Filename, term, ListID, KeyNameList),
    erlang:halt(0).

create(Filename, DbType, ListID, KeyNameList) ->
    case bt_yaml:file(Filename) of
	{error, Reason} ->
	    {error, Reason};
	YAML ->
	    Data = bt_yaml:to_term(YAML),
	    Entries = 
		if ListID =:= <<>> -> Data;
		   true -> proplists:get_value(ListID, Data)
		end,
	    create_db(Filename, DbType, KeyNameList, Entries)
    end.

create_db(Filename, DbType, KeyNameList, Entries) ->
    Ext = filename:extension(Filename),
    BaseName = filename:basename(Filename, Ext),
    {ok, Fd} = file:open(BaseName++".db", [write]),
    file:write(Fd, "#!/usr/bin/env escript\n"),
    BeamName = filename:join(code:priv_dir(bt),"escript_db.beam"),
    {ok, Escript} = file:read_file(BeamName),
    file:write(Fd, Escript),
    {ok, StartPos} = file:position(Fd, cur),
    %% info header - index name table
    write_object(Fd, DbType, string),
    lists:foreach(
      fun(KeyName) ->
	      write_object(Fd, KeyName, string)
      end, KeyNameList),
    ok = file:write(Fd, <<0:32>>),

    %% Write entry db keep Positions
    PosEntries =
	[begin
	     {ok,Pos} = file:position(Fd, cur),
	     ok = write_object(Fd, Entry, DbType),
	     {Pos, Entry}
	 end || Entry <- Entries],
    ok = file:write(Fd, <<0:32>>), %% end marker

    %% First classify indices and error none-uniqness
    IndexTables = init_db_index(KeyNameList, PosEntries, []),

    %% table1, table2 .. tableN
    StartPosTable = 
	lists:foldl(
	  fun({_Name,{uint,16},{index,_First,_Last,Sorted}}, Acc) ->
		  IndexData = [<<Value:16, Pos:32>> || {Value,Pos} <- Sorted],
		  {ok, IndPos} = file:position(Fd, cur),
		  ok = file:write(Fd, IndexData),
		  [IndPos|Acc];
	     ({_Name,_DbType,{indirect,_First,_Last,Array}},Acc) ->
		  IndexData = [<<Pos:32>> || {_I,Pos} <- Array],
		  {ok, IndPos} = file:position(Fd, cur),
		  ok = file:write(Fd, IndexData),
		  [IndPos|Acc];
	     ({_Name,string,{hash,_First,_Last,_N,Hashed}},Acc) ->
		  IndexData = [<<Pos:32>> || {_I,Pos} <- Hashed],
		  {ok, IndPos} = file:position(Fd, cur),
		  ok = file:write(Fd, IndexData),
		  [IndPos|Acc];
	     ({_Name,string,{hashx,_First,_Last,_N,Hashed}},Acc) ->
		  IndexData = [<<H:32,Pos:32>> || {_I,H,Pos} <- Hashed],
		  {ok, IndPos} = file:position(Fd, cur),
		  ok = file:write(Fd, IndexData),
		  [IndPos|Acc]
	  end, [], IndexTables),
    %% start position tables is build in reverse
    %% 0, posN...pos2,pos1, followed by start_pos of db
    ok = file:write(Fd, <<0:32>>),
    lists:foreach(
      fun(Pos) ->
	      ok = file:write(Fd, <<Pos:32>>)
      end, StartPosTable),
    ok = file:write(Fd, <<StartPos:32>>),
    file:close(Fd).

%% 
init_db_index([KeyName|KeyNameList], PosEntries, Acc) ->
    Index = init_db_index1(KeyName, PosEntries),
    init_db_index(KeyNameList, PosEntries,[Index|Acc]);
init_db_index([], _PosEntries, Acc) ->
    lists:reverse(Acc).

init_db_index1(KeyName, PosEntries) ->
    init_db_index1(KeyName, PosEntries, sets:new(), undefined, []).

init_db_index1(KeyName, [{Pos,Entry}|PosEntries], Set, Type, Acc) ->
    Value = proplists:get_value(KeyName, Entry),
    NotUnique = sets:is_element(Value, Set),
    if NotUnique, Type =/= none ->
	    io:format("index ~s is not unique\n", [KeyName]),
	    Type1 = none,
	    init_db_index1(KeyName, PosEntries, Set, Type1,
			   [{Value,Pos}|Acc]);
       true ->
	    Set1 = sets:add_element(Value, Set),
	    Type1 = update_type(Value, Type),
	    init_db_index1(KeyName, PosEntries, Set1, Type1,
			   [{Value,Pos}|Acc])
    end;
init_db_index1(KeyName, [], _Set, Type, Acc) ->
    %% coerce values
    Entries = 
	case Type of
	    none -> [];
	    undefined -> [];
	    string -> update_string(Acc);
	    {uint,_N} -> update_int(Acc);
	    {int, _N} -> update_int(Acc);
	    {float, _} -> update_float(Acc)
	end,
    {KeyName, Type, Entries}.

update_string(PosEntries) ->
    update_string_(PosEntries, []).

update_string_([{Value,Pos}|PosEntries], Acc) ->
    update_string_(PosEntries,[{erlang:adler32(Value),Pos}|Acc]);
update_string_([], Acc) ->
    Len = length(Acc),
    N0 = if Len band 1 =:= 1 -> Len;
	    true -> Len+1
	 end,
    Limit = 3*N0 + 1,
    search_hash_len(Acc, N0, Limit, {Len,N0}).

search_hash_len(HashEntries, N, Limit, {Best,BestN}) when N < Limit ->
    Hashed = [{H rem N,H,Pos} || {H,Pos} <- HashEntries],
    case number_of_hash_conflicts(Hashed) of
	0 ->
	    Hashed1 = lists:keysort(1, Hashed),
	    First = element(1, hd(Hashed1)),
	    Last  = element(1, lists:last(Hashed1)),
	    Hashed2 = [{H,Pos} || {_,H,Pos} <- fill(Hashed1)],
	    {hash, First, Last, N, Hashed2};
	C ->
	    if C < Best ->
		    search_hash_len(HashEntries, N+2, Limit, {C,N});
	       true ->
		    search_hash_len(HashEntries, N+2, Limit, {Best,BestN})
	    end
    end;
search_hash_len(HashEntries, _N, _Limit, {_Best,BestN})  ->
    Hashed = [{H rem BestN,H,Pos} || {H,Pos} <- HashEntries],
    Hashed1 = lists:keysort(1, Hashed),
    First = element(1, hd(Hashed1)),
    Last  = element(1, lists:last(Hashed1)),
    %% hash miss entries?
    {hashx, First, Last, BestN, Hashed1}.


number_of_hash_conflicts(HashEntries) ->
    Vals = [element(1,Ent)|| Ent <- HashEntries],
    length(Vals -- lists:usort(Vals)).


update_int(PosEntries) ->
    case lists:keysort(1, PosEntries) of
	[]  -> 
	    {index,0,0,[]};
	Sorted=[{Value,_}] -> 
	    {index,Value,Value,Sorted};
	Sorted = [{FirstValue,_} | Entries] ->
	    {LastValue,_} = lists:last(Entries),
	    Len = length(Sorted),
	    M = (LastValue - FirstValue)+1,
	    if Len / M > 0.8 ->
		    %% fill with zero's offsets
		    Array = [Pos || {_Value,Pos} <- fill(Sorted)],
		    {indirect,FirstValue,LastValue,Array};
	       true ->
		    {index,FirstValue,LastValue,Sorted}
	    end
    end.

update_float(PosEntries) ->
    lists:keysort(1, PosEntries).

fill(Entries=[]) -> Entries;
fill(Entries=[_]) -> Entries;
fill([At|Entries=[Bt|_]]) when element(1,At)+1 =:= element(1,Bt) ->
    [At|fill(Entries)];
fill([At|Entries=[Bt|_]]) when element(1,At) < element(1,Bt) ->
    Ct = list_to_tuple([element(1,At)+1 | lists:duplicate(tuple_size(At)-1,0)]),
    [At|fill([Ct|Entries])].

	    
update_type(Value, Type0) ->
    Type1 = 
	if is_integer(Value), Value >= 16#00, Value =< 16#ff ->
		{uint,8};
	   is_integer(Value), Value >= 16#0000, Value =< 16#ffff ->
		{uint,16};		
	   is_integer(Value), Value >= 16#00000000, Value =< 16#ffffffff ->
		{uint,32};
	   is_integer(Value), Value >= -16#80, Value =< 16#7f ->
		{int,8};		
	   is_integer(Value), Value >= -16#8000, Value =< 16#7fff ->
		{int,16};		
	   is_integer(Value), Value >= -16#80000000, Value =< 16#7fffffff ->
		{int,32};
	   is_float(Value) ->
		{float,32};
	   is_binary(Value) ->
		string;
	   is_boolean(Value) ->
		boolean;
	   is_atom(Value) ->
		atom
	end,
    coerce_type(Type0, Type1).

coerce_type(undefined, T) -> T;
coerce_type(T, T)    -> T;
coerce_type(none, _) -> none;
coerce_type(_, none) -> none;
coerce_type({uint,N},{uint,M}) -> {uint,max(N,M)};
coerce_type({int,N},{uint,M}) ->  {int,min(32,max(N+1,M))};
coerce_type({uint,N},{int,M}) ->  {int,min(32,max(N,M+1))};
coerce_type({float,N},{uint,_}) -> {float,N};
coerce_type({float,N},{int,_}) -> {float,N};
coerce_type({uint,_},{float,N}) -> {float,N};
coerce_type({int,_},{float,N}) -> {float,N};

coerce_type(T={int,_},boolean) -> T;
coerce_type(T={uint,_},boolean) -> T;
coerce_type(boolean,T={int,_}) -> T;
coerce_type(boolean,T={uint,_}) -> T;

%% can not mix string/number
coerce_type({int,_},string) -> none;
coerce_type({uint,_},string) -> none;
coerce_type({float,_},string) -> none;
coerce_type(string,{int,_}) -> none;
coerce_type(string,{uint,_}) -> none;
coerce_type(string,{float,_}) -> none;
coerce_type(_, _) -> none.


write_object(Fd, Object, DbType) ->
    Bin = encode_object(Object, DbType),
    Size = byte_size(Bin),
    file:write(Fd, <<Size:32, Bin/binary>>).

encode_object(Object, term) ->
    term_to_binary(Object);
encode_object(Object, erlang) ->
    iolist_to_binary(io_lib:format("~1000p", [Object]));
encode_object(Object, json) ->
    iolist_to_binary(json:encode(Object));
encode_object(Object, cson) ->
    iolist_to_binary(cson:encode(Object));
%% string type is for encoding dbtype/keynames ...
encode_object(Object, string) when is_binary(Object) ->
    Object;
encode_object(Object, string) when is_atom(Object) ->
    atom_to_binary(Object);
encode_object(Object, string) when is_list(Object) ->
    unicode:characters_to_binary(Object).




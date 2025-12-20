%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%    Look up and print values in uuid databases
%%% @end
%%% Created :  1 Dec 2025 by Tony Rogvall <tony@rogvall.se>

-module(bt_db).

-export([open/1, open/2]).
-export([close/1]).
-export([lookup/2]).

%% debug
-export([open_dbf_/1]).
-export([close_dbf_/1]).
-export([lookup_dbf_/3]).

-export([dump_index/2]).

-record(dbi,
	{
	 name :: binary(),
	 type = uint16 :: uint16,
	 start_pos :: non_neg_integer(),
	 end_pos   :: non_neg_integer()
	}).

-record(dbf,
	{
	 fd,
	 db_name :: atom(),
	 db_type = term :: term | json | cson | erlang, %% csv, xml
	 start_pos :: non_neg_integer(),
	 end_pos   :: non_neg_integer(),
	 ind = [] :: [#dbi{}],
	 cache = undefined :: undefined | ets:table()
	}).

-define(DB_SERV,  bt_db_serv).   %% registered server process
-define(DB_TABLE, bt_db_table).  %% name meta table

%% start the "meta" table server
start() ->
    case whereis(?DB_SERV) of
	undefined -> 
	    StartRef = make_ref(),
	    Starter = self(),
	    {Serv,Mon} = 
		spawn_monitor(
		  fun() ->
			  register(?DB_SERV, self()),
			  ets:new(?DB_TABLE, [named_table]),
			  reply([Starter|StartRef], ok),
			  db_serv_loop(#{})
		  end),
	    receive
		{reply, StartRef, ok} ->
		    demonitor(Mon, [flush]),
		    Serv;
		{reply, StartRef, {error, ealready}} ->
		    demonitor(Mon, [flush]),
		    whereis(?DB_SERV);
		{'DOWN', Mon, process, Serv, _Reason} ->
		    whereis(?DB_SERV)
	    end;
	Pid ->
	    Pid
    end.

-type db_open_option() :: 
	{dbdir, Dir::string()} |
	{dbfile, Filename::string()}.
-type db_open_options() :: [db_open_option()].

open(DbName) ->
    open(DbName, []).
-spec open(DbName::atom(), Options::db_open_options()) ->
	  {ok,Name::string()} | {error, Reason::string()}.
open(DbName, Options) when is_atom(DbName) ->
    Serv = start(),
    call(Serv, {open, DbName, Options}).

close(DbName) when is_atom(DbName) ->
    call(?DB_SERV, {close, DbName}).

-spec lookup(DbName::atom(), Key::term) ->
	  {ok, Object::map()} | {error, enoent} | {error, Reason::term()}.

lookup(DbName, Key) when is_atom(DbName) ->
    Pid = case whereis(?DB_SERV) of
	      undefined -> start();
	      Serv -> Serv
	  end,
    case ets:lookup(?DB_TABLE, DbName) of
	[] ->
	    call(Pid, {open, DbName, []}),
	    KeyName = get_index_name(DbName),
	    call(Pid, {lookup, DbName, KeyName, Key});
	[{_, Cache}] ->
	    KeyName = get_index_name(DbName),
	    CKey = {KeyName, Key},
	    case ets:lookup(Cache, CKey) of
		[] ->
		    call(Pid, {lookup, DbName, KeyName, Key});
		[{_, Pos}] ->
		    case ets:lookup(Cache, Pos) of
			[] ->
			    call(Pid, {lookup, DbName, KeyName, Key});
			[{_, Object}] ->
			    {ok, Object}
		    end
	    end
    end.

call(Serv, Request) when is_pid(Serv) ->
    Ref = erlang:monitor(process, Serv),
    Serv ! {call, [self()|Ref], Request},
    receive
	{'DOWN', Ref, process, Serv, Reason} ->
	    {error, Reason};
	{reply, Ref, Response} ->
	    Response
    end;
call(?DB_SERV, Request) ->
    case whereis(?DB_SERV) of
	undefined -> {error, not_started};
	Pid -> call(Pid, Request)
    end.

reply([Caller|Ref], Response) ->
    Caller ! {reply, Ref, Response}.

%% meta server loop

-type db_serv_state() :: #{ DbName::atom() => #dbf{} }.

-spec db_serv_loop(St::db_serv_state()) -> ok.

db_serv_loop(St) ->
    receive
	{call, From, Request} ->
	    {Reply,St1} = handle_call(Request, St),
	    reply(From, Reply),
	    db_serv_loop(St1)
    end.

handle_call({open, DbName, Options}, St) ->
    case maps:get(DbName, St, undefined) of
	undefined ->
	    DbDir = case proplists:get_value(dbdir, Options) of
			undefined -> code:priv_dir(bt);
			Dir -> Dir
		    end,
	    DbFile = case proplists:get_value(dbfile, Options) of
			 undefined ->
			     atom_to_list(DbName) ++ ".db";
			   Name -> Name
		     end,
	    Filename = filename:join(DbDir, DbFile),
	    try open_dbf_(Filename) of
		{ok, Dbf} ->
		    Dbf1 = Dbf#dbf { db_name = DbName },
		    ets:insert(?DB_TABLE, {DbName,Dbf1#dbf.cache}),
		    {{ok, DbName}, St#{ DbName => Dbf1 }};
		Error ->
		    {Error, St}
	    catch
		error:Reason ->
		    {{error,Reason}, St}
	    end;
	_Dbf -> %% {error, eexist} ? nah just go on!
	    {{ok, DbName}, St}
    end;
handle_call({close, DbName}, St) ->
    case maps:take(DbName, St) of
	{Dbf, St1} ->
	    case close_dbf_(Dbf) of
		ok ->
		    ets:delete(?DB_TABLE, DbName),
		    St1 = maps:remove(DbName, St),
		    {ok, St1};
		Error ->
		    {Error, St}
	    end;
	error ->
	    {{error, enoent}, St}
    end;
handle_call({lookup, DbName, KeyName, Key}, St) ->
    %% io:format("handle_call: lookup ~p, ~p, ~p\n", [DbName,KeyName,Key]),
    case maps:get(DbName, St, undefined) of
	undefined  ->
	    {{error, einval}, St}; %% db not open!
	Dbf ->
	    Reply = lookup_dbf_(Dbf, KeyName, Key),
	    {Reply, St}
    end;
handle_call(_Call, St) ->
    {{error, badcall}, St}.

open_dbf_(Filename) ->
    {ok, Fd} = file:open(Filename, [read, binary]),
    {ok, EndPos0} = file:position(Fd, {eof, -4}),
    {ok, <<InitPos:32>>} = file:pread(Fd,EndPos0,4),
    {IndEnd,IndList0} = read_ind_poses(Fd, EndPos0-4, []),
    EndPos = case IndList0 of
		 [] -> EndPos0 - 4;
		 [#dbi{start_pos=IStartPos1}|_] -> IStartPos1-4
	     end,
    {ok,_} = file:position(Fd, InitPos),
    {ok, DbType} = read_object(Fd, atom),
    IndList1 = read_ind_names(Fd, IndList0, IndEnd),
    %% we need to skip an end table marker here ! so add 4
    {ok, StartPos} = file:position(Fd, {cur,4}),
    Cache = ets:new(bt_db_cache, [set]),
    ets:insert(Cache, {'_INDEX', IndList1}),
    {ok, #dbf{ fd = Fd,
	       db_type = DbType,
	       start_pos     = StartPos,
	       end_pos       = EndPos,
	       ind           = IndList1,
	       cache         = Cache
	     }}.

read_ind_poses(Fd, Pos, Acc) ->
    case file:pread(Fd,Pos,4) of
	{ok, <<0:32>>} ->
	    {Pos,lists:reverse(Acc)};
	{ok, <<Start:32>>} ->
	    read_ind_poses(Fd, Pos-4,
			   [#dbi{start_pos=Start}|Acc])
    end.

%% names are stored in order index1, index2,...indexn
read_ind_names(_Fd, [], _IndEnd) ->
    [];
read_ind_names(Fd, [Ind], IndEnd) ->
    case read_object(Fd, string) of
	eof -> 
	    [];
	{ok, Name} ->
	    [Ind#dbi{name=Name, end_pos=IndEnd}]
    end;
read_ind_names(Fd, [Ind | IndList=[Nxt|_]], IndEnd) ->
    case read_object(Fd, string) of
	eof -> 
	    [];
	{ok, Name} ->
	    [Ind#dbi{name=Name,end_pos=Nxt#dbi.start_pos}|
	     read_ind_names(Fd,IndList,IndEnd)]
    end.

close_dbf_(#dbf{fd=undefined}) -> ok;
close_dbf_(#dbf{fd=Fd}) ->
    file:close(Fd).

%% unly key types uint16 right now
lookup_dbf_(Dbf=#dbf{ind = [Ind]}, KeyName, Key) 
  when Ind#dbi.type =:= uint16,
       Ind#dbi.name =:= KeyName,
       is_integer(Key) ->
    CKey = {KeyName,Key},
    %% io:format("lookup: binary ckey=~p, dbf=~p\n", [CKey, Dbf]),
    case ets:lookup(Dbf#dbf.cache, CKey) of
	[] ->
	    case binary_search(Dbf, KeyName, Key) of
		false ->
		    {error, enoent};  %% cache?
		{ok,Pos} ->
		    read_and_cache(Dbf, Pos, CKey)
	    end;
	[{_, Pos}] ->
	    case ets:lookup(Dbf#dbf.cache, Pos) of
		[] ->
		    read_and_cache(Dbf, Pos, CKey);
		[{_, Object}] ->
		    {ok, Object}
	    end
    end;

lookup_dbf_(Dbf, KeyName, Key) ->
    CKey = {KeyName,Key},
    %% io:format("lookup: linear ckey=~p, dbf=~p\n", [CKey, Dbf]),
    case ets:lookup(Dbf#dbf.cache, CKey) of
	[] ->
	    case linear_search(Dbf, KeyName, Key) of
		Err ={error, _} ->
		    Err;
		false ->
		    {error, enoent};
		{cached, _Pos, Object} ->
		    {ok, Object};
		{ok, Pos, Object} ->
		    cache_object(Dbf, Pos, CKey, Object)
	    end;
	[{_, Pos}] ->
	    case ets:lookup(Dbf#dbf.cache, Pos) of
		[] ->
		    read_and_cache(Dbf, Pos, CKey);
		[{_, Object}] ->
		    {ok, Object}
	    end
    end.

read_and_cache(Dbf, Pos, CKey) ->
    {ok,_} = file:position(Dbf#dbf.fd, Pos), 
    case read_object(Dbf#dbf.fd, Dbf#dbf.db_type) of
	{ok,Object} ->
	    cache_object(Dbf, Pos, CKey, Object);
	Error ->
	    Error
    end.

cache_object(Dbf, Pos, CKey, Object) ->
    ets:insert(Dbf#dbf.cache, {Pos,Object}),
    ets:insert(Dbf#dbf.cache, {CKey,Pos}),
    {ok,Object}.


dump_index(Dbf=#dbf{ind=IndList}, Index) when is_binary(Index) ->
    case lists:keysearch(Index, #dbi.name, IndList) of
	false -> {error, enoent};
	{value,Dbi} ->
	    io:format("type: ~p\n", [Dbi#dbi.type]),
	    {ok, IndBin} = 
		file:pread(Dbf#dbf.fd, 
			   Dbi#dbi.start_pos,
			   (Dbi#dbi.end_pos - Dbi#dbi.start_pos)),
	    _ = [
		 io:format("key=~w, offset=~w\n", [Key,Offset])
		|| <<Key:16, Offset:32>> <= IndBin],
	    ok
    end.

read_object(Fd, DbType) ->
    case file:read(Fd, 4) of
	{ok, <<0:32>>} ->
	    eof;
	{ok, <<Size:32>>} ->
	    case file:read(Fd, Size) of
		{ok, Bin} ->
		    {ok,decode(Bin, DbType)};
		Error ->
		    Error
	    end;
	Error ->
	    Error
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

%%
%% Linear search over data base
%% only cache the items that we search for!
%% (do we want negative caching, probably)
%%    
linear_search(Dbf, KeyName, Key) ->
    {ok, _} = file:position(Dbf#dbf.fd, Dbf#dbf.start_pos),
    linear_search_(Dbf, KeyName, Key).

linear_search_(Dbf, KeyName, Key) ->
    {ok, Pos} = file:position(Dbf#dbf.fd, cur),
    case ets:lookup(Dbf#dbf.cache, Pos) of
	[] ->
	    case read_object(Dbf#dbf.fd, Dbf#dbf.db_type) of
		{ok,Entry} ->
		    case proplists:get_value(KeyName, Entry, undefined) of
			Key -> 
			    {ok, Pos, Entry};
			_ ->
			    linear_search_(Dbf, KeyName, Key)
		    end;
		Error ->
		    Error
	    end;
	[{_, Entry}] ->
	    {cached, Pos, Entry}
    end.

-define(RECORD_SIZE, (2+4)). %% uuid16 + offset32
%% search index for Position in Db file
%% fixme: add search when index is in db-file
binary_search(#dbf{fd=Fd,ind=IndList}, Index, Key) ->
    case lists:keysearch(Index, #dbi.name, IndList) of
	false ->
	    {error, enoent};
	{value,Dbi} ->
	    Len = Dbi#dbi.end_pos - Dbi#dbi.start_pos,
	    Num = Len div ?RECORD_SIZE,
	    binary_search_(Fd, Dbi#dbi.start_pos, 0, Num-1, Key)
    end.
	    
binary_search_(Fd, StartPos, L, R, Key) when L =< R ->
    I = (L + R) div 2,
    {Key1, Offset} = read_index(Fd, I, StartPos, uint16),
    if Key1 < Key ->
	    binary_search_(Fd, StartPos, I+1, R, Key);
       Key1 > Key ->
	    binary_search_(Fd, StartPos, L, I-1, Key);
       true ->
	    {ok, Offset}
    end;
binary_search_(_Fd, _StartPos, _L, _R, _Key) ->
    false.

read_index(Fd, I, StartPos, uint16) ->
    {ok, <<Key:16, Offset:32>>} = file:pread(Fd,StartPos+I*6, 6),
    {Key, Offset};
read_index(Fd, I, StartPos, uint32) ->
    {ok, <<Key:32, Offset:32>>} = file:pread(Fd,StartPos+I*8, 8),
    {Key, Offset};
read_index(Fd, I, StartPos, uint64) ->
    {ok, <<Key:64, Offset:32>>} = file:pread(Fd,StartPos+I*12, 12),
    {Key, Offset}.

%% get FIRST index! (fixme remove <<"uuid">> stuff
get_index_name(DbName) ->    	
    case ets:lookup(?DB_TABLE, DbName) of    
	[] -> <<"uuid">>;
	[{_, Cache}] ->
	    case ets:lookup(Cache,'_INDEX') of
		[] -> <<"uuid">>;
		[{_, [#dbi{name=KeyName}|_]}] -> KeyName
	    end
    end.

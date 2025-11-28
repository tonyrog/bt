%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%    Lookup manufaturer name from out.db
%%% @end
%%% Created : 25 Nov 2025 by Tony Rogvall <tony@rogvall.se>

-module(bt_oui).

-export([manuf_from_addr/1]).
-export([manuf_from_oui/1]).
-export([show/0, show/1, show/2]).

-define(OUI_DB_FILENAME, filename:join(code:priv_dir(bt), "oui.db")).
-define(RECORD_SIZE, 128).

manuf_from_addr({A,B,C,_D,_E,_F}) ->
    OUI = (A*256 + B)*256 + C,
    manuf_from_oui(OUI);
manuf_from_addr(<<A,B,C,_D,_E,_F>>) ->
    OUI = (A*256 + B)*256 + C,
    manuf_from_oui(OUI);
manuf_from_addr([A1,A0,$:,B1,B0,$:,C1,C0|_]) ->
    OUI = list_to_integer([A1,A0,B1,B0,C1,C0], 16),
    manuf_from_oui(OUI);    
manuf_from_addr([A1,A0,$-,B1,B0,$-,C1,C0|_]) ->
    OUI = list_to_integer([A1,A0,B1,B0,C1,C0], 16),
    manuf_from_oui(OUI).

manuf_from_oui(OUI) when is_integer(OUI), OUI >= 16#000000, OUI =< 16#ffffff ->
    {ok, Fd} = file:open(?OUI_DB_FILENAME, [read, binary]),
    try binary_search_(Fd, OUI) of
	Result ->
	    Result
    after
	file:close(Fd)
    end.

-ifdef(not_used).
linear_search_(Fd, OUI) ->
    case file:read(Fd, ?RECORD_SIZE) of
	{ok, <<OUI:24, Len, Name:Len/binary, _/binary>>} ->
	    {ok,Name};
	{ok, _} ->
	    linear_search_(Fd, OUI);
	eof ->
	    {error, enoent}
    end.
-endif.

binary_search_(Fd, OUI) ->
    {ok,Len0} = file:position(Fd, eof),
    Len = Len0 div ?RECORD_SIZE,
    binary_search_(Fd, 0, Len-1, OUI).
	    
binary_search_(Fd, L, R, OUI) when L =< R ->
    I = (L + R) div 2,
    {ok,<<VAL:24>>} = file:pread(Fd,I*?RECORD_SIZE, 3),
    if VAL < OUI ->
	    binary_search_(Fd, I+1, R, OUI);
       VAL > OUI ->
	    binary_search_(Fd, L, I-1, OUI);
       true ->
	    {ok, <<OUI:24, Len, Name:Len/binary, _/binary>>} =
		file:pread(Fd,I*?RECORD_SIZE, ?RECORD_SIZE),
	    {ok, Name}
    end;
binary_search_(_Fd, _L, _R, _OUT) ->
    false.

%% list all records in range
show() -> show(0, 1000000).
show(Ent) -> show(Ent, Ent).
show(From, To) when is_integer(From), From >= 0,
		    is_integer(To), To >= 0, To >= From ->
    {ok,Fd} = file:open(?OUI_DB_FILENAME, [read, binary]),
    show_(Fd, From, To).

show_(Fd, I, N) when I =< N ->
    case file:pread(Fd,I*?RECORD_SIZE, ?RECORD_SIZE) of
	{ok, <<OUI:24, Len, Name:Len/binary, _/binary>>} ->
	    io:format("16#~6.16.0B: ~ts\n", [OUI, Name]),
	    show_(Fd, I+1, N);
	eof ->
	    ok
    end;
show_(_Fd, _I, _N) ->
    ok.



		
    
		    
		     

%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%    YAML scanner - anough to get assigned numbers
%%% @end
%%% Created : 29 Nov 2025 by Tony Rogvall <tony@rogvall.se>

-module(bt_yaml).

-export([file/1]).
-export([scan_file/1]).
-export([print_file/1]).

-export([to_term/1]).
-export([parse/1]).
-export([tokens/1]).

-type yaml() :: [{yaml_key(), yaml_value()}].
-type yaml_key() :: string().
-type yaml_value() :: map() | list().
-type token() :: {string,string()} | {integer,integer()} |
		 {float, float()} | {boolean, boolean()} |
		 ':' | '-' | '---' | '...' |
		 '>' | '>-' | '>+' | '|' | '|-' | '|+'.
		 

-define(is_digit(C), (((C) >= $0) andalso ((C) =< $9))).

-define(is_xdigit(C), ((((C) >= $0) andalso ((C) =< $9)) 
		       orelse
			 (((C) >= $a) andalso ((C) =< $f))
		       orelse
			 (((C) >= $A) andalso ((C) =< $F)))).

-spec file(Filename::file:name_all()) -> [yaml()] | {error, Reason::term()}.

file(Filename) ->
    case file:open(Filename, [binary, read]) of
	{ok, Fd} ->
	    try scan(Fd) of
		Ts -> parse(Ts)
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.

-spec scan_file(Filename::file:name_all()) -> 
	  [{Ln::pos_integer(),Indent::non_neg_integer(),[token()]}] |
	  {error, Reason::term()}.

scan_file(Filename) ->
    case file:open(Filename, [binary, read]) of
	{ok, Fd} ->
	    try scan(Fd) of
		Ts -> Ts
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.


%% convert a parsed yaml structure into an erlang term
-spec to_term(yaml()) -> term().
to_term({string,Str}) ->  unicode:characters_to_binary(Str);
to_term({integer,Int}) -> Int;
to_term({float,Flt}) -> Flt;
to_term({boolean,Bool}) -> Bool;
to_term({Key,Value}) -> {to_term(Key), to_term(Value)};
to_term([]) -> [];
to_term([E | Es]) -> [to_term(E) | to_term(Es)].

-spec print_file(Filename::file:name_all()) ->
	  ok.
print_file(Filename) ->
    try file(Filename) of
	Elems ->
	    print_elems(0, Elems)
    catch
	error:Reason ->
	    {error, Reason}
    end.

%% print parsed yaml 
print_elems(I, [E|Es]) ->
    print_elem(I, E),
    print_elems(I, Es);
print_elems(_I, []) ->
    ok.

print_elem(I, {Key,Value}) when is_list(Value) ->
    indent(I),
    print_value(I, Key), io:put_chars(":\n"),
    print_elems(I+1, Value);
print_elem(I, {Key,Value}) ->
    indent(I),
    print_value(I, Key), io:put_chars(": "), print_value(I, Value),
    io:put_chars("\n");
print_elem(I, Value) ->
    indent(I), print_value(I, Value), io:put_chars("\n").

print_value(_I,{string,Str}) -> 
    io:format("~ts", [Str]);
print_value(_I,{integer,Int}) -> 
    io:put_chars(integer_to_list(Int));
print_value(_I,{float,Flt}) ->
    io:put_chars(io_lib_format:fwrite_g(Flt));
print_value(_I,{boolean,Bool}) ->
    io:format("~w", [Bool]);
print_value(I, [{Key,Value}|Elems]) ->
    io:put_chars("- "), print_value(I, Key), io:put_chars(": "),
    print_value(I, Value), io:put_chars("\n"),
    print_elems(I+1, Elems).

indent(I) -> io:put_chars([lists:duplicate(2*I, $\s)]).

parse(Lns) ->
    {List, _} = parse_elem(Lns, 0, []),
    lists:reverse(List).

%% FIXME: implement and indentation stack!
%% Check if we should continue parsing at this level or return (pop up).
%% If the next line has lower indentation than CurrInd, we should return.
should_continue([{_,NextI,_}|_], CurrInd) -> NextI >= CurrInd;
should_continue([], _CurrInd) -> true.

parse_elem(Lns0=[{_Ln,Ind,['-', Key, ':', Value]} | Lns], CurrInd, Acc) ->
    %% List items must have Ind >= CurrInd
    if Ind >= CurrInd ->
	    {List,Lns1} = parse_elem(Lns, Ind+1, []),
	    Value1 = [{Key,Value}|lists:reverse(List)],
	    %% Continue at Ind level to parse next list items at same level
	    case should_continue(Lns1, Ind) of
		true -> parse_elem(Lns1, Ind, [Value1|Acc]);
		false -> {[Value1|Acc], Lns1}
	    end;
       true ->
	    {Acc, Lns0}
    end;
parse_elem(Lns0=[{_Ln,Ind,[Key,':']} | Lns], CurrInd, Acc) ->
    if Ind >= CurrInd ->
	    %% For Key: without value, parse nested content starting from Ind+1
	    %% This will catch both nested mappings and lists
	    {List, Lns1} = parse_elem(Lns, Ind+1, []),
	    Value = {Key, lists:reverse(List)},
	    case should_continue(Lns1, CurrInd) of
		true -> parse_elem(Lns1, CurrInd, [Value|Acc]);
		false -> {[Value|Acc], Lns1}
	    end;
       true ->
	    {Acc, Lns0}
    end;

parse_elem(Lns0=[{_Ln,Ind,['-', Value]} | Lns], CurrInd, Acc) ->
    %% List items must have Ind >= CurrInd
    if Ind >= CurrInd ->
	    %% Continue at Ind level (where the list items are)
	    parse_elem(Lns, Ind, [Value|Acc]);
       true ->
	    {Acc, Lns0}
    end;

parse_elem(Lns0 = [{_Ln,Ind,[Key, ':', Value]} | Lns], CurrInd, Acc) ->
    if Ind >= CurrInd ->
	    parse_elem(Lns, CurrInd, [{Key,Value}|Acc]);
       true ->
	    {Acc, Lns0}
    end;

parse_elem([{Ln,Ind, ['---' | _]} | Lns], CurrInd, Acc) ->
    {List, Lns1} = parse_elem(Lns, Ind+1, []),
    Value = {doc,Ln,lists:reverse(List)},
    case should_continue(Lns1, CurrInd) of
	true -> parse_elem(Lns1, CurrInd, [Value|Acc]);
	false -> {[Value|Acc], Lns1}
    end;

parse_elem([{_Ln,Ind, ['...'|_]}|Lns], Ind, Acc) ->
    {Acc, Lns};
parse_elem([], _Ind, Acc) ->
    {Acc, []}.
		
scan(Fd) ->
    scan(Fd, 1, []).

scan(Fd, Ln, Acc) ->
    case read_line(Fd) of 
	{ok, Line} ->
	    scan_line(Fd, Ln, Line, Acc);
	eof ->
	    lists:reverse(Acc);
	Error ->
	    Error
    end.

scan_line(Fd, Ln, Line, Acc) ->
    case tokens(Line) of
	{_, []} ->
	    scan(Fd, Ln+1, Acc);
	{_, [_I]} ->
	    scan(Fd, Ln+1, Acc);
	{Last,[Ind|Ts]} ->
	    case Last of
		'>'  -> scan_block(Fd, Ln, Ind, Ts, fold, clip, Acc);
		'>-' -> scan_block(Fd, Ln, Ind, Ts, fold, strip, Acc);
		'>+' -> scan_block(Fd, Ln, Ind, Ts, fold, keep, Acc);
		'|'  -> scan_block(Fd, Ln, Ind, Ts, preserve, clip, Acc);
		'|-' -> scan_block(Fd, Ln, Ind, Ts, preserve, strip, Acc);
		'|+' -> scan_block(Fd, Ln, Ind, Ts, preserve, keep, Acc);
		_ -> scan(Fd, Ln+1, [{Ln,Ind,Ts}|Acc])
	    end
    end.

trim_block_ts([Key, ':', _Style]) -> [Key,':'].

%% scan first block
scan_block(Fd, Ln, CurrInd, Ts, Style, Chomp, Acc) ->
    case read_line(Fd) of
	{ok, Line} ->
	    {Ind, Cs} = scan_ind(Line),
	    Lines0 = join(Cs, [], Style, Chomp),
	    Ts1 = trim_block_ts(Ts),
	    scan_block(Fd, Ln, Ind, Ts1, Style, Chomp, Acc, Ln, Lines0);
	eof ->
	    lists:reverse([{Ln,CurrInd,Ts}|Acc]);
	Error ->
	    Error
    end.

scan_block(Fd, Ln, CurrInd, Ts, Style, Chomp, Acc, Ln0, Lines) ->
    case read_line(Fd) of
	{ok, Line} ->
	    {Ind, Cs} = strip_ind(Line, CurrInd),
	    if Ind =:= CurrInd ->
		    scan_block(Fd, Ln+1, CurrInd, Ts, Style, Chomp,
			       Acc, Ln0, join(Cs, Lines, Style, Chomp));
	       Ind =:= 0, Cs =:= "\n" ->
		    scan_block(Fd, Ln+1, CurrInd, Ts, Style, Chomp,
			       Acc, Ln0, join(Cs, Lines, Style, Chomp));
	       true ->
		    scan(Fd, Ln, [{Ln0,CurrInd,Ts++make_block(Chomp,Lines)}|Acc])
	    end;
	eof ->
	    lists:reverse([{Ln0,CurrInd,Ts++make_block(Chomp,Lines)}|Acc]);
	Error ->
	    Error
    end.

make_block(Chomp, Lines) ->
    %% io:format("block: chomp=~w, lines=~p\n", [Chomp, Lines]),
    make_block_(Chomp, Lines).

make_block_(clip,["\n"|Lines]) -> make_block_(clip,Lines);
make_block_(strip,["\n"|Lines]) -> make_block_(strip,Lines);
make_block_(clip,Lines) -> 
    [{string,lists:append(lists:reverse(Lines))}];
make_block_(strip,Lines) ->
    [{string,string:trim(lists:append(lists:reverse(Lines)),trailing)}];
make_block_(keep,Lines) ->
    [{string,lists:append(lists:reverse(Lines))}].

join([$\n], Lines, fold, _Chomp) -> ["\n" | Lines];
join(Cs, [], fold, _Chomp) -> [string:trim(Cs,trailing)];
join(Cs, [Cs1|Lines], fold, _Chomp) -> 
    [Cs1++[$\s|string:trim(Cs,trailing)]|Lines];
join(Cs, Lines, preserve, _Chomp) -> [Cs | Lines];

join(Cs, Lines, _Style, _Chomp) ->
    [Cs|Lines].

%% read a line of unicode characters
read_line(Fd) ->
    case file:read_line(Fd) of
	{ok, Data} when is_binary(Data) ->
	    {ok, unicode:characters_to_list(Data)};
	eof ->
	    eof;
	Error -> Error
    end.

%% scan indentaion level
scan_ind(Cs) -> scan_ind(Cs, 0).
scan_ind([$\s|Cs], I) -> scan_ind(Cs, I+1);
scan_ind([$\t|Cs], I) -> scan_ind(Cs, I);
scan_ind(Cs, I) -> {I, Cs}.

strip_ind(Cs, N) -> strip_ind(Cs, 0, N).
strip_ind(Cs, I, 0) -> {I, Cs};
strip_ind([$\s|Cs], I, N) -> strip_ind(Cs, I+1, N-1);
strip_ind([$\t|Cs], I, N) -> strip_ind(Cs, I, N);
strip_ind(Cs, I,_N) -> {I, Cs}.

tokens(Chars) when is_list(Chars) -> tokens0(Chars,0).

tokens0([$\s|Cs], I) -> tokens0(Cs, I+1);
tokens0([$\t|Cs], I) -> tokens0(Cs, I);
tokens0(Cs, I) -> tokens(Cs, [I]).

tokens([$\s|Cs], Ts) -> tokens(Cs, Ts);
tokens([$\t|Cs], Ts) -> tokens(Cs, Ts);
tokens([$\n], Ts) -> token1(Ts);
tokens([], Ts) -> token1(Ts);
tokens(Cs, Ts) -> token(Cs, Ts).

token([], Ts) -> token1(Ts);
token([$#,$\s|_], Ts) -> token1(Ts);
token([$:,$\s|Cs], Ts) -> tokens(Cs, [':'|Ts]);
token([$:,$\n|Cs], Ts) -> tokens(Cs, [':'|Ts]);
token([$>,$+|Cs], Ts) -> tokens(Cs, ['>+'|Ts]);
token([$>,$-|Cs], Ts) -> tokens(Cs, ['>-'|Ts]);
token([$>|Cs], Ts) -> tokens(Cs, ['>'|Ts]);
token([$|,$+|Cs], Ts) -> tokens(Cs, ['|+'|Ts]);
token([$|,$-|Cs], Ts) -> tokens(Cs, ['|-'|Ts]);
token([$||Cs], Ts) -> tokens(Cs, ['|'|Ts]);
token([$-,$-,$-|Cs], Ts) -> tokens(Cs, ['---'|Ts]);
token([$-,$\s|Cs], Ts) -> tokens(Cs, ['-'|Ts]);
token([$.,$.,$.|Cs], Ts) -> tokens(Cs, ['...'|Ts]);
token([$"|Cs], Ts) -> dquote(Cs, [], Ts);
token([$'|Cs], Ts) -> squote(Cs, [], Ts);
token([C|Cs], Ts) -> chars(Cs, [C], Ts).

token1([]) -> {none, []};
token1([T]) -> {T, [T]};
token1(Ts=[T|_]) -> {T,lists:reverse(Ts)}.

squote([$',$'|Cs], Lit, Ts) -> squote(Cs, [$'|Lit], Ts);
squote([$'|Cs], Lit, Ts) -> tokens(Cs, [make_string(lists:reverse(Lit))|Ts]);
squote([C|Cs], Lit, Ts) -> squote(Cs, [C|Lit], Ts);
%% error
squote([], Lit, Ts) -> tokens([], [make_string(lists:reverse(Lit))|Ts]).

dquote([$"|Cs], Lit, Ts) ->
    tokens(Cs, [make_string(lists:reverse(Lit))|Ts]);
dquote([$\\|Cs], Lit, Ts) ->
    case Cs of
	[$0|Cs1] -> dquote(Cs1, [0|Lit], Ts);
	[$a|Cs1] -> dquote(Cs1, [$\a|Lit], Ts);	
	[$b|Cs1] -> dquote(Cs1, [$\b|Lit], Ts);	
	[$t|Cs1] -> dquote(Cs1, [$\t|Lit], Ts);
	[$n|Cs1] -> dquote(Cs1, [$\n|Lit], Ts);
	[$v|Cs1] -> dquote(Cs1, [$\v|Lit], Ts);
	[$f|Cs1] -> dquote(Cs1, [$\f|Lit], Ts);
	[$r|Cs1] -> dquote(Cs1, [$\r|Lit], Ts);
	[$e|Cs1] -> dquote(Cs1, [$\e|Lit], Ts);
	[$\s|Cs1] -> dquote(Cs1, [$\s|Lit], Ts);
	[$"|Cs1] -> dquote(Cs1, [$"|Lit], Ts);
	[$\\|Cs1] -> dquote(Cs1, [$\\|Lit], Ts);
	[$/|Cs1] -> dquote(Cs1, [$/|Lit], Ts);
	%% N next line unicode
	%% non-breaking space xA0

	[$_|Cs1] -> dquote(Cs1, [16#A0|Lit], Ts);
	[$L|Cs1] -> dquote(Cs1, [16#2028|Lit], Ts);
	[$P|Cs1] -> dquote(Cs1, [16#2029|Lit], Ts);

	[$x,X1,X0|Cs1] when ?is_xdigit(X1),?is_xdigit(X0) ->
	    dquote(Cs1, [list_to_integer([X1,X0],16) | Lit], Ts);
	[$u,X3,X2,X1,X0|Cs1] when ?is_xdigit(X3),?is_xdigit(X2),
				  ?is_xdigit(X1),?is_xdigit(X0) ->
	    dquote(Cs1,[list_to_integer([X3,X2,X1,X0],16) | Lit], Ts);
	[$u,X7,X6,X5,X4,X3,X2,X1,X0|Cs1] when 
	      ?is_xdigit(X7),?is_xdigit(X6),
	      ?is_xdigit(X5),?is_xdigit(X4),
	      ?is_xdigit(X3),?is_xdigit(X2),
	      ?is_xdigit(X1),?is_xdigit(X0) ->
	    dquote(Cs1, [list_to_integer([X3,X2,X1,X0],16) | Lit], Ts);
	[C|Cs1] -> dquote(Cs1, [C|Lit], Ts)
    end;
dquote([C|Cs], Lit, Ts) -> dquote(Cs, [C|Lit], Ts);
%% error
dquote([], Lit, Ts) -> tokens([], [make_string(lists:reverse(Lit))|Ts]).


chars([$#,$\s|_Cs], Str, Ts) ->
    tokens([], [chars_value(Str)|Ts]);
chars([$:,$\s|Cs], Str, Ts) ->
    tokens(Cs, [':',chars_value(Str)|Ts]);
chars([$:,$\n|Cs], Str, Ts) ->
    tokens(Cs, [':',chars_value(Str)|Ts]);
chars(Cs0=[C|Cs], Str, Ts) ->
    case lists:member(C, "\n>|&") of
	true ->
	    tokens(Cs0, [chars_value(Str)|Ts]);
	false ->
	    chars(Cs, [C|Str], Ts)
    end;
chars([], Str, Ts) -> tokens([], [chars_value(Str)|Ts]).

chars_value(Str) ->
    make_value(lists:reverse(string:trim(Str,leading))).

make_value("True") -> {boolean,true};
make_value("False") -> {boolean,false};
make_value("true") -> {boolean,true};
make_value("false") -> {boolean,false};
make_value("TRUE") -> {boolean,true};
make_value("FALSE") -> {boolean,false};

make_value("Yes") -> {boolean,true};
make_value("No") -> {boolean,false};
make_value("yes") -> {boolean,true};
make_value("no") -> {boolean,false};
make_value("YES") -> {boolean,true};
make_value("NO") -> {boolean,false};

make_value("On") -> {boolean,true};
make_value("Off") -> {boolean,false};
make_value("on") -> {boolean,true};
make_value("off") -> {boolean,false};
make_value("ON") -> {boolean,true};
make_value("OFF") -> {boolean,false};

make_value(Cs0=[C|_]) when C >= $0, C =< $9 ->
    make_number(Cs0);
make_value(Cs0=[$-,C|_]) when C >= $0, C =< $9 ->
    make_number(Cs0);
make_value(Cs) ->
    make_string(Cs).

make_number(Cs0=[$0,$x|Cs]) ->
    try list_to_integer(Cs, 16) of
	Int -> {integer,Int}
    catch
	error:badarg -> make_string(Cs0)
    end;
make_number(Cs) ->
    try list_to_float(Cs) of
	Flt -> {float,Flt}
    catch
	error:badarg ->
	    Base = if hd(Cs) =:= $0 -> 8; true -> 10 end,
	    try list_to_integer(Cs, Base) of
		Int -> {integer,Int}
	    catch
		error:badarg ->
		    make_string(Cs)
	    end
    end.

make_string(Cs) ->
    {string,Cs}.

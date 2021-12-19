%% bluetooth l2cap nif
-module(bt_l2cap).

-on_load(init/0).

-export([
	 open_/0,
	 bind_/3,
	 close/1,
	 listen_/1,
	 connect_/3,
	 accept_/1,
	 set_mtu/2,
	 get_mtu/1,
	 getsockname/1,
	 getpeername/1,
	 write_/2,
	 read_/1,
	 select_/2
	]).

-export([preloaded_atoms_/0]). % internal

-type addr() :: {byte(),byte(),byte(),byte(),byte(),byte()} |
		binary().
-type psm() :: 0..65535.
-type handle() :: reference().
-type reason() :: atom().

-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

%%
%% Exported: init
%%

init() ->
    ok = erlang:load_nif(
           filename:join(code:priv_dir(bt), bt_l2cap_nif), none).


preloaded_atoms_() ->
    [
     ok, error, undefined, select, read, write, 
     no_such_handle
    ].


-spec open_() -> {ok, handle()} | {error, reason()}.
open_() ->
    ?nif_stub.

-spec bind_(Handle::handle(), Addr::addr(), Psm::psm()) ->
	  ok | {error, reason()}.
bind_(_Handle, _Addr, _Psm) ->
    ?nif_stub.

-spec close(Handle::handle()) -> ok | {error, reason()}.
close(_Handle) ->
    ?nif_stub.

-spec listen_(Handle::handle()) ->
	  ok | {error, reason()}.
listen_(_Handle) ->
    ?nif_stub.

-spec connect_(Handle::handle(), Addr::addr(), Psm::psm()) ->
	  ok | {error, reason()}.
connect_(_Handle, _Addr, _Psm) ->
    ?nif_stub.

-spec accept_(Handle::handle()) -> {ok, PeerHandle::handle()} |
	  {error, reason()}.
accept_(_Handle) ->
    ?nif_stub.

-spec set_mtu(Handle::handle(), Mtu::integer()) -> ok | {error, reason()}.
set_mtu(_Handle, _Mtu) ->
    ?nif_stub.

-spec get_mtu(Handle::handle()) -> {ok, Mtu::integer()} | {error, reason()}.
get_mtu(_Handle) ->
    ?nif_stub.

-spec getsockname(Handle::handle()) -> 
	  {ok,{Addr::addr(),Psm::psm()}} | {error, reason()}.
getsockname(_Handle) ->
    ?nif_stub.

-spec getpeername(Handle::handle()) -> 
	  {ok,{Addr::addr(),Psm::psm()}} | {error, reason()}.
getpeername(_Handle) ->
    ?nif_stub.

-spec write_(Handle::handle(), Data::iolist()) ->
	  {ok, Size::integer()} | {error, reason()}.

write_(_Handle, _Data) ->
    ?nif_stub.

-spec read_(Handle::handle()) ->
	  {ok, Data::binary()} | {error, reason()}.

read_(_Handle) ->
    ?nif_stub.


-spec select_(Handle::handle(), Mode::read|write) ->
	  ok | {error, reason()}.

select_(_Handle, _Mode) ->
    ?nif_stub.

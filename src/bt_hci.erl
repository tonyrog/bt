%% bluetooth hci nif
-module(bt_hci).

-on_load(init/0).

-export([open/0]).
-export([bind/2]).
-export([close/1]).
-export([dev_up/1, dev_up/2]).
-export([dev_down/1, dev_down/2]).
-export([dev_reset/1, dev_reset/2]).
-export([dev_restat/1, dev_restat/2]).
-export([get_dev_list/1]).
-export([get_dev_info/1, get_dev_info/2]).
-export([get_conn_list/1 , get_conn_list/2]).
-export([get_conn_info/3]).
-export([get_auth_info/2]).
-export([set_raw/1, set_raw/2]).
-export([set_auth/2, set_auth/3]).
-export([set_encrypt/2, set_encrypt/3]).
-export([set_ptype/2, set_ptype/3]).
-export([set_link_policy/2, set_link_policy/3]).
-export([set_link_mode/2, set_link_mode/3]).
-export([set_scan/2, set_scan/3]).
-export([set_acl_mtu/3, set_acl_mtu/4]).
-export([set_sco_mtu/3, set_sco_mtu/4]).
-export([block/2]).
-export([unblock/2]).
-export([inquiry/5, inquiry/6]).
-export([set_filter/2]).
-export([get_filter/1]).
%% 
-export([debug_/2]).
-export([write_/2]).
-export([read_/1]).
-export([select_/2]).

-export([set_filter_ptype/2]).
-export([clr_filter_ptype/2]).
-export([set_filter_event/2]).
-export([clr_filter_event/2]).
-export([set_filter_opcode/2]).
-export([make_filter/3]).

-export([preloaded_atoms_/0]). % internal

-type addr() :: {byte(),byte(),byte(),byte(),byte(),byte()} |
		binary().
-type handle() :: reference().
-type reason() :: atom().
-type devid() :: integer().
-type uint8() :: integer().

-include("../include/hci_drv.hrl").
-include("hci_api.hrl").

-define(nif_stub(),nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).



%%
%% Exported: init
%%

init() ->
    ok = erlang:load_nif(
           filename:join(code:priv_dir(bt), bt_hci_nif), none).

preloaded_atoms_() ->
    [
     ok, error, undefined, select, read, write, 
     no_such_handle,
     enabled, disabled, p2p, 
     hci_filter, hci_dev_info, hci_dev_stats, hci_conn_info,
     inquiry_info
    ].

-spec open() -> {ok, handle()} | {error, reason()}.
open() ->
    ?nif_stub().

-spec bind(Hci::handle(),DevID::devid()) -> ok | {error, reason()}.
bind(_Hci, _DevID) ->
    ?nif_stub().

-spec close(Hci::handle()) -> ok | {error, reason()}.
close(_Hci) ->
    ?nif_stub().

-spec dev_up(Hci::handle()) -> ok | {error, reason()}.
dev_up(_Hci) -> ?nif_stub().
dev_up(_Hci, _DevID) -> ?nif_stub().

-spec dev_down(Hci::handle()) -> ok | {error, reason()}.
dev_down(_Hci) -> ?nif_stub().
dev_down(_Hci, _DevID) -> ?nif_stub().

-spec dev_reset(Hci::handle()) -> ok | {error, reason()}.
dev_reset(_Hci) -> ?nif_stub().
dev_reset(_Hci, _DevID) -> ?nif_stub().

-spec dev_restat(Hci::handle()) -> ok | {error, reason()}.
dev_restat(_Hci) -> ?nif_stub().
dev_restat(_Hci, _DevID) -> ?nif_stub().

-spec get_dev_list(Hci::handle()) -> ok | {error, reason()}.
get_dev_list(_Hci) -> ?nif_stub().

-spec get_dev_info(Hci::handle()) -> ok | {error, reason()}.
get_dev_info(_Hci) -> ?nif_stub().
get_dev_info(_Hci, _DevID) -> ?nif_stub().

-spec get_conn_list(Hci::handle()) -> ok | {error, reason()}.
get_conn_list(_Hci) -> ?nif_stub().
get_conn_list(_Hci, _DevID) -> ?nif_stub().

-spec get_conn_info(Hci::handle(),Addr::addr(),Type::uint8()) -> ok | {error, reason()}.
get_conn_info(_Hci,_Addr,_Type) -> ?nif_stub().

-spec get_auth_info(Hci::handle(), Addr::addr()) -> ok | {error, reason()}.
get_auth_info(_Hci, _Addr) -> ?nif_stub().

-spec set_raw(Hci::handle()) -> ok | {error, reason()}.
set_raw(_Hci) -> ?nif_stub().
set_raw(_Hci, _DevID) -> ?nif_stub().

-spec set_auth(Hci::handle(), Auth::enabled|disabled) ->
	  ok | {error, reason()}.
set_auth(_Hci, _Auth) -> ?nif_stub().
set_auth(_Hci, _DevID, _Auth) -> ?nif_stub().

-spec set_encrypt(Hci::handle(), Encrypt::p2p|disabled) -> 
	  ok | {error, reason()}.
set_encrypt(_Hci, _Encrypt) -> ?nif_stub().
set_encrypt(_Hci, _DevID, _Encrypt) -> ?nif_stub().

-spec set_ptype(Hci::handle(), Opt::integer()) -> ok | {error, reason()}.
set_ptype(_Hci, _Opt) -> ?nif_stub().
set_ptype(_Hci, _DevID, _Opt) -> ?nif_stub().

-spec set_link_policy(Hci::handle(), Opt::integer()) -> ok | {error, reason()}.
set_link_policy(_Hci, _Opt) -> ?nif_stub().
set_link_policy(_Hci, _DevID, _Opt) -> ?nif_stub().

-spec set_link_mode(Hci::handle(), Opt::integer()) -> ok | {error, reason()}.
set_link_mode(_Hci, _Opt) -> ?nif_stub().
set_link_mode(_Hci, _DevID, _Opt) -> ?nif_stub().

-spec set_scan(Hci::handle(), Opt::integer()) -> ok | {error, reason()}.
set_scan(_Hci, _Opt) -> ?nif_stub().
set_scan(_Hci, _DevID, _Opt) -> ?nif_stub().

-spec set_acl_mtu(Hci::handle(), Mtu::integer(), Mpkt::integer()) ->
	  ok | {error, reason()}.
set_acl_mtu(_Hci, _Mtu, _Mpkt) -> ?nif_stub().
set_acl_mtu(_Hci, _DevID, _Mtu, _Mpkt) -> ?nif_stub().

-spec set_sco_mtu(Hci::handle(), Mtu::integer(), Mpkt::integer()) ->
	  ok | {error, reason()}.
set_sco_mtu(_Hci, _Mtu, _Mpkt) -> ?nif_stub().
set_sco_mtu(_Hci, _DevID, _Mtu, _Mpkt) -> ?nif_stub().

-spec block(Hci::handle(), Addr::addr()) -> ok | {error, reason()}.
block(_Hci, _Addr) -> ?nif_stub().

-spec unblock(Hci::handle(), Addr::addr()) -> ok | {error, reason()}.
unblock(_Hci, _Addr) -> ?nif_stub().

-spec inquiry(Hci::handle(),Timeout::integer(), NumResp::integer(),
	      Lap::<<_:3>>, Flags::integer()) ->
	  ok | {error, reason()}.
inquiry(_Hci, _Timeout, _NumResp, _Lap, _Flags) -> ?nif_stub().
inquiry(_Hci, _DevID, _Timeout, _NumResp, _Lap, _Flags) -> ?nif_stub().

-spec set_filter(Hci::handle(), Filter::#hci_filter{}) -> 
	  ok | {error, reason()}.
set_filter(_Hci, _Filter) -> ?nif_stub().

-spec get_filter(Hci::handle()) -> ok | {error, reason()}.
get_filter(_Hci) ->  ?nif_stub().

-spec debug_(Hci::handle(), Level::integer()) -> ok.
debug_(_Hci, _Level) -> ?nif_stub().

-spec write_(Hci::handle(), Data::binary()) -> ok.
write_(_Hci, _Data) -> ?nif_stub().

-spec read_(Hci::handle()) -> binary().
read_(_Hci) -> ?nif_stub().
    
-spec select_(Handle::handle(), Mode::read|write) ->
	  ok | {error, reason()}.

select_(_Handle, _Mode) ->
    ?nif_stub().

%%
set_filter_ptype(T, F = #hci_filter { type_mask = M}) ->
    Bit = if T =:= ?HCI_VENDOR_PKT -> 1;
	      true ->  1 bsl (T band ?HCI_FLT_TYPE_BITS)
	   end,
    F#hci_filter { type_mask = M bor Bit }.

clr_filter_ptype(T, F = #hci_filter { type_mask = M}) ->
    Bit = if T =:= ?HCI_VENDOR_PKT -> 1;
	      true ->  1 bsl (T band ?HCI_FLT_TYPE_BITS)
	   end,
    F#hci_filter { type_mask = M band (bnot Bit) }.

set_filter_event(E, F = #hci_filter { event_mask = M}) ->
    Bit = 1 bsl (E band ?HCI_FLT_EVENT_BITS),
    F#hci_filter { event_mask = M bor Bit }.

clr_filter_event(E, F = #hci_filter { event_mask = M}) ->
    Bit = 1 bsl (E band ?HCI_FLT_EVENT_BITS),
    F#hci_filter { event_mask = M band (bnot Bit) }.

set_filter_opcode(Opcode,  F = #hci_filter { }) ->
    F#hci_filter { opcode = Opcode }.

make_filter(Opcode, Ts, Es) when is_integer(Opcode),
				 is_list(Ts),
				 is_list(Es) ->
    
    Type_mask = make_bits(Ts, 0) band 16#ffffffff,
    Event_mask = make_bits(Es, 0) band 16#ffffffffffffffff,
    #hci_filter { type_mask = Type_mask,
		  event_mask = Event_mask,
		  opcode = Opcode }.

make_bits([255|Ns], Bits) ->  %% ?HCI_VENDOR_PKT! -> 1
    make_bits(Ns, Bits bor 1);
make_bits([-1|_], _Bits) -> 16#ffffffffffffffff;
make_bits([Nr|Ns], Bits) when is_integer(Nr), Nr >= 0 ->
    make_bits(Ns, Bits bor (1 bsl Nr));
make_bits([], Bits) ->
    Bits.


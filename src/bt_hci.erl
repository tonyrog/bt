%% bluetooth hci nif
-module(bt_hci).

-on_load(init/0).

-export([open/0]).
-export([bind/3]).
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
-export([debug/2]).
-export([write/2]).
-export([read/1]).
-export([select/2]).

-export([set_filter_ptype/2]).
-export([clr_filter_ptype/2]).
-export([set_filter_event/2]).
-export([clr_filter_event/2]).
-export([set_filter_opcode/2]).
-export([make_filter/3]).
-export([make_opcode/1]).
-export([decode_filter/1]).
-export([decode_opcode/1]).
-export([decode_type/1]).
-export([decode_event/1]).
-export([decode_le_event/1]).
-export([decode_le_event_mask/1]).

-export([preloaded_atoms_/0]). % internal


-type addr() :: {byte(),byte(),byte(),byte(),byte(),byte()} |
		binary().
-type handle() :: reference().
-type reason() :: atom().
-type devid() :: integer().
-type uint8() :: integer().
-type channel() :: integer().

-export_type([addr/0, handle/0, reason/0,
	      devid/0, channel/0]).

-include("../include/hci.hrl").
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

-spec bind(Hci::handle(),DevID::devid(),Channel::channel()) -> 
	  ok | {error, reason()}.
bind(_Hci, _DevID, _Channel) ->
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

-spec get_dev_list(Hci::handle()) ->
	  {ok,[{devid(),Opts::integer()}]} | {error, reason()}.
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
	  {ok, [#inquiry_info{}]} | {error, reason()}.
inquiry(_Hci, _Timeout, _NumResp, _Lap, _Flags) -> ?nif_stub().

-spec inquiry(Hci::handle(),DevID::devid(),Timeout::integer(),
	      NumResp::integer(), Lap::<<_:3>>, Flags::integer()) ->
	  {ok, [#inquiry_info{}]} | {error, reason()}.
inquiry(_Hci, _DevID, _Timeout, _NumResp, _Lap, _Flags) -> ?nif_stub().

-spec set_filter(Hci::handle(), Filter::#hci_filter{}) -> 
	  ok | {error, reason()}.
set_filter(_Hci, _Filter) -> ?nif_stub().

-spec get_filter(Hci::handle()) -> {ok,#hci_filter{}} | {error, reason()}.
get_filter(_Hci) ->  ?nif_stub().

-spec debug(Hci::handle(), Level::integer()) -> ok.
debug(_Hci, _Level) -> ?nif_stub().

-spec write(Hci::handle(), Data::binary()) -> 
	  {ok, Size::integer()} | {error, reason()}.
write(_Hci, _Data) -> ?nif_stub().

-spec read(Hci::handle()) -> 
	  {ok, binary()} | {error, reason()}.
read(_Hci) -> ?nif_stub().
    
-spec select(Handle::handle(), Mode::read|write|[read|write|cancel]) ->
	  ok | {ok, cancelled} | {error, reason()}.

select(_Handle, _Mode) ->
    ?nif_stub().

%%
type_bit(?HCI_VENDOR_PKT) -> 1;
type_bit(Type) -> 1 bsl (Type band  ?HCI_FLT_TYPE_BITS).

event_bit(Evt) -> 1 bsl (Evt band ?HCI_FLT_EVENT_BITS).
     
set_filter_ptype(T, F = #hci_filter { type_mask = M}) ->
    F#hci_filter { type_mask = M bor type_bit(T) }.
clr_filter_ptype(T, F = #hci_filter { type_mask = M}) ->
    F#hci_filter { type_mask = M band (bnot type_bit(T)) }.

set_filter_event(E, F = #hci_filter { event_mask = M}) ->
    F#hci_filter { event_mask = M bor event_bit(E) }.
clr_filter_event(E, F = #hci_filter { event_mask = M}) ->
    F#hci_filter { event_mask = M band (bnot event_bit(E)) }.

set_filter_opcode(Opcode,  F = #hci_filter { }) ->
    F#hci_filter { opcode = Opcode }.

make_filter(Op, Ts, Es) ->
    Type_mask = make_type_mask(Ts, 0) band 16#ffffffff,
    Event_mask = make_event_mask(Es, 0) band 16#ffffffffffffffff,
    {OGF,OCF} = make_opcode_(Op),
    OpCode = ?cmd_opcode_pack(OGF, OCF),
    #hci_filter { type_mask = Type_mask,
		  event_mask = Event_mask,
		  opcode = OpCode }.

make_opcode(Op) ->
    {OGF,OCF} = make_opcode_(Op),
    ?cmd_opcode_pack(OGF, OCF).

make_opcode_({OGF,OCF}) -> {OGF band ?HCI_FLT_OGF_BITS, OCF band ?HCI_FLT_OCF_BITS};
make_opcode_(any) -> {16#00,16#000};
make_opcode_(Code) -> {?cmd_opcode_ogf(Code), ?cmd_opcode_ocf(Code)}.
    
make_type_mask([Type|Flags], Mask) when Type >= 0 ->
    make_type_mask(Flags, Mask bor type_bit(Type));
make_type_mask([], Mask) -> Mask;
make_type_mask(all, _Mask) -> 16#fffffffe. %% all except vendor type

make_event_mask([Evt|Events], Mask) when is_integer(Evt), Evt >= 0 ->
    make_event_mask(Events, Mask bor (1 bsl Evt));
make_event_mask([], Mask) -> Mask;
make_event_mask(all, _Mask) -> 16#fffffffffffffffe.

decode_filter(Filter) ->
    OpCode = decode_opcode(Filter#hci_filter.opcode),
    EventMask = decode_event_mask(Filter#hci_filter.event_mask),
    TypeMask = decode_type_mask(Filter#hci_filter.type_mask),
    #hci_filter{
       type_mask = TypeMask,
       event_mask = EventMask,
       opcode = OpCode
      }.

decode_type_mask(Mask) when Mask band 1 =:= 1 ->
    decode_bits_(Mask, 1, 31, 16#00000002, [vendor]);
decode_type_mask(Mask) ->
    decode_type_mask_(Mask, 1, 31, 16#00000002, []).

decode_type_mask_(Mask, I, N, Code, Acc) when I =< N ->
    if Code band Mask =/= 0 ->
	    decode_type_mask_(Mask-Code,I+1,N,Code bsl 1,[decode_type(I)|Acc]);
       true ->
	    decode_type_mask_(Mask,I+1,N,Code bsl 1,Acc)
    end;
decode_type_mask_(_Mask, _I, _N, _Code, Acc) ->
    Acc.
	
decode_type(Code) ->
    case Code of
	?HCI_COMMAND_PKT -> command;
	?HCI_ACLDATA_PKT -> acl;
	?HCI_SCODATA_PKT -> sco;
	?HCI_EVENT_PKT   -> event;
	?HCI_ISODATA_PKT -> isodata;
	_ -> Code
    end.

decode_event_mask(Mask) when Mask band 1 =:= 1 ->
    [vendor | decode_bits_(Mask, 1, 63, 16#0000000000000002, [])];
decode_event_mask(Mask) ->
    decode_event_mask_(Mask, 1, 63, 16#0000000000000002, []).


decode_event_mask_(0, _I, _N, _Code, Acc) -> Acc;
decode_event_mask_(Mask, I, N, Code, Acc) when I =< N ->
    if Code band Mask =/= 0 ->
	    decode_event_mask_(Mask-Code,I+1,N,Code bsl 1, [decode_event(I)|Acc]);
       true ->
	    decode_event_mask_(Mask, I+1, N, Code bsl 1, Acc)
    end;
decode_event_mask_(_Mask, _I, _N, _Code, Acc) ->
    Acc.

decode_event(Evt) ->
    case Evt of
	?EVT_INQUIRY_COMPLETE -> inquiry_complete;
	?EVT_INQUIRY_RESULT -> inquiry_result;
	?EVT_CONN_COMPLETE -> conn_complete;
	?EVT_CONN_REQUEST -> conn_request;
	?EVT_DISCONN_COMPLETE -> disconn_complete;
	?EVT_LE_META_EVENT -> le_meta_event;
	?EVT_AUTH_COMPLETE -> auth_complete;
	?EVT_REMOTE_NAME_REQ_COMPLETE -> remote_name_req_complete;
	?EVT_ENCRYPT_CHANGE -> encrypt_change;
	?EVT_CHANGE_CONN_LINK_KEY_COMPLETE -> change_conn_link_key_complete;
	?EVT_MASTER_LINK_KEY_COMPLETE -> master_link_key_complete;
	?EVT_READ_REMOTE_FEATURES_COMPLETE -> read_remote_features_complete;
	?EVT_READ_REMOTE_VERSION_COMPLETE -> read_remote_version_complete;
	?EVT_QOS_SETUP_COMPLETE -> qos_setup_complete;
	?EVT_CMD_COMPLETE -> cmd_complete;
	?EVT_CMD_STATUS -> cmd_status;
	?EVT_HARDWARE_ERROR -> hardware_error;
	?EVT_FLUSH_OCCURRED -> flush_occurred;
	?EVT_ROLE_CHANGE -> role_change;
	?EVT_NUM_COMP_PKTS -> num_comp_pkts;
	?EVT_MODE_CHANGE -> mode_change;
	?EVT_RETURN_LINK_KEYS -> return_link_keys;
	?EVT_PIN_CODE_REQ -> pin_code_req;
	?EVT_LINK_KEY_REQ -> link_key_req;
	?EVT_LINK_KEY_NOTIFY -> link_key_notify;
	?EVT_LOOPBACK_COMMAND -> loopback_command;
	?EVT_DATA_BUFFER_OVERFLOW -> data_buffer_overflow;
	?EVT_MAX_SLOTS_CHANGE -> max_slots_change;
	?EVT_READ_CLOCK_OFFSET_COMPLETE -> read_clock_offset_complete;
	?EVT_CONN_PTYPE_CHANGED -> conn_ptype_changed;
	?EVT_QOS_VIOLATION -> qos_violation;
	?EVT_PSCAN_REP_MODE_CHANGE -> pscan_rep_mode_change;
	?EVT_FLOW_SPEC_COMPLETE -> flow_spec_complete;
	?EVT_INQUIRY_RESULT_WITH_RSSI -> inquiry_result_with_rssi;
	?EVT_READ_REMOTE_EXT_FEATURES_COMPLETE -> read_remote_ext_features_complete;
	?EVT_SYNC_CONN_COMPLETE -> sync_conn_complete;
	?EVT_SYNC_CONN_CHANGED -> sync_conn_changed;
	?EVT_SNIFF_SUBRATING -> sniff_subrating;
	?EVT_EXTENDED_INQUIRY_RESULT -> extended_inquiry_result;
	?EVT_ENCRYPTION_KEY_REFRESH_COMPLETE -> encryption_key_refresh_complete;
	?EVT_IO_CAPABILITY_REQUEST -> io_capability_request;
	?EVT_IO_CAPABILITY_RESPONSE -> io_capability_response;
	?EVT_USER_CONFIRM_REQUEST -> user_confirm_request;
	?EVT_USER_PASSKEY_REQUEST -> user_passkey_request;
	?EVT_REMOTE_OOB_DATA_REQUEST -> remote_oob_data_request;
	?EVT_SIMPLE_PAIRING_COMPLETE -> simple_pairing_complete;
	?EVT_LINK_SUPERVISION_TIMEOUT_CHANGED -> link_supervision_timeout_changed;
	?EVT_ENHANCED_FLUSH_COMPLETE -> enhanced_flush_complete;
	?EVT_USER_PASSKEY_NOTIFY -> user_passkey_notify;
	?EVT_KEYPRESS_NOTIFY -> keypress_notify;
	?EVT_REMOTE_HOST_FEATURES_NOTIFY -> remote_host_features_notify;
	?EVT_PHYSICAL_LINK_COMPLETE -> physical_link_complete;
	?EVT_CHANNEL_SELECTED -> channel_selected;
	?EVT_DISCONNECT_PHYSICAL_LINK_COMPLETE -> disconnect_physical_link_complete;
	?EVT_PHYSICAL_LINK_LOSS_EARLY_WARNING -> physical_link_loss_early_warning;
	?EVT_PHYSICAL_LINK_RECOVERY -> physical_link_recovery;
	?EVT_LOGICAL_LINK_COMPLETE -> logical_link_complete;
	?EVT_DISCONNECT_LOGICAL_LINK_COMPLETE -> disconnect_logical_link_complete;
	?EVT_FLOW_SPEC_MODIFY_COMPLETE -> flow_spec_modify_complete;
	?EVT_NUMBER_COMPLETED_BLOCKS -> number_completed_blocks;
	?EVT_AMP_STATUS_CHANGE -> amp_status_change;	
	_ -> Evt
    end.

decode_le_event_mask(Mask) when Mask band 1 =:= 1 ->
    [vendor | decode_bits_(Mask, 1, 63, 16#0000000000000002, [])];
decode_le_event_mask(Mask) ->
    decode_le_event_mask_(Mask, 1, 63, 16#0000000000000002, []).

decode_le_event_mask_(0, _I, _N, _Code, Acc) -> Acc;
decode_le_event_mask_(Mask, I, N, Code, Acc) when I =< N ->
    if Code band Mask =/= 0 ->
	    decode_le_event_mask_(Mask-Code,I+1,N,Code bsl 1, 
				  [decode_le_event(I)|Acc]);
       true ->
	    decode_le_event_mask_(Mask, I+1, N, Code bsl 1, Acc)
    end;
decode_le_event_mask_(_Mask, _I, _N, _Code, Acc) ->
    Acc.

decode_le_event(Evt) ->
    case Evt of
	?EVT_LE_CONN_COMPLETE -> le_conn_complete;
	?EVT_LE_ADVERTISING_REPORT -> le_advertising_report;
	?EVT_LE_CONN_UPDATE_COMPLETE -> le_conn_update_complete;
	?EVT_LE_READ_REMOTE_USED_FEATURES_COMPLETE -> 
	    le_read_remote_used_features_complete;
	?EVT_LE_LTK_REQUEST -> le_ltk_request;
	_ -> Evt
    end.




decode_bits_(Mask, I, N, Code, Acc) when I =< N ->
    if Code band Mask =/= 0 -> 
	    decode_bits_(Mask, I+1, N, Code bsl 1, [I|Acc]);
       true ->
	    decode_bits_(Mask, I+1, N, Code bsl 1, Acc)
    end;
decode_bits_(_Mask, _I, _N, _Code, Acc) ->
    Acc.


%% Op = <<6:OGF, 10:OCF>>
decode_opcode(Op) ->
    OGF = decode_ogf(Op),
    {OGF, ?cmd_opcode_ocf(Op)}.

decode_ogf(Op) ->
    case ?cmd_opcode_ogf(Op) of
	?OGF_LINK_CTL -> link_ctl;
	?OGF_LINK_POLICY -> link_policy;
	?OGF_HOST_CTL -> host_ctl;
	?OGF_INFO_PARAM -> info_param;
	?OGF_STATUS_PARAM -> status_param;
	?OGF_TESTING_CMD -> testing_cmd;
	?OGF_LE_CTL -> le_ctl;
	?OGF_VENDOR_CMD -> vendor_cmd;
	OGF0 -> OGF0
    end.

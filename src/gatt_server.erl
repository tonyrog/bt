%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%    Simple GATT Server for BLE Peripheral
%%%    Handles ATT (Attribute Protocol) over L2CAP channel 0x0004
%%%
%%%    This is a minimal implementation for testing.
%%%    For production, you'd want full GATT service discovery, etc.
%%% @end
%%% Created : 19 Nov 2025 by Tony Rogvall <tony@rogvall.se>

-module(gatt_server).

-export([start/3, stop/1]).
-export([notify/3]).

-include("../include/hci.hrl").

%% ATT Protocol opcodes
-define(ATT_OP_ERROR_RSP,           16#01).
-define(ATT_OP_MTU_REQ,             16#02).
-define(ATT_OP_MTU_RSP,             16#03).
-define(ATT_OP_FIND_INFO_REQ,       16#04).
-define(ATT_OP_FIND_INFO_RSP,       16#05).
-define(ATT_OP_FIND_BY_TYPE_REQ,    16#06).
-define(ATT_OP_FIND_BY_TYPE_RSP,    16#07).
-define(ATT_OP_READ_BY_TYPE_REQ,    16#08).
-define(ATT_OP_READ_BY_TYPE_RSP,    16#09).
-define(ATT_OP_READ_REQ,            16#0A).
-define(ATT_OP_READ_RSP,            16#0B).
-define(ATT_OP_READ_BLOB_REQ,       16#0C).
-define(ATT_OP_READ_BLOB_RSP,       16#0D).
-define(ATT_OP_READ_MULTI_REQ,      16#0E).
-define(ATT_OP_READ_MULTI_RSP,      16#0F).
-define(ATT_OP_READ_BY_GROUP_REQ,   16#10).
-define(ATT_OP_READ_BY_GROUP_RSP,   16#11).
-define(ATT_OP_WRITE_REQ,           16#12).
-define(ATT_OP_WRITE_RSP,           16#13).
-define(ATT_OP_WRITE_CMD,           16#52).
-define(ATT_OP_HANDLE_VALUE_NTF,    16#1B).
-define(ATT_OP_HANDLE_VALUE_IND,    16#1D).

%% ATT Error codes
-define(ATT_ECODE_INVALID_HANDLE,   16#01).
-define(ATT_ECODE_READ_NOT_PERM,    16#02).
-define(ATT_ECODE_WRITE_NOT_PERM,   16#03).
-define(ATT_ECODE_INVALID_PDU,      16#04).
-define(ATT_ECODE_INSUFF_AUTHEN,    16#05).
-define(ATT_ECODE_REQ_NOT_SUPP,     16#06).
-define(ATT_ECODE_INVALID_OFFSET,   16#07).
-define(ATT_ECODE_INSUFF_AUTHOR,    16#08).
-define(ATT_ECODE_PREP_QUEUE_FULL,  16#09).
-define(ATT_ECODE_ATTR_NOT_FOUND,   16#0A).
-define(ATT_ECODE_ATTR_NOT_LONG,    16#0B).
-define(ATT_ECODE_INSUFF_ENC_KEY,   16#0C).
-define(ATT_ECODE_INVAL_ATTR_LEN,   16#0D).
-define(ATT_ECODE_UNLIKELY,         16#0E).
-define(ATT_ECODE_INSUFF_ENC,       16#0F).
-define(ATT_ECODE_UNSUPP_GRP_TYPE,  16#10).
-define(ATT_ECODE_INSUFF_RESOURCES, 16#11).

-define(L2CAP_ATT_CID, 16#0004).  % Fixed channel for ATT

-record(gatt_state, {
    hci :: reference(),
    conn_handle :: integer(),
    services :: list(),
    mtu = 23 :: integer(),
    attributes = [] :: list()  % [{Handle, UUID, Value, Props}]
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start GATT server for a connection
%% Services: List of service maps from ble module
-spec start(Hci::bt_hci:handle(), ConnHandle::integer(), Services::list()) ->
    {ok, pid()} | {error, term()}.
start(Hci, ConnHandle, Services) ->
    State = #gatt_state{
        hci = Hci,
        conn_handle = ConnHandle,
        services = Services,
        attributes = build_attribute_table(Services)
    },
    Pid = spawn_link(fun() -> gatt_loop(State) end),
    {ok, Pid}.

%% @doc Stop GATT server
-spec stop(Pid::pid()) -> ok.
stop(Pid) ->
    Pid ! stop,
    ok.

%% @doc Send notification to client
-spec notify(Pid::pid(), UUID::binary(), Value::binary()) -> ok.
notify(Pid, UUID, Value) ->
    Pid ! {notify, UUID, Value},
    ok.

%%====================================================================
%% Internal - Attribute Table
%%====================================================================

%% @doc Build ATT attribute table from GATT services
%% Handle allocation:
%%   0x0000 - Reserved
%%   0x0001 - GAP Service Declaration
%%   0x0002 - Device Name Characteristic
%%   ...    - Your services start here
build_attribute_table(Services) ->
    %% Start handle at 0x0010 for user services
    {Attrs, _NextHandle} = lists:foldl(
        fun(Service, {AccAttrs, Handle}) ->
            add_service_attributes(Service, AccAttrs, Handle)
        end,
        {[], 16#0010},
        Services
    ),
    lists:reverse(Attrs).

%% @doc Add attributes for a service
add_service_attributes(Service, Attrs, Handle) ->
    UUID = maps:get(uuid, Service),
    Characteristics = maps:get(characteristics, Service, []),

    %% Service Declaration attribute
    ServiceAttr = #{
        handle => Handle,
        type => <<16#2800:16/little>>,  % Primary Service UUID
        value => UUID,
        props => [read]
    },

    %% Add characteristics
    {CharAttrs, NextHandle} = lists:foldl(
        fun(Char, {AccC, H}) ->
            add_characteristic_attributes(Char, AccC, H)
        end,
        {[ServiceAttr | Attrs], Handle + 1},
        Characteristics
    ),

    {CharAttrs, NextHandle}.

%% @doc Add attributes for a characteristic
add_characteristic_attributes(Char, Attrs, Handle) ->
    UUID = maps:get(uuid, Char),
    Props = maps:get(properties, Char, [read]),
    Value = maps:get(value, Char, <<>>),

    %% Characteristic Declaration (handle H)
    PropsByte = encode_properties(Props),
    CharDecl = #{
        handle => Handle,
        type => <<16#2803:16/little>>,  % Characteristic UUID
        value => <<PropsByte, (Handle+1):16/little, UUID/binary>>,
        props => [read]
    },

    %% Characteristic Value (handle H+1)
    CharValue = #{
        handle => Handle + 1,
        type => UUID,
        value => Value,
        props => Props
    },

    {[CharValue, CharDecl | Attrs], Handle + 2}.

%% @doc Encode characteristic properties byte
encode_properties(Props) ->
    lists:foldl(
        fun(read, Acc) -> Acc bor 16#02;
           (write, Acc) -> Acc bor 16#08;
           (notify, Acc) -> Acc bor 16#10;
           (indicate, Acc) -> Acc bor 16#20;
           (_, Acc) -> Acc
        end,
        0,
        Props
    ).

%%====================================================================
%% Internal - GATT Server Loop
%%====================================================================

gatt_loop(State) ->
    %% TODO: For production, we'd use L2CAP to listen on ATT channel
    %% For now, we'll just handle messages from ble module
    receive
        {att_request, AttPdu} ->
            handle_att_request(AttPdu, State);

        {notify, UUID, Value} ->
            %% Find handle for this characteristic
            case find_handle_by_uuid(UUID, State#gatt_state.attributes) of
                {ok, Handle} ->
                    send_notification(Handle, Value, State),
                    gatt_loop(State);
                error ->
                    io:format("GATT: Unknown UUID for notification: ~p~n", [UUID]),
                    gatt_loop(State)
            end;

        stop ->
            ok;

        Other ->
            io:format("GATT Server: Unknown message: ~p~n", [Other]),
            gatt_loop(State)
    end.

%% @doc Handle incoming ATT request
handle_att_request(<<?ATT_OP_MTU_REQ, ClientMTU:16/little>>, State) ->
    %% Exchange MTU
    ServerMTU = State#gatt_state.mtu,
    MTU = min(ClientMTU, ServerMTU),
    Response = <<?ATT_OP_MTU_RSP, MTU:16/little>>,
    send_att_response(Response, State),
    gatt_loop(State#gatt_state{mtu = MTU});

handle_att_request(<<?ATT_OP_READ_BY_GROUP_REQ, StartHandle:16/little,
                     EndHandle:16/little, GroupType/binary>>, State) ->
    %% Read by group type (used for service discovery)
    io:format("GATT: Read by group type ~p - ~p~n", [StartHandle, EndHandle]),
    Attrs = find_attributes_by_type(GroupType, StartHandle, EndHandle,
                                   State#gatt_state.attributes),
    Response = format_read_by_group_response(Attrs),
    send_att_response(Response, State),
    gatt_loop(State);

handle_att_request(<<?ATT_OP_READ_REQ, Handle:16/little>>, State) ->
    %% Read attribute value
    case find_attribute_by_handle(Handle, State#gatt_state.attributes) of
        {ok, Attr} ->
            Value = maps:get(value, Attr),
            Response = <<?ATT_OP_READ_RSP, Value/binary>>,
            send_att_response(Response, State);
        error ->
            Error = <<?ATT_OP_ERROR_RSP, ?ATT_OP_READ_REQ,
                     Handle:16/little, ?ATT_ECODE_INVALID_HANDLE>>,
            send_att_response(Error, State)
    end,
    gatt_loop(State);

handle_att_request(<<Opcode, _/binary>>, State) ->
    io:format("GATT: Unsupported ATT opcode: ~.16B~n", [Opcode]),
    %% Send error: request not supported
    Error = <<?ATT_OP_ERROR_RSP, Opcode, 0:16/little, ?ATT_ECODE_REQ_NOT_SUPP>>,
    send_att_response(Error, State),
    gatt_loop(State).

%%====================================================================
%% Internal - ATT Helpers
%%====================================================================

send_att_response(Pdu, State) ->
    %% TODO: Send via L2CAP ATT channel
    %% For now just log
    io:format("GATT: Would send ATT response: ~p~n", [Pdu]),
    ok.

send_notification(Handle, Value, State) ->
    Pdu = <<?ATT_OP_HANDLE_VALUE_NTF, Handle:16/little, Value/binary>>,
    io:format("GATT: Would send notification: handle=~w, value=~p~n",
              [Handle, Value]),
    %% TODO: Send via L2CAP
    ok.

find_handle_by_uuid(UUID, Attrs) ->
    case lists:keyfind(UUID, 2, [{maps:get(type, A), maps:get(handle, A)}
                                 || A <- Attrs]) of
        {UUID, Handle} -> {ok, Handle};
        false -> error
    end.

find_attribute_by_handle(Handle, Attrs) ->
    case lists:keyfind(Handle, 2, [{maps:get(handle, A), A} || A <- Attrs]) of
        {Handle, Attr} -> {ok, Attr};
        false -> error
    end.

find_attributes_by_type(Type, Start, End, Attrs) ->
    [A || A <- Attrs,
          maps:get(type, A) =:= Type,
          maps:get(handle, A) >= Start,
          maps:get(handle, A) =< End].

format_read_by_group_response([]) ->
    <<?ATT_OP_ERROR_RSP, ?ATT_OP_READ_BY_GROUP_REQ, 0:16/little,
      ?ATT_ECODE_ATTR_NOT_FOUND>>;
format_read_by_group_response(Attrs) ->
    %% Format: opcode, length, [handle, end_handle, value]...
    %% Simplified version
    <<?ATT_OP_READ_BY_GROUP_RSP>>.

%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%    HCI LE (Low Energy) Commands
%%%    Wrapper around BLE-specific HCI commands
%%% @end
%%% Created : 18 Nov 2025 by Tony Rogvall <tony@rogvall.se>

-module(hci_le).

-export([
	 set_event_mask/2,

	 %% LE Advertising
	 set_advertising_parameters/2,
	 set_advertising_data/2,
	 set_scan_response_data/2,
	 set_advertise_enable/2,

	 %% LE Scanning
	 set_scan_parameters/2,
	 set_scan_enable/3,

	 %% LE Connection
	 create_connection/3,
	 cancel_connection/1,

	 %% LE Info
	 read_buffer_size/1,
	 read_local_supported_features/1

]).

-include("../include/hci.hrl").
-include("hci_api.hrl").
-include("bt_log.hrl").

-define(DEFAULT_TIMEOUT, 2000).

%% LE Advertising types
-define(ADV_IND, 0).              % Connectable undirected advertising
-define(ADV_DIRECT_IND, 1).       % Connectable directed advertising
-define(ADV_SCAN_IND, 2).         % Scannable undirected advertising
-define(ADV_NONCONN_IND, 3).      % Non connectable undirected advertising
-define(ADV_DIRECT_IND_LOW, 4).   % Connectable directed low duty cycle

%% Default advertising interval (in 0.625ms units)
%% 100ms = 160 units, 1sec = 1600 units
-define(DEFAULT_ADV_INTERVAL_MIN, 800).   % 500ms
-define(DEFAULT_ADV_INTERVAL_MAX, 800).   % 500ms

%% simple util
bool_to_byte(true) -> 1;
bool_to_byte(_) -> 0.
     
%%====================================================================
%% API - LE Advertising
%%====================================================================

%% @doc Set LE advertising parameters
%% Options: #{
%%   interval => Ms,           % Advertising interval in milliseconds
%%   type => connectable | scannable | non_connectable,
%%   own_addr_type => public | random,
%%   channel_map => 0..7       % Bit mask for channels 37,38,39
%% }
-spec set_advertising_parameters(Hci::reference(), Options::map()) ->
    ok | {error, term()}.
set_advertising_parameters(Hci, Options) ->
    %% Convert interval from ms to 0.625ms units
    IntervalMs = maps:get(interval, Options, 500),
    IntervalUnits = round(IntervalMs * 1000 / 625),
    MinInterval = max(32, min(16383, IntervalUnits)),  % 20ms - 10.24s
    MaxInterval = MinInterval,

    %% Advertising type
    AdvType = case maps:get(type, Options, connectable) of
        connectable -> ?ADV_IND;
        scannable -> ?ADV_SCAN_IND;
        non_connectable -> ?ADV_NONCONN_IND;
        directed -> ?ADV_DIRECT_IND
    end,

    %% Own address type
    OwnAddrType = case maps:get(own_addr_type, Options, public) of
        public -> ?LE_PUBLIC_ADDRESS;
        random -> ?LE_RANDOM_ADDRESS
    end,

    %% Direct address (used only for directed advertising)
    DirectAddrType = ?LE_PUBLIC_ADDRESS,
    DirectAddr = <<0,0,0,0,0,0>>,

    %% Channel map: bit 0=ch37, bit 1=ch38, bit 2=ch39
    ChannelMap = maps:get(channel_map, Options, 7),  % All channels

    %% Filter policy
    Filter = 0,  % Process scan and connection requests from all

    Params = <<?le_set_advertising_parameters_cp_bin(
        MinInterval, MaxInterval, AdvType, OwnAddrType,
        DirectAddrType, DirectAddr, ChannelMap, Filter)>>,
    %% terminate with CMD_COMPLETE?
    case hci:call(Hci, ?OGF_LE_CTL, ?OCF_LE_SET_ADVERTISING_PARAMETERS,
                  Params, undefined) of
        {ok, <<0>>} -> ok;
        {ok, <<Status>>} -> {error, hci:decode_status(Status)};
        Error -> Error
    end.

%% @doc Set LE advertising data (max 31 bytes)
%% Data is advertising packet payload
-spec set_advertising_data(Hci::reference(), Data::binary()) ->
    ok | {error, term()}.
set_advertising_data(Hci, Data) when byte_size(Data) =< 31 ->
    Length = byte_size(Data),
    %% Pad to 31 bytes
    PaddedData = <<Data/binary, 0:(31-Length)/unit:8>>,

    Params = <<?le_set_advertising_data_cp_bin(Length, PaddedData)>>,
    %% terminate with CMD_COMPLETE?
    case hci:call(Hci, ?OGF_LE_CTL, ?OCF_LE_SET_ADVERTISING_DATA,
                  Params, undefined) of
        {ok, <<0>>} -> ok;
        {ok, <<Status>>} -> {error, hci:decode_status(Status)};
        Error -> Error
    end;
set_advertising_data(_Hci, _Data) ->
    {error, data_too_long}.

%% @doc Set scan response data (max 31 bytes)
-spec set_scan_response_data(Hci::reference(), Data::binary()) ->
    ok | {error, term()}.
set_scan_response_data(Hci, Data) when byte_size(Data) =< 31 ->
    Length = byte_size(Data),
    PaddedData = <<Data/binary, 0:(31-Length)/unit:8>>,

    Params = <<?le_set_scan_response_data_cp_bin(Length, PaddedData)>>,
    %% terminate with CMD_COMPLETE?
    case hci:call(Hci, ?OGF_LE_CTL, ?OCF_LE_SET_SCAN_RESPONSE_DATA,
                  Params, undefined) of
        {ok, <<0>>} -> ok;
        {ok, <<Status>>} -> {error, hci:decode_status(Status)};
        Error -> Error
    end;
set_scan_response_data(_Hci, _Data) ->
    {error, data_too_long}.

%% @doc Enable/disable LE advertising
-spec set_advertise_enable(Hci::reference(), Enable::boolean()) ->
    ok | {error, term()}.
set_advertise_enable(Hci, Enable) ->
    EnableByte = bool_to_byte(Enable),
    Params = <<?le_set_advertise_enable_cp_bin(EnableByte)>>,
    %% terminate with CMD_COMPLETE?
    case hci:call(Hci, ?OGF_LE_CTL, ?OCF_LE_SET_ADVERTISE_ENABLE,
                  Params, undefined) of
        {ok, <<0>>} -> ok;
        {ok, <<Status>>} -> {error, hci:decode_status(Status)};
        Error -> Error
    end.

%%====================================================================
%% API - LE Scanning
%%====================================================================

%% @doc Set LE scan parameters
%% Options: #{
%%   type => active | passive,
%%   interval => Ms,
%%   window => Ms,
%%   own_addr_type => public | random
%% }
-spec set_scan_parameters(Hci::reference(), Options::map()) ->
    ok | {error, term()}.
set_scan_parameters(Hci, Options) ->
    %% Scan type
    ScanType = case maps:get(type, Options, active) of
        active -> 1;
        passive -> 0
    end,

    %% Scan interval and window (in 0.625ms units)
    IntervalMs = maps:get(interval, Options, 100),
    WindowMs = maps:get(window, Options, 50),

    Interval = round(IntervalMs * 1000 / 625),
    Window = round(WindowMs * 1000 / 625),

    %% Own address type
    OwnAddrType = case maps:get(own_addr_type, Options, public) of
        public -> ?LE_PUBLIC_ADDRESS;
        random -> ?LE_RANDOM_ADDRESS
    end,

    %% Filter policy
    Filter = 0,  % Accept all

    Params = <<?le_set_scan_parameters_cp_bin(
        ScanType, Interval, Window, OwnAddrType, Filter)>>,
    %% terminate with CMD_COMPLETE?
    case hci:call(Hci, ?OGF_LE_CTL, ?OCF_LE_SET_SCAN_PARAMETERS,
                  Params, undefined) of
        {ok, <<0>>} -> ok;
        {ok, <<Status>>} -> {error, hci:decode_status(Status)};
        Error -> Error
    end.

%% @doc Enable/disable LE scanning
-spec set_scan_enable(Hci::reference(), Enable::boolean(),
                     FilterDuplicates::boolean()) ->
    ok | {error, term()}.
set_scan_enable(Hci, Enable, FilterDuplicates) ->
    EnableByte = bool_to_byte(Enable),
    FilterByte =  bool_to_byte(FilterDuplicates),
    Params = <<?le_set_scan_enable_cp_bin(EnableByte, FilterByte)>>,
    %% terminate with CMD_COMPLETE?
    case hci:call(Hci, ?OGF_LE_CTL, ?OCF_LE_SET_SCAN_ENABLE,
                  Params, undefined) of
        {ok, <<0>>} -> ok;
        {ok, <<S>>} -> {error, hci:decode_status(S)};
        Error -> Error
    end.

%%====================================================================
%% API - LE Connection
%%====================================================================

%% @doc Create LE connection to a device
%% Options: #{
%%   peer_addr => {A,B,C,D,E,F},
%%   peer_addr_type => public | random,
%%   own_addr_type => public | random,
%%   interval => {Min, Max},     % Connection interval in ms
%%   latency => Integer,          % Slave latency
%%   timeout => Ms                % Supervision timeout
%% }
-spec create_connection(Hci::reference(), Address::tuple(), Options::map()) ->
    ok | {error, term()}.
create_connection(Hci, PeerAddr, Options) ->
    %% Scan interval/window for connection (in 0.625ms units)
    ScanInterval = 96,  % 60ms
    ScanWindow = 96,    % 60ms

    InitiatorFilter = 0,  % Use peer address

    PeerAddrType = case maps:get(peer_addr_type, Options, public) of
        public -> ?LE_PUBLIC_ADDRESS;
        random -> ?LE_RANDOM_ADDRESS
    end,

    PeerAddrBin = addr_to_binary(PeerAddr),

    OwnAddrType = case maps:get(own_addr_type, Options, public) of
        public -> ?LE_PUBLIC_ADDRESS;
        random -> ?LE_RANDOM_ADDRESS
    end,

    %% Connection interval (in 1.25ms units)
    {MinIntervalMs, MaxIntervalMs} = maps:get(interval, Options, {30, 50}),
    MinInterval = round(MinIntervalMs * 1000 / 1250),
    MaxInterval = round(MaxIntervalMs * 1000 / 1250),

    Latency = maps:get(latency, Options, 0),

    %% Supervision timeout (in 10ms units)
    TimeoutMs = maps:get(timeout, Options, 5000),
    SupervisionTimeout = round(TimeoutMs / 10),

    MinCeLength = 0,
    MaxCeLength = 0,

    Params = <<?le_create_conn_cp_bin(
        ScanInterval, ScanWindow, InitiatorFilter,
        PeerAddrType, PeerAddrBin, OwnAddrType,
        MinInterval, MaxInterval, Latency, SupervisionTimeout,
        MinCeLength, MaxCeLength)>>,

    %% Send command - LE_CREATE_CONN returns CMD_STATUS immediately
    %% The actual connection complete event will arrive later and must be handled by the caller
    %% We use a longer timeout since we're not waiting for the connection, just the command acknowledgement
    case hci:call(Hci, ?OGF_LE_CTL, ?OCF_LE_CREATE_CONN,
                  Params, -1, undefined, ?DEFAULT_TIMEOUT) of
        ok -> %% <<?evt_cmd_status_bin(0, _Ncmd, _Opcode)>>} ->
	    ?info("LE_CREATE_CONN command accepted, waiting for connection event..."),
	    ok;
        {error, Status} -> %% <<?evt_cmd_status_bin(Status, _Ncmd, _Opcode)>>} ->
	    %% StatusName = hci:decode_status(Status),
	    ?error("LE_CREATE_CONN command rejected: ~p", [Status]),
            {error, Status};
        Error ->
            ?error("LE_CREATE_CONN error: ~p", [Error]),
            Error
    end.

%% @doc Cancel ongoing LE connection attempt
-spec cancel_connection(Hci::reference()) -> ok | {error, term()}.
cancel_connection(Hci) ->
    case hci:call(Hci, ?OGF_LE_CTL, ?OCF_LE_CREATE_CONN_CANCEL,
                  <<>>, -1, undefined, ?DEFAULT_TIMEOUT) of
        {ok,<<0>>} ->
            ?info("LE connection cancelled"),
            ok;
        {ok, <<S>>} -> 
	    Status = hci:decode_status(S),
            ?error("Failed to cancel LE connection: ~p", [Status]),
            {error, Status}
    end.

%%====================================================================
%% API - LE Information
%%====================================================================

%% @doc Read LE buffer size
-spec read_buffer_size(Hci::reference()) ->
    {ok, #{pkt_len => integer(), max_pkt => integer()}} | {error, term()}.
read_buffer_size(Hci) ->
    case hci:call(Hci, ?OGF_LE_CTL, ?OCF_LE_READ_BUFFER_SIZE,<<>>,undefined) of
        {ok, <<?le_read_buffer_size_rp_bin(0, PktLen, MaxPkt)>>} ->
            {ok, #{pkt_len => PktLen, max_pkt => MaxPkt}};
        {ok, <<?le_read_buffer_size_rp_bin(Status, _, _)>>} ->
            {error, hci:decode_status(Status)};
        Error ->
            Error
    end.

%% @doc Read LE local supported features
-spec read_local_supported_features(Hci::reference()) ->
    {ok, binary()} | {error, term()}.
read_local_supported_features(Hci) ->
    case hci:call(Hci, ?OGF_LE_CTL, ?OCF_LE_READ_LOCAL_SUPPORTED_FEATURES,
                  <<>>, undefined) of
        {ok, <<?le_read_local_supported_features_rp_bin(0, Features)>>} ->
            {ok, Features};
        {ok, <<?le_read_local_supported_features_rp_bin(Status, _)>>} ->
            {error, hci:decode_status(Status)};
        Error ->
            Error
    end.

%% @doc Set LE event mask
set_event_mask(Hci, Events) ->
    Mask = encode_event_mask(Events),
    ?debug("set_event_mask: ~p", [bt_hci:decode_le_event_mask(Mask)]),
    %% terminate with CMD_COMPLETE?
    case hci:call(Hci, ?OGF_LE_CTL, ?OCF_LE_SET_EVENT_MASK,
		  <<?le_set_event_mask_cp_bin(<<Mask:64/native>>)>>, undefined) of
	{ok, <<0>>} ->
	    ok;
	{ok, <<Status>>} ->
            {error, hci:decode_status(Status)}
    end.

encode_event_mask(E) when is_integer(E) ->
    (1 bsl E); %% single 
encode_event_mask(Es) when is_list(Es) ->
    encode_event_mask_(Es, 0).

encode_event_mask_([E|Es], Mask) ->
    encode_event_mask_(Es, (1 bsl E) bor Mask);
encode_event_mask_([], Mask) ->
    Mask.

%%====================================================================
%% Internal helpers
%%====================================================================

addr_to_binary({A,B,C,D,E,F}) ->
    list_to_binary(lists:reverse([A,B,C,D,E,F])).

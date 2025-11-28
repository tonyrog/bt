%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%    BLE (Bluetooth Low Energy) High-Level API
%%%    Arduino-style simplicity with Erlang power!
%%%
%%%    Quick Start:
%%%      1. As Peripheral (Server):
%%%         {ok, Peripheral} = ble:begin_peripheral("MyDevice"),
%%%         ble:add_service(Peripheral, "180D"),  % Heart Rate Service
%%%         ble:advertise(Peripheral).
%%%
%%%      2. As Central (Client):
%%%         {ok, Central} = ble:begin_central(),
%%%         Devices = ble:scan(Central, 5000),
%%%         {ok, Dev} = ble:connect(Central, DeviceAddr).
%%%
%%% @end
%%% Created : 18 Nov 2025 by Tony Rogvall <tony@rogvall.se>

-module(ble).

%% High-level API - Arduino style!
-export([
	 %% Setup
	 begin_peripheral/0, begin_peripheral/1,
	 begin_central/0, begin_central/1,
	 stop/1,

	 %% Peripheral (Server) mode
	 set_device_name/2,
	 add_service/2, add_service/3,
	 add_characteristic/4, add_characteristic/5,
	 advertise/1, advertise/2,
	 stop_advertising/1,
	 
	 %% Central (Client) mode
	 scan/1, scan/2,
	 connect/2, connect/3,
	 disconnect/2,
	 discover_services/2,

	 %% GATT operations (works for both Central and Peripheral)
	 read/3, %% works on central side
	 read/2,
	 write/3,
	 subscribe/3,
	 unsubscribe/2,
	 
	 %% Utility
	 uuid/1,
	 print_device/1,
	 maps_take/3,
	 maps_find/3,
	 
	 %% Advanced - HCI management
	 reset_hci/0
	]).

%% internal
-export([central_loop_/1, central_loop/1]).
-export([peripheral_loop_/1, peripheral_loop/1]).

%% test
-export([start/0]).
-export([test_connect1/0]).
-export([test_connect2/0]).

-include("../include/bt.hrl").
-include("../include/hci.hrl").
-include("ble_state.hrl").
-include("hci_api.hrl").
-include("bt_log.hrl").

-type ble_handle() :: pid().
-type uuid_string() :: string() | atom() | uuid(). %% uid input
-type device_addr() :: {byte(),byte(),byte(),byte(),byte(),byte()}.
-type char_properties() :: [read | write | notify | indicate].

-define(HCI_TIMEOUT, 2000).
-define(HCI_NOEVENT, -1).
%% ,?HCI_NOEVENT,?HCI_TIMEOUT

%% TEST
start() ->
    reset_hci(),
    timer:sleep(1000),
    {ok, Central} = begin_central(),
    Devices = scan(Central, 5000),
    io:format("Devices = ~p\n", [Devices]),
    {ok, Central}.

%% connect lego remote control
test_connect1() ->
    {ok, Central} = begin_central(),
    case connect(Central, {192,99,128,37,255,144}) of
	{ok, ConnRef} ->
	    {ok, _Services} = discover(Central, ConnRef),
	    ok;
	Error ->
	    Error
    end.

%% connect hold peak
test_connect2() ->
    ServiceUUID = uuid("0000ffb0-0000-1000-8000-00805f9b34fb"),
    CharUUID    = uuid("0000ffb2-0000-1000-8000-00805f9b34fb"),
    {ok, Central} = begin_central(),
    case connect(Central, {1,182,236,211,24,0}) of
	{ok, ConnRef} ->
	    {ok, Services} = discover(Central, ConnRef),
	    loop2(Central, ConnRef, Services, ServiceUUID, CharUUID);
	Error ->
	    Error
    end.

loop2(Central, ConnRef, Services, ServiceUUID, CharUUID) ->
    case maps_find(ServiceUUID, uuid, Services) of
	false ->
	    {error, enoent};
	#{ handle := ServHandle } ->
	    loop2_(Central, ConnRef, ServHandle, CharUUID, 30)
    end.

loop2_(_Central, _ConnRef, _ServHandle, _CharUUID, 0)  ->
    ok;
loop2_(Central, ConnRef, ServHandle, CharUUID, I) when I > 0 ->
    case read(Central, ConnRef, CharUUID) of
	{ok, Value} ->
	    case tmeter:decode(Value) of
		{ok, {Val,Flags}} ->
		    io:format("Value = ~p ~w\n", [Val,Flags]);
		Error ->
		    io:format("~p\n", [Error])
	    end,
	    timer:sleep(1000),
	    loop2_(Central, ConnRef, ServHandle, CharUUID, I-1);
	{error, _Reason} = Error ->
	    io:format("Value = ~p\n", [Error]),
	    timer:sleep(1000),
	    loop2_(Central, ConnRef, ServHandle, CharUUID, I-1)
    end.

discover(Central, ConnRef) ->	
    {ok, Services} = discover_services(Central, ConnRef),
    io:format("Services = ~p\n", [Services]),
    lists:foreach(
      fun(Service) ->
	      case discover_characteristics(Central, ConnRef, Service) of 
		  {ok,Char} ->
		      io:format("Char = ~p\n", [Char]);
		  Error ->
		      io:format("Char = ~p\n", [Error])
	      end
      end, Services),
    {ok, Services}.

%%====================================================================
%% API - Setup
%%====================================================================

%% @doc Initialize BLE in Peripheral (server) mode with default name
-spec begin_peripheral() -> {ok, ble_handle()} | {error, term()}.
begin_peripheral() ->
    begin_peripheral("Erlang-BLE").

%% @doc Initialize BLE in Peripheral (server) mode
%% Like Arduino BLE.begin() but as peripheral
-spec begin_peripheral(Name::string()) -> {ok, ble_handle()} | {error, term()}.
begin_peripheral(Name) when is_list(Name) ->
    SELF = self(),
    Peripheral =
	spawn_link(
	  fun() ->
		  {ok,Hci} = hci:open(),
		  {ok, Size} = hci_le:read_buffer_size(Hci),
		  {ok, Features} = hci_le:read_local_supported_features(Hci),
		  %% Get current filter first (for debugging)
		  {ok, Filter0} = bt_hci:get_filter(Hci),
		  ?debug("periheral orignal filter: ~p",
			 [bt_hci:decode_filter(Filter0)]),
		  State =
		      #ble_state{
			 mode = peripheral,
			 hci = Hci,
			 name = Name,
			 services = [],
			 size = Size,
			 features = Features
			},

		  %% Set LE event mask to receive connection events
		  ?debug("Setting LE event mask for peripheral..."),
		  ok = hci_le:set_event_mask(Hci,
					     [?EVT_LE_CONN_COMPLETE,
					      ?EVT_LE_LTK_REQUEST,
					      ?EVT_LE_ADVERTISING_REPORT,
					      ?EVT_LE_CONN_UPDATE_COMPLETE,
					      ?EVT_LE_READ_REMOTE_USED_FEATURES_COMPLETE]),

		  %% Setup HCI filter to receive connection events and ACL data
		  Filter = bt_hci:make_filter(
			     any,  % All opcodes
			     [?HCI_EVENT_PKT, ?HCI_ACLDATA_PKT],  % Event and ACL data packets
			     [?EVT_CONN_COMPLETE,      %% Connection complete
			      ?EVT_DISCONN_COMPLETE,   %% Disconnection complete
			      ?EVT_LE_META_EVENT]      %% LE meta events
			    ),
		  ?debug("New HCI filter: ~p",
			 [bt_hci:decode_filter(Filter)]),
		  case bt_hci:set_filter(Hci, Filter) of
		      ok ->
			  {ok, VerifiedFilter} = bt_hci:get_filter(Hci),
			  ?debug("HCI filter set for peripheral mode"),
			  ?debug("  Verified filter: ~p",
				 [bt_hci:decode_filter(VerifiedFilter)]),
			  SELF ! {self(), ok},
			  peripheral_loop(State);
		      {error, Reason} ->
			  ?error("Failed to set HCI filter: ~p", [Reason]),
			  SELF ! {self(), ok},
			  peripheral_loop(State)
		  end
	  end),
    receive
	{Peripheral, _Result} ->
	    {ok,Peripheral}
    end.

%% @doc Initialize BLE in Central (client) mode
-spec begin_central() -> {ok, ble_handle()} | {error, term()}.
begin_central() ->
    begin_central([]).

%% @doc Initialize BLE in Central (client) mode with options
-spec begin_central(Options::list()) -> {ok, ble_handle()} | {error, term()}.
begin_central(_Options) ->
    SELF = self(),
    Central = 
	spawn_link(
	  fun() ->
		  {ok,Hci} = hci:open(),
		  {ok, Size} = hci_le:read_buffer_size(Hci),
		  {ok, Features} = hci_le:read_local_supported_features(Hci),
		  {ok, Filter0} = bt_hci:get_filter(Hci),
		  ?debug("central original filter: ~p", 
			 [bt_hci:decode_filter(Filter0)]),
		  State = #ble_state{
			     mode = central,
			     hci = Hci,
			     services = [],
			     connections = #{},
			     conn_refs = #{},
			     size = Size,
			     features = Features
			    },
		  ?debug("Setting LE event mask for central..."),
		  ok = hci_le:set_event_mask(Hci,
					     [?EVT_LE_CONN_COMPLETE,
					      ?EVT_LE_LTK_REQUEST,
					      ?EVT_LE_ADVERTISING_REPORT,
					      ?EVT_LE_CONN_UPDATE_COMPLETE,
					      ?EVT_LE_READ_REMOTE_USED_FEATURES_COMPLETE]),

		  %% Setup HCI filter for central mode
		  Filter = bt_hci:make_filter(
			     any,  % All opcodes
			     [?HCI_EVENT_PKT, ?HCI_ACLDATA_PKT],
			     all  % All events
			    ),

		  case bt_hci:set_filter(Hci, Filter) of
		      ok ->
			  {ok, VerifiedFilter} = bt_hci:get_filter(Hci),
			  ?debug("HCI filter set for central mode"),
			  ?debug("  Verified filter: ~p", 
				 [bt_hci:decode_filter(VerifiedFilter)]),
			  SELF ! {self(), ok},
			  central_loop(State);
		      {error, _Reason} ->
			  SELF ! {self(), ok},
			  central_loop(State)			  
		  end
	  end),
    receive
	{Central, _Result} ->
	    {ok,Central}
    end.


%% @doc Stop BLE and cleanup
-spec stop(Handle::ble_handle()) -> ok.
stop(Handle) when is_pid(Handle) ->
    Handle ! {stop, self()},
    receive
        {stopped, Handle} -> ok
    after 5000 ->
        exit(Handle, kill),
        ok
    end.

%%====================================================================
%% API - Peripheral (Server) Mode
%%====================================================================

%% @doc Set the device name for advertising
%% Like BLE.setLocalName("MyDevice")
-spec set_device_name(Handle::ble_handle(), Name::string()) -> ok.
set_device_name(Handle, Name) when is_pid(Handle), is_list(Name) ->
    Handle ! {set_name, Name, self()},
    receive
        {ok, Handle} -> ok
    after 5000 ->
        {error, timeout}
    end.

%% @doc Add a GATT service (using 16-bit UUID shorthand)
%% Like BLE.addService("180D") for Heart Rate
-spec add_service(Handle::ble_handle(), UUID::uuid_string()) -> ok | {error, term()}.
add_service(Handle, UUID) ->
    add_service(Handle, UUID, primary).

%% @doc Add a GATT service with type (primary/secondary)
-spec add_service(Handle::ble_handle(), UUID::uuid_string(), Type::atom()) ->
    ok | {error, term()}.
add_service(Handle, UUID, Type) when is_pid(Handle) ->
    Handle ! {add_service, uuid(UUID), Type, self()},
    receive
        {ok, Handle} -> ok;
        {error, Reason} -> {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

%% @doc Add a characteristic to the last added service
%% Properties: [read, write, notify, indicate]
%% Like: BLE.addCharacteristic("2A37", BLERead | BLENotify)
-spec add_characteristic(Handle::ble_handle(), UUID::uuid(),
                        Properties::char_properties(), InitialValue::binary()) ->
    ok | {error, term()}.
add_characteristic(Handle, UUID, Properties, InitialValue) ->
    add_characteristic(Handle, UUID, Properties, InitialValue, []).

%% @doc Add characteristic with descriptors
-spec add_characteristic(Handle::ble_handle(), UUID::uuid_string(),
                        Properties::char_properties(),
                        InitialValue::binary(), Descriptors::list()) ->
    ok | {error, term()}.
add_characteristic(Handle, UUID, Properties, InitialValue, Descriptors)
  when is_pid(Handle) ->
    Handle ! {add_characteristic, uuid(UUID), Properties,
              InitialValue, Descriptors, self()},
    receive
        {ok, Handle} -> ok;
        {error, Reason} -> {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

%% @doc Start advertising
%% Like BLE.advertise()
-spec advertise(Handle::ble_handle()) -> ok | {error, term()}.
advertise(Handle) ->
    advertise(Handle, #{}).

%% @doc Start advertising with options
%% Options: #{interval => Ms, connectable => Bool}
-spec advertise(Handle::ble_handle(), Options::map()) -> ok | {error, term()}.
advertise(Handle, Options) when is_pid(Handle), is_map(Options) ->
    Handle ! {advertise, Options, self()},
    receive
        {ok, Handle} -> ok;
        {error, Reason} -> {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

%% @doc Stop advertising
-spec stop_advertising(Handle::ble_handle()) -> ok.
stop_advertising(Handle) when is_pid(Handle) ->
    Handle ! {stop_advertising, self()},
    receive
        {ok, Handle} -> ok
    after 5000 ->
        {error, timeout}
    end.

%%====================================================================
%% API - Central (Client) Mode
%%====================================================================

%% @doc Scan for BLE devices
%% Like BLE.scanForUuid("180D") but scans for all
-spec scan(Handle::ble_handle()) -> [map()].
scan(Handle) ->
    scan(Handle, 5000).

%% @doc Scan for BLE devices with timeout
-spec scan(Handle::ble_handle(), Timeout::integer()) -> [map()].
scan(Handle, Timeout) when is_pid(Handle), is_integer(Timeout) ->
    Handle ! {scan, Timeout, self()},
    receive
        {scan_result, Devices} -> Devices
    after Timeout + 1000 ->
        {error, timeout}
    end.

%% @doc Connect to a BLE device
%% Like BLE.connect() after scan
-spec connect(Handle::ble_handle(), Addr::device_addr()) ->
    {ok, reference()} | {error, term()}.
connect(Handle, Addr) ->
    connect(Handle, Addr, 5000).

%% @doc Connect to BLE device with timeout
-spec connect(Handle::ble_handle(), Addr::device_addr(), Timeout::integer()) ->
    {ok, reference()} | {error, term()}.
connect(Handle, Addr, Timeout) when is_pid(Handle) ->
    Handle ! {connect, Addr, Timeout, self()},
    receive
        {connected, ConnRef} -> {ok, ConnRef};
        {error, Reason} -> {error, Reason}
    after Timeout + 1000 ->
        {error, timeout}
    end.

%% @doc Disconnect from device
-spec disconnect(Handle::ble_handle(), ConnRef::reference()) -> ok.
disconnect(Handle, ConnRef) when is_pid(Handle), is_reference(ConnRef) ->
    Handle ! {disconnect, ConnRef, self()},
    receive
        {ok, Handle} -> ok
    after 5000 ->
        {error, timeout}
    end.

%% @doc Discover GATT services on connected device
-spec discover_services(Handle::ble_handle(), ConnRef::reference()) ->
    {ok, [map()]} | {error, term()}.
discover_services(Handle, ConnRef) when is_pid(Handle), is_reference(ConnRef) ->
    Handle ! {discover_services, ConnRef, self()},
    receive
        {services, Services} -> {ok, Services};
        {error, Reason} -> {error, Reason}
    after 10000 ->
        {error, timeout}
    end.

%% @doc Discover GATT characteristics on connected device
-spec discover_characteristics(Handle::ble_handle(), ConnRef::reference(),
			       Service::map()) ->
	  {ok, [map()]} | {error, term()}.
discover_characteristics(Handle, ConnRef, Service) when is_pid(Handle), is_reference(ConnRef), is_map(Service)  ->
    Handle ! {discover_characteristics, ConnRef, self(), Service},
    receive
        {characteristics, Char} -> {ok, Char};
        {error, Reason} -> {error, Reason}
    after 20000 ->
        {error, timeout}
    end.

%%====================================================================
%% API - GATT Operations
%%====================================================================

%% @doc Read characteristic value on central side 
-spec read(Handle::ble_handle(), ConnRef::handle(), CharUUID::uuid_string()) ->
    {ok, binary()} | {error, term()}.
read(Handle, ConnRef, CharUUID) when is_pid(Handle) ->
    Handle ! {read_char, ConnRef, uuid(CharUUID), self()},
    receive
        {value, Value} -> {ok, Value};
        {error, Reason} -> {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

%% @doc Read characteristic value
-spec read(Handle::ble_handle(), CharUUID::uuid()) ->
    {ok, binary()} | {error, term()}.
read(Handle, CharUUID) when is_pid(Handle) ->
    Handle ! {read_char, uuid(CharUUID), self()},
    receive
        {value, Value} -> {ok, Value};
        {error, Reason} -> {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

%% @doc Write characteristic value
-spec write(Handle::ble_handle(), CharUUID::uuid(), Value::binary()) ->
    ok | {error, term()}.
write(Handle, CharUUID, Value) when is_pid(Handle), is_binary(Value) ->
    Handle ! {write_char, uuid(CharUUID), Value, self()},
    receive
        {ok, Handle} -> ok;
        {error, Reason} -> {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

%% @doc Subscribe to characteristic notifications
%% Callback: fun(UUID, Value) -> ok end
-spec subscribe(Handle::ble_handle(), CharUUID::uuid(), Callback::function()) ->
    ok | {error, term()}.
subscribe(Handle, CharUUID, Callback) when is_pid(Handle), is_function(Callback) ->
    Handle ! {subscribe, uuid(CharUUID), Callback, self()},
    receive
        {ok, Handle} -> ok;
        {error, Reason} -> {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

%% @doc Unsubscribe from notifications
-spec unsubscribe(Handle::ble_handle(), CharUUID::uuid()) -> ok.
unsubscribe(Handle, CharUUID) when is_pid(Handle) ->
    Handle ! {unsubscribe, uuid(CharUUID), self()},
    receive
        {ok, Handle} -> ok
    after 5000 ->
        {error, timeout}
    end.

%%====================================================================
%% API - Utility
%%====================================================================

%% @doc Convert various UUID formats to binary
-spec uuid(UUID::uuid_string()) -> uuid().
uuid(UUID) when is_binary(UUID), byte_size(UUID) == 16 ->
    UUID;
uuid(<<UUID:16>>) ->
    %% 16-bit UUID
    ?BT_UUID16(UUID);
uuid(UUID) when is_list(UUID), length(UUID) == 4 ->
    %% "180D" format
    Val = list_to_integer(UUID, 16),
    ?BT_UUID16(Val);
uuid(UUID) when is_atom(UUID) ->
    uuid(atom_to_list(UUID));
uuid(UUID) when is_list(UUID) ->     %% Full UUID string
    bt_util:string_to_uuid(UUID).

%% @doc Pretty print device info
-spec print_device(Device::map()) -> ok.
print_device(#{bdaddr := Addr} = Device) ->
    AddrStr = bt_util:format_address(Addr),
    Name = maps:get(name, Device, "Unknown"),
    RSSI = maps:get(rssi, Device, 0),
    io:format("Device: ~s (~s) RSSI: ~w dBm~n", [Name, AddrStr, RSSI]),
    ok.

%%====================================================================
%% API - Advanced HCI Management
%%====================================================================

%% @doc Reset HCI controller to clean state
%% Call this once at startup if needed, before creating any peripherals/centrals
%% Example:
%%   1> ble:reset_hci().
%%   2> {ok, BLE} = ble:begin_peripheral("MyDevice").
-spec reset_hci() -> ok | {error, term()}.
reset_hci() ->
    reset_hci(#{}).

%% @doc Reset HCI with options
%% Options:
%%   disable_pairing => true  % Disable simple pairing (for testing)
-spec reset_hci(Options::map()) -> ok | {error, term()}.
reset_hci(Options) ->
    case hci:open() of
        {ok, Hci} ->
            %% Reset HCI
            Result = case hci_api:reset(Hci,?HCI_NOEVENT,?HCI_TIMEOUT) of
			 {ok, <<0>>} ->
			     ?debug("HCI reset successful"),

			     %% Enable LE mode
			     ?debug("Enabling LE mode..."),
			     case hci_api:write_le_host_supported(Hci, 1, 0,?HCI_NOEVENT,?HCI_TIMEOUT) of  % LE enabled, simultaneous LE/BR disabled
				 {ok, <<0>>} ->
				     ?debug("LE mode enabled"),
				     ok;
				 {ok, <<Status>>} ->
				     ?warning("Could not enable LE mode: ~p", 
					      [hci:decode_status(Status)]);
				 Error ->
				     ?warning("Enabling LE mode: ~p", [Error])
			     end,

			     %% Set HCI event mask to include LE Meta Event
			     ?debug("Setting HCI event mask for LE..."),
			     EventMask = 16#20001FFFFFFFFFFF,  % Include LE Meta Event (bit 61)
			     case hci_api:set_event_mask(Hci, <<EventMask:64/little>>,?HCI_NOEVENT,?HCI_TIMEOUT) of
				 {ok, <<0>>} ->
				     ?debug("HCI event mask set");
				 {ok, <<Status2>>} ->
				     ?warning("Could not set event mask: ~p", [hci:decode_status(Status2)]);
				 Error2 ->
				     ?error("Warning: Error setting event mask: ~p", [Error2])
			     end,

			     %% Optionally disable pairing
			     case maps:get(disable_pairing, Options, false) of
				 true ->
				     ?debug("Disabling simple pairing..."),
				     case hci_api:write_simple_pairing_mode(Hci, 0,?HCI_NOEVENT,?HCI_TIMEOUT) of
					 {ok, <<0>>} ->
					     ?debug("Simple pairing disabled"),
					     ok;
					 _ ->
					     ?warning("Could not disable pairing"),
					     ok
				     end;
				 false ->
				     ok
			     end,
			     ok;
			 {ok, <<Status>>} ->
			     Error = hci:decode_status(Status),
			     ?error("HCI reset failed: ~p", [Error]),
			     {error, Error};
			 Error ->
			     ?error("HCI reset error: ~p", [Error]),
			     Error
		     end,
            hci:close(Hci),
            Result;
        Error ->
            ?error("Failed to open HCI: ~p", [Error]),
            Error
    end.

%%====================================================================
%% Internal - Peripheral Loop
%%====================================================================

%% @doc Handle incoming LE connection (BLE)
handle_le_connection_complete(Packet, State) ->
    try hci_api:decode_evt_le_conn_complete(Packet) of
        #evt_le_conn_complete{status = 0, handle = Handle, peer_bdaddr = Bdaddr} ->
            AddrStr = bt_util:format_address(Bdaddr),
            ?info(""),
            ?info("=============================================="),
            ?info(" BLE Device Connected!"),
            ?info(" Address: ~s", [AddrStr]),
            ?info(" Handle: ~w", [Handle]),
            ?info("=============================================="),
            ?info(""),

            %% Add connection to state
            Conn = #{handle => Handle, bdaddr => Bdaddr,
                    gatt_server => undefined},  % Will start GATT server here
            NewConns = [Conn | State#ble_state.connections],
            ?MODULE:peripheral_loop(State#ble_state{connections = NewConns});
        #evt_le_conn_complete{status = Status} ->
            ?error("BLE Connection failed: ~p", [hci:decode_status(Status)]),
            ?MODULE:peripheral_loop(State)
    catch
        _:Error ->
            ?error("Failed to decode LE connection event: ~p", [Error]),
            ?MODULE:peripheral_loop(State)
    end.

%% @doc Handle incoming classic BT connection (fallback)
handle_connection_complete(Packet, State) ->
    try hci_api:decode_evt_conn_complete(Packet) of
        #evt_conn_complete{status = 0, handle = Handle, bdaddr = Bdaddr} ->
            AddrStr = bt_util:format_address(Bdaddr),
            ?info(""),
            ?info("=============================================="),
            ?info(" Classic BT Device Connected!"),
            ?info(" Address: ~s", [AddrStr]),
            ?info(" Handle: ~w", [Handle]),
            ?info("=============================================="),
            ?info(""),
            %% Add connection to state
            Conn = #{handle => Handle, bdaddr => Bdaddr,
                    gatt_server => undefined},  % Will start GATT server here
            NewConns = [Conn | State#ble_state.connections],
            ?MODULE:peripheral_loop(State#ble_state{connections = NewConns});
        #evt_conn_complete{status = Status} ->
            ?error("Classic BT Connection failed: ~p",
		   [hci:decode_status(Status)]),
            ?MODULE:peripheral_loop(State)
    catch
        _:Error ->
            ?error("Failed to decode connection event: ~p", [Error]),
            ?MODULE:peripheral_loop(State)
    end.

%% @doc Handle incoming ACL data (L2CAP/ATT packets for GATT)
%% Parse L2CAP header: Length (2) + CID (2) + Payload

handle_acl_data(ConnHandle, Data, State) ->
    case Data of
        <<_L2capLen:16/little, 16#00004:16/little, Payload/binary>> ->
            %% ATT channel (GATT)
	    ?debug("BLE: Got ATT data on handle ~w: ~p", [ConnHandle, Payload]),
	    %% Check if this is a response to a pending GATT request
	    case State#ble_state.mode of
		central ->
		    %% GATT Client: handle ATT response
		    gatt_client:handle_att_response(Payload, ConnHandle, State);
		peripheral ->
		    %% GATT Server: handle ATT request
		    %% TODO: Forward to GATT server
		    State
	    end;
        <<_L2capLen:16/little, 16#00005:16/little, Payload/binary>> ->
            %% L2CAP signaling channel (LE)
	    ?debug("BLE: Got L2CAP LE signaling on handle ~w: ~p",
		 [ConnHandle, Payload]),
	    State;
        <<_L2capLen:16/little, 16#00006:16/little, Payload/binary>> ->
	    %% SMP channel (Security Manager Protocol)
	    ?info("=== SMP (Pairing) Request ===", []),
	    ?debug("BLE: Got SMP pairing data on handle ~w", [ConnHandle]),
	    ?debug("SMP Payload: ~p", [Payload]),
	    ?debug("============================\n",[]),
	    %% For now, we just log it - pairing not implemented yet
	    %% The LTK request will come later and we'll reject it
	    State;
        <<_L2capLen:16/little, Cid:16/little, Payload/binary>> ->
	    ?info("BLE: Got L2CAP data on CID 0x~4.16.0B, handle ~w, payload: ~p",
		 [Cid, ConnHandle, Payload]),
	    State
    end.


%% @doc Handle disconnection
handle_disconnection_complete(Packet, State) ->
    try hci_api:decode_evt_disconn_complete(Packet) of
        #evt_disconn_complete{status = 0, handle = Handle} ->
            ?info(" BLE Device Disconnected (handle: ~w)", [Handle]),
            %% Remove connection from state
            NewConns = lists:keydelete(Handle, 2,
                [{maps:get(handle, C), C} || C <- State#ble_state.connections]),
            ?MODULE:peripheral_loop(State#ble_state{connections = [C || {_, C} <- NewConns]});
        _ ->
            ?MODULE:peripheral_loop(State)
    catch
        _:Error ->
            ?error("Failed to decode disconnection event: ~p", [Error]),
            ?MODULE:peripheral_loop(State)
    end.

peripheral_loop(State) ->
    ok = bt_hci:select(State#ble_state.hci, read),    
    peripheral_loop_(State).
    
peripheral_loop_(State) ->
    receive
        %% HCI Event: Incoming connection or data!
        {select, Hci, _, ready_input} when Hci =:= State#ble_state.hci ->
            case bt_hci:read(Hci) of
                %% LE Meta Event (for BLE connections)
                {ok, <<?HCI_EVENT_PKT, ?EVT_LE_META_EVENT, Len, Packet:Len/binary, _/binary>>} ->
                    ?debug("Got LE_META_EVENT: ~p", [Packet]),
                    case Packet of
                        <<?evt_le_meta_event_bin(?EVT_LE_CONN_COMPLETE, _D1), LePacket/binary>> ->
                            ?debug("Got LE_CONN_COMPLETE"),
                            handle_le_connection_complete(LePacket, State);
                        <<?evt_le_meta_event_bin(?EVT_LE_LTK_REQUEST, _D1), LePacket/binary>> ->
                            ?debug("Got LE_LTK_REQUEST (pairing request)"),
                            %% For now, reject pairing - we'll implement proper pairing later
                            %% Parse LTK request to get handle
                            case LePacket of
                                <<Handle:16/little, _Random:64/little, _Diversifier:16/little, _/binary>> ->
                                    ?info("Rejecting LTK request for handle ~w (no pairing support yet)", [Handle]),
                                    hci_api:le_ltk_neg_reply(Hci,Handle,?HCI_NOEVENT,?HCI_TIMEOUT),
                                    ?MODULE:peripheral_loop(State);
                                _ ->
                                    ?warning("Could not parse LTK request"),
                                    ?MODULE:peripheral_loop(State)
                            end;
                        _ ->
                            ?warning("Got other LE subevent"),
                            ?MODULE:peripheral_loop(State)
                    end;

                %% Classic Bluetooth connection (fallback)
                {ok, <<?HCI_EVENT_PKT, ?EVT_CONN_COMPLETE, Len, Packet:Len/binary, _/binary>>} ->
                    ?debug("HCI_EVENT_PKT: CONN_COMPLETE: ~p",[Packet]),
                    handle_connection_complete(Packet, State);

                {ok, <<?HCI_EVENT_PKT, ?EVT_DISCONN_COMPLETE, Len, Packet:Len/binary, _/binary>>} ->
		    ?debug("HCI_EVENT_PKT: DISCONN_COMPLETE: ~p", [Packet]),
                    handle_disconnection_complete(Packet, State);
		
                {ok, <<?HCI_ACLDATA_PKT, Handle:16/native, Len:16/little, Data:Len/binary, _/binary>>} ->
		    ConnHandle = Handle band 16#fff, %%  _Flags:4
		    State1 = handle_acl_data(ConnHandle, Data, State),
                    ?MODULE:peripheral_loop(State1);
                {ok, Data} ->
                    ?debug("BLE Peripheral: Got HCI data: ~p", [Data]),
                    ?MODULE:peripheral_loop(State);
                {error, Reason} ->
                    ?error("BLE Peripheral: HCI read error: ~p", [Reason]),
                    ?MODULE:peripheral_loop(State)
            end;

        {set_name, Name, From} ->
            From ! {ok, self()},
            ?MODULE:peripheral_loop_(State#ble_state{name = Name});

        {add_service, UUID, Type, From} ->
            Service = #{uuid => UUID, type => Type, characteristics => []},
            NewServices = State#ble_state.services ++ [Service],
            From ! {ok, self()},
            ?MODULE:peripheral_loop_(State#ble_state{services = NewServices});

        {add_characteristic, UUID, Props, Value, Descs, From} ->
            Char = #{uuid => UUID, properties => Props,
		     value => Value, descriptors => Descs},
            case State#ble_state.services of
                [] ->
                    From ! {error, no_service},
                    ?MODULE:peripheral_loop_(State);
                Services ->
                    [LastService | Rest] = lists:reverse(Services),
                    Chars = maps:get(characteristics, LastService, []),
                    UpdatedService = LastService#{characteristics => Chars ++ [Char]},
                    NewServices = lists:reverse([UpdatedService | Rest]),
                    From ! {ok, self()},
                    ?MODULE:peripheral_loop_(State#ble_state{services = NewServices})
            end;

        {advertise, Options, From} ->
            %% Start real BLE advertising!
            ?info("Starting BLE advertising: ~s", [State#ble_state.name]),
            case start_advertising(State#ble_state.hci,
                                  State#ble_state.name,
                                  State#ble_state.services,
                                  Options) of
                ok ->
                    ?debug("BLE advertising started successfully!"),
                    From ! {ok, self()},
                    ?MODULE:peripheral_loop(State#ble_state{advertising = true});
                {error, Reason} ->
                    ?debug("Failed to start advertising ~p", [Reason]),
                    From ! {error, Reason},
                    ?MODULE:peripheral_loop(State)
            end;

        {stop_advertising, From} ->
            ?info("Stopping BLE advertising"),
            case State#ble_state.advertising of
                true ->
                    %%hci_le:set_advertise_enable(State#ble_state.hci, false),
		    hci_api:le_set_advertise_enable(State#ble_state.hci, 0,?HCI_NOEVENT,?HCI_TIMEOUT),
                    ?info("BLE advertising stopped");
                false ->
                    ok
            end,
            From ! {ok, self()},
            ?MODULE:peripheral_loop(State#ble_state{advertising = false});

        {read_char, UUID, From} ->
            case find_characteristic(UUID, State#ble_state.services) of
                {ok, Char} ->
                    From ! {value, maps:get(value, Char)};
                error ->
                    From ! {error, not_found}
            end,
            ?MODULE:peripheral_loop_(State);

        {write_char, UUID, Value, From} ->
            case update_characteristic(UUID, Value, State#ble_state.services) of
                {ok, NewServices} ->
                    From ! {ok, self()},
                    ?MODULE:peripheral_loop_(State#ble_state{services = NewServices});
                error ->
                    From ! {error, not_found},
                    ?MODULE:peripheral_loop_(State)
            end;
	
        {stop, From} ->
            %% Stop advertising if active
            case State#ble_state.advertising of
                true ->
		    hci_api:le_set_advertise_enable(State#ble_state.hci, 0,
						    ?HCI_NOEVENT,?HCI_TIMEOUT);
                false ->
		    ok
            end,
            hci:close(State#ble_state.hci),
            From ! {stopped, self()};

        Other ->
            ?warning("Peripheral: Unknown message: ~p", [Other]),
            ?MODULE:peripheral_loop_(State)
    end.

%%====================================================================
%% Internal - Central Loop
%%====================================================================

central_loop(State) ->
    ok = bt_hci:select(State#ble_state.hci, read),
    central_loop_(State).

central_loop_(State) ->
    receive
        %% HCI Event: For central mode (connection complete, etc)
        {select, Hci, _, ready_input} when Hci =:= State#ble_state.hci ->
            case bt_hci:read(Hci) of
                %% LE Meta Event (for BLE connections)
                {ok, <<?HCI_EVENT_PKT, ?EVT_LE_META_EVENT, Len, Packet:Len/binary, _/binary>>} ->
                    ?debug("BLE Central: Got LE_META_EVENT: ~p", [Packet]),
                    ok = bt_hci:select(Hci, read),
                    case Packet of
                        <<?evt_le_meta_event_bin(?EVT_LE_CONN_COMPLETE, _D1), LePacket/binary>> ->
                            ?debug("BLE Central: Got LE_CONN_COMPLETE", []),
                            case hci_api:decode_evt_le_conn_complete(LePacket) of
                                #evt_le_conn_complete{status = 0, handle = Handle, peer_bdaddr = Bdaddr} ->
                                    AddrStr = bt_util:format_address(Bdaddr),
                                    ?info("BLE Connection Complete!", []),
                                    ?info(" Address: ~s", [AddrStr]),
                                    ?info(" Handle: ~w", [Handle]),

                                    %% Notify any pending connection request
                                    case State#ble_state.pending_conn of
                                        {Addr, From, TRef} when Bdaddr =:= Addr ->
                                            %% Cancel the timeout timer
                                            erlang:cancel_timer(TRef),
                                            ConnRef = make_ref(),
                                            Conn = #{ref => ConnRef, handle => Handle, addr => Bdaddr},
					    Connections = State#ble_state.connections,
					    Connections1 = Connections#{ Handle => Conn },
					    ConnRefs = State#ble_state.conn_refs,
					    ConnRefs1 = ConnRefs#{ ConnRef => Handle },
                                            From ! {connected, ConnRef},
                                            ?MODULE:central_loop_(State#ble_state{connections = Connections1,
										  conn_refs = ConnRefs1,
										  pending_conn = undefined});
                                        _ ->
                                            %% Unsolicited connection, just add it
                                            ConnRef = make_ref(),
                                            Conn = #{ref => ConnRef, handle => Handle, addr => Bdaddr},
                                            NewConns = [Conn | State#ble_state.connections],
                                            ?MODULE:central_loop_(State#ble_state{connections = NewConns})
                                    end;
                                #evt_le_conn_complete{status = Status} ->
                                    ?error("BLE Connection failed: ~p", [hci:decode_status(Status)]),
                                    case State#ble_state.pending_conn of
                                        {_Addr, From, TRef} ->
                                            %% Cancel the timeout timer
                                            erlang:cancel_timer(TRef),
                                            From ! {error, hci:decode_status(Status)},
                                            ?MODULE:central_loop_(State#ble_state{pending_conn = undefined});
                                        _ ->
                                            ?MODULE:central_loop_(State)
                                    end
                            end;
                        _ ->
                            ?warning("BLE Central: Got other LE subevent", []),
                            ?MODULE:central_loop_(State)
                    end;

                {ok, <<?HCI_ACLDATA_PKT, Handle:16/native, Len:16/little, Data:Len/binary, _/binary>>} ->
		    ConnHandle = Handle band 16#0fff, %%  Extract handle (12 bits)
                    ?debug("BLE Central: Got ACL data on handle ~w", [ConnHandle]),
                    %% Check if we have a pending connection - if so, this ACL data
                    %% proves the connection succeeded even if we missed the LE_CONN_COMPLETE event!
                    State1 =
			case State#ble_state.pending_conn of
			    {_AddrBin, From, TRef} ->
				%% We got ACL data while waiting for connection complete!
				%% This means the connection actually succeeded.
				?info("BLE Connection established (detected via ACL data) handle: ~w", [ConnHandle]),
				%% Cancel the timeout timer
				erlang:cancel_timer(TRef),
				%% Create connection record (we don't know the exact address, but we have the handle)
				Connections = State#ble_state.connections,
				ConnRefs = State#ble_state.conn_refs,
				ConnRef = make_ref(),
				Conn = #{ref => ConnRef,
					 handle => ConnHandle, 
					 addr => undefined,
					 objects => #{},
					 uuids => #{}
					},
				Connections1 = Connections#{ ConnHandle => Conn },
				ConnRefs1 = ConnRefs#{ ConnRef => ConnHandle },

				From ! {connected, ConnRef},
				State#ble_state{connections = Connections1,
						conn_refs = ConnRefs1,
						pending_conn = undefined};
			    undefined ->
				State
			end,
                    State2 = handle_acl_data(ConnHandle, Data, State1),
		    ?MODULE:central_loop(State2);
                {ok, Data} ->
                    ?debug("BLE Central: Got HCI data: ~p", [Data]),
                    ?MODULE:central_loop(State);
                {error, Reason} ->
                    ?error("BLE Central: HCI read error: ~p", [Reason]),
                    %% Re-arm select even on error
                    ?MODULE:central_loop(State)
            end;

        {scan, Timeout, From} ->
            %% Perform LE scan (BLE devices)
            ?info("Scanning for BLE devices (~w ms)...", [Timeout]),
            case perform_le_scan(State#ble_state.hci, Timeout) of
                {ok, Devices} ->
                    ?info("Found ~w BLE devices", [length(Devices)]),
                    From ! {scan_result, Devices};
                {error, Reason} ->
                    ?info("Scan failed: ~p", [Reason]),
                    From ! {scan_result, []}
            end,
            ?MODULE:central_loop(State);
	
        {connect, Addr, Timeout, From} ->
            %% Create LE connection
            ?debug("Connecting to BLE device ~p (timeout: ~wms)...",
		   [Addr, Timeout]),

            %% Convert address to binary format for comparison
            AddrBin = list_to_binary(lists:reverse(tuple_to_list(Addr))),

            ConnOpts = #{
                peer_addr_type => public,
                interval => {30, 50},
                timeout => Timeout
            },
            case hci_le:create_connection(State#ble_state.hci, Addr, ConnOpts) of
                ok ->
		    ?debug("Connection command sent, waiting for response...", []),
                    %% Command sent successfully, now wait for connection complete event
                    %% OR for ACL data (which proves connection succeeded even if we missed the event)
                    %% Store pending connection info so we can match it when event arrives
                    %% Also schedule a timeout message
                    TRef = erlang:send_after(Timeout, self(), {connect_timeout, AddrBin}),
                    ?MODULE:central_loop(State#ble_state{pending_conn = {AddrBin, From, TRef}});
                Error ->
                    ?error("Connection command failed: ~p", [Error]),
                    From ! Error,
                    ?MODULE:central_loop(State)
            end;

        {connect_timeout, AddrBin} ->
            %% Connection timed out - cancel the pending connection
            case State#ble_state.pending_conn of
                {AddrBin, From, _TRef} ->
                    AddrStr = bt_util:format_address(list_to_tuple(lists:reverse(binary_to_list(AddrBin)))),
                    ?warning("Connection timeout for device ~s", [AddrStr]),

                    %% Cancel the ongoing connection attempt
                    case hci_le:cancel_connection(State#ble_state.hci) of
                        ok ->
                            ?debug("Connection attempt cancelled", []);
                        {error, Reason} ->
                            ?warning("Failed to cancel connection: ~p", [Reason])
                    end,
                    ?error("Connection rejected", []),
                    From ! {error, timeout},
                    ?MODULE:central_loop(State#ble_state{pending_conn = undefined});
                _ ->
                    %% Stale timeout, ignore
                    ?MODULE:central_loop(State)
            end;

        {disconnect, ConnRef, From} ->
	    case maps_take(ConnRef, ref, State#ble_state.connections) of
                false ->
                    From ! {error, not_found},
                    ?MODULE:central_loop(State);
                {value, #{ handle := Handle }, NewConns} ->
                    hci:disconnect(State#ble_state.hci, Handle, 16#13, 5000),
                    From ! {ok, self()},
                    ?MODULE:central_loop(State#ble_state{connections = NewConns})
            end;

        {discover_services, ConnRef, From} ->
	    case maps:get(ConnRef, State#ble_state.conn_refs, 0) of
		0 ->
                    From ! {error, not_found},
                    ?MODULE:central_loop(State);
		Handle ->
                    %% Send GATT service discovery request
                    case gatt_client:send_discover_services_request(State, Handle, From) of
                        {ok, NewState} ->
                            ?MODULE:central_loop(NewState);
                        {error, Reason} ->
                            From ! {error, Reason},
                            ?MODULE:central_loop(State)
                    end
            end;

        {discover_characteristics, ConnRef, From, Service} ->
	    case maps:get(ConnRef, State#ble_state.conn_refs, 0) of
		0 ->
                    From ! {error, not_found},
                    ?MODULE:central_loop(State);
                Handle ->
                    %% Send GATT service discovery request
		    ServHandle = maps:get(handle, Service, 0),
                    case gatt_client:send_discover_characteristics_request(State, Handle, From, ServHandle, Service) of
                        {ok, NewState} ->
                            ?MODULE:central_loop(NewState);
                        {error, Reason} ->
                            From ! {error, Reason},
                            ?MODULE:central_loop(State)
                    end
            end;

        {read_char, ConnRef, UUID, From} ->
	    ConnHandle = maps:get(ConnRef, State#ble_state.conn_refs, 0),
	    case maps:get(ConnHandle, State#ble_state.connections, undefined) of
		undefined ->
		    From ! {error, not_found};
		#{ objects := Objects, uuids := UUIDs } ->
		    Handle = maps:get(UUID, UUIDs, 0),
		    Char = maps:get(Handle, Objects, #{}),
		    case maps:get(value, Char, undefined) of 
			undefined ->
			    From ! {error, not_found};
			Value ->
			    From ! {value, Value}
		    end
            end,
            ?MODULE:central_loop_(State);

        {stop, From} ->
            %% Disconnect all
            lists:foreach(fun(#{handle := H}) ->
                hci:disconnect(State#ble_state.hci, H, 16#13, 1000)
            end, State#ble_state.connections),
            hci:close(State#ble_state.hci),
            From ! {stopped, self()};

        Other ->
            ?warning("Central: Unknown message: ~p", [Other]),
            ?MODULE:central_loop(State)
    end.

%%====================================================================
%% Internal Helpers
%%====================================================================

%% locate a map with in a list of maps and return a 
%% the map as value and remove the matching map from list
%% return the new list
maps_take(Value, Key, MapList) ->
    maps_take_(Value, Key, MapList, []).

maps_take_(Value, Key, [M|Ms], Acc) ->
    case maps:find(Key, M) of
	{ok,Value} ->
	    {value, M, lists:reverse(Acc, Ms)};
	_ ->
	    maps_take_(Value, Key, Ms, [M|Acc])
    end;
maps_take_(_Value, _Key, [], _) ->
    false.

%% locate a map with in a list of maps
maps_find(Value, Key, MapList) ->
    maps_find_(Value, Key, MapList).

maps_find_(Value, Key, [M|Ms]) ->
    case maps:find(Key, M) of
	{ok,Value} ->
	    M;
	_ ->
	    maps_find_(Value, Key, Ms)
    end;
maps_find_(_Value, _Key, []) ->
    false.


find_characteristic(_UUID, []) ->
    error;
find_characteristic(UUID, [Service | Rest]) ->
    Chars = maps:get(characteristics, Service, []),
    case lists:keyfind(UUID, 2, [{maps:get(uuid, C), C} || C <- Chars]) of
        false ->
            find_characteristic(UUID, Rest);
        {UUID, Char} ->
            {ok, Char}
    end.

update_characteristic(UUID, Value, Services) ->
    update_characteristic(UUID, Value, Services, []).

update_characteristic(_UUID, _Value, [], _Acc) ->
    error;
update_characteristic(UUID, Value, [Service | Rest], Acc) ->
    Chars = maps:get(characteristics, Service, []),
    case update_char_in_list(UUID, Value, Chars) of
        {ok, NewChars} ->
            NewService = Service#{characteristics => NewChars},
            {ok, lists:reverse(Acc) ++ [NewService | Rest]};
        error ->
            update_characteristic(UUID, Value, Rest, [Service | Acc])
    end.

update_char_in_list(UUID, Value, Chars) ->
    update_char_in_list(UUID, Value, Chars, []).

update_char_in_list(_UUID, _Value, [], _Acc) ->
    error;
update_char_in_list(UUID, Value, [Char | Rest], Acc) ->
    case maps:get(uuid, Char) of
        UUID ->
            NewChar = Char#{value => Value},
            {ok, lists:reverse(Acc) ++ [NewChar | Rest]};
        _ ->
            update_char_in_list(UUID, Value, Rest, [Char | Acc])
    end.

%%====================================================================
%% Internal - BLE Scanning
%%====================================================================

%% @doc Perform LE scan for BLE devices
perform_le_scan(Hci, Timeout) ->
    %% Set scan parameters
    ScanOpts = 
	#{
	  type => active,      % Active scanning (request scan responses)
	  interval => 100,     % 100ms
	  window => 50,        % 50ms
	  own_addr_type => public
	 },
    {ok, Filter0} = bt_hci:get_filter(Hci),
    ?debug("scan using filters: ~p",
	   [bt_hci:decode_filter(Filter0)]),

    case hci_le:set_scan_parameters(Hci, ScanOpts) of
        ok ->
            %% Enable scanning
            case hci_le:set_scan_enable(Hci, true, true) of
                ok ->
		    ok = hci_le:set_event_mask(Hci, 
					       [?EVT_LE_ADVERTISING_REPORT,
						?EVT_LE_CONN_COMPLETE,
						?EVT_LE_CONN_UPDATE_COMPLETE,
						?EVT_LE_READ_REMOTE_USED_FEATURES_COMPLETE]),
		    ScanFilter = bt_hci:make_filter(any,[?HCI_EVENT_PKT],
						    [?EVT_LE_META_EVENT]),
		    ?debug("Setting scan filter: ~p", [bt_hci:decode_filter(ScanFilter)]),
		    bt_hci:set_filter(Hci, ScanFilter),
		    
                    %% Collect scan results for Timeout ms
                    StartTime = erlang:monotonic_time(millisecond),
                    Devices = collect_scan_results(Hci, StartTime, Timeout, #{}),

                    %% Disable scanning
                    hci_le:set_scan_enable(Hci, false, false),

                    %% Restore original filter
                    bt_hci:set_filter(Hci, Filter0),

                    %% Convert device map to list
                    DeviceList = maps:fold(
                        fun(_Addr, DeviceInfo, Acc) ->
                            [DeviceInfo | Acc]
                        end, [], Devices),

                    {ok, DeviceList};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @doc Collect LE advertising reports from HCI events
%% Returns a map of {Address => DeviceInfo}
collect_scan_results(Hci, StartTime, Timeout, Devices) ->
    ok = bt_hci:select(Hci, read),
    collect_scan_results_(Hci, StartTime, Timeout, Devices).

collect_scan_results_(Hci, StartTime, Timeout, Devices) ->
    Now = erlang:monotonic_time(millisecond),
    Remaining = Timeout - (Now - StartTime),

    if Remaining =< 0 ->
            Devices;
       true ->
            %% Wait for HCI events with a short timeout
            receive
                {select, Hci, _, ready_input} ->
                    case bt_hci:read(Hci) of
                        {ok, <<?HCI_EVENT_PKT, ?EVT_LE_META_EVENT, Len, Packet:Len/binary, _/binary>>} ->
                            ?debug("Got LE_META_EVENT, len=~w, packet=~p", [Len, Packet]),
                            %% Parse LE meta event
                            case Packet of
                                <<?evt_le_meta_event_bin(?EVT_LE_ADVERTISING_REPORT, _D1), LePacket/binary>> ->
                                    ?debug("Got ADVERTISING_REPORT, data=~p", [LePacket]),
                                    %% Parse advertising report
                                    UpdatedDevices = parse_advertising_reports(LePacket, Devices),
                                    ?info("Now have ~w devices", [maps:size(UpdatedDevices)]),
                                    collect_scan_results(Hci, StartTime, Timeout, UpdatedDevices);
                                <<?evt_le_meta_event_bin(SubEvt, _D1), _LePacket/binary>> ->
                                    ?debug("Got LE subevent ~w (not advertising report)", [SubEvt]),
                                    %% Other LE event, ignore
                                    collect_scan_results(Hci, StartTime, Timeout, Devices)
                            end;
                        {ok, OtherData} ->
                            ?debug("Got other HCI data: ~p", [OtherData]),
                            %% Other HCI event, ignore
                            collect_scan_results(Hci, StartTime, Timeout, Devices);
                        {error, Reason} ->
                            ?error("Reading HCI: ~p", [Reason]),
                            %% Error reading, continue anyway
                            collect_scan_results(Hci, StartTime, Timeout, Devices)
                    end;
		Other ->
		    ?debug("Got OTHER ~p\n", [Other]),
		    collect_scan_results_(Hci, StartTime, Timeout, Devices)
		    
            after min(Remaining, 100) ->
                    %% Check again with updated time
                    collect_scan_results(Hci, StartTime, Timeout, Devices)
            end
    end.

%% @doc Parse LE advertising report packet
%% Multiple reports can be in one packet
parse_advertising_reports(<<NumReports:8, Rest/binary>>, Devices) ->
    parse_advertising_reports(NumReports, Rest, Devices).

parse_advertising_reports(0, _Data, Devices) ->
    Devices;
parse_advertising_reports(N, Data, Devices) when N > 0 ->
    %% Parse one advertising report
    %% Format: evt_type(1) + bdaddr_type(1) + bdaddr(6) + length(1) + data(length) + rssi(1)
    case Data of
        <<EvtType:8, BdaddrType:8, Bdaddr:6/binary, Length:8, Rest/binary>> when byte_size(Rest) >= Length + 1 ->
            <<AdvData:Length/binary, Rssi:8/signed, NextRest/binary>> = Rest,

            %% Convert address to tuple format
            <<A:8, B:8, C:8, D:8, E:8, F:8>> = Bdaddr,
            AddrTuple = {F, E, D, C, B, A},  % Reverse byte order for display

            Adv = ble_adv:decode(AdvData),
	    Name = ble_adv:get_name(Adv),
            %% Create device info
            DeviceInfo = #{
			   bdaddr => AddrTuple,
			   bdaddr_type => BdaddrType,
			   evt_type => EvtType,
			   rssi => Rssi,
			   name => Name,
			   adv_data => Adv
			  },

            %% Add or update device in map (keyed by address)
            UpdatedDevices = maps:put(AddrTuple, DeviceInfo, Devices),

            %% Continue with next report
            parse_advertising_reports(N - 1, NextRest, UpdatedDevices);
        _ ->
            %% Incomplete data, skip remaining reports
            ?warning("incomplete advertising report data"),
            Devices
    end.

%%====================================================================
%% Internal - BLE Advertising
%%====================================================================

%% @doc Start BLE advertising with real HCI commands
start_advertising(Hci, DeviceName, Services, Options) ->
    %% Step 1: Set advertising parameters
    AdvOptions = #{
        interval => maps:get(interval, Options, 500),  % 500ms default
        type => maps:get(type, Options, connectable),
        own_addr_type => public,
        channel_map => 7  % All channels
    },

    case hci_le:set_advertising_parameters(Hci, AdvOptions) of
        ok ->
            %% Step 2: Build and set advertising data
            AdvData = build_advertising_data(DeviceName, Services),
            case hci_le:set_advertising_data(Hci, AdvData) of
                ok ->
                    %% Step 3: Build and set scan response data (optional)
                    ScanData = build_scan_response_data(Services),
                    hci_le:set_scan_response_data(Hci, ScanData),

                    %% Step 4: Enable advertising
                    hci_le:set_advertise_enable(Hci, true);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @doc Build BLE advertising data packet (max 31 bytes)
%% Format: [Length, Type, Data]
build_advertising_data(DeviceName, Services) ->
    %% Flags (mandatory for LE)
    %% 0x06 = LE General Discoverable + BR/EDR Not Supported
    Flags = <<2, 16#01, 16#06>>,

    %% Complete Local Name
    NameBin = list_to_binary(DeviceName),
    NameLen = byte_size(NameBin),
    Name = if NameLen =< 28 -> %% Leave room for flags
                   <<(NameLen + 1), 16#09, NameBin/binary>>;
              true -> %% Truncate if too long
                   <<29, 16#08, NameBin:28/binary>>
           end,

    %% Complete List of 16-bit Service UUIDs (if any)
    ServiceUUIDs = build_service_uuid_list(Services),

    %% Combine all AD structures
    Data = <<Flags/binary, Name/binary, ServiceUUIDs/binary>>,

    %% Truncate to 31 bytes if needed
    case byte_size(Data) of
        Size when Size =< 31 -> Data;
        _ -> binary:part(Data, 0, 31)
    end.

%% @doc Build scan response data
build_scan_response_data(Services) ->
    %% Could include additional service UUIDs or manufacturer data
    %% For now, just leave empty
    case Services of
        [] -> <<>>;
        _ -> <<>>  % Could add more service info here
    end.

%% @doc Extract 16-bit service UUIDs from service list
build_service_uuid_list([]) ->
    <<>>;
build_service_uuid_list(Services) ->
    %% Extract 16-bit UUIDs
    UUIDs = lists:filtermap(fun(Service) ->
        case maps:get(uuid, Service) of
            ?BT_UUID16(UUID16) ->
                {true, <<UUID16:16/native>>};  %% native or little?
            _ ->
                false
        end
    end, Services),

    case UUIDs of
        [] -> <<>>;
        _ ->
            UUIDsBin = iolist_to_binary(UUIDs),
            Len = byte_size(UUIDsBin),
            <<(Len + 1), 16#03, UUIDsBin/binary>>  % 0x03 = Complete List of 16-bit UUIDs
    end.

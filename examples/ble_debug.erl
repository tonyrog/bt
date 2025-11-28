%%% @doc
%%% Debug BLE peripheral - minimal test
%%% @end

-module(ble_debug).
-export([test/0, raw_listen/0]).

-include("../include/hci_drv.hrl").

%% Test 1: Verify filter and basic setup
test() ->
    io:format("~n=== BLE Debug Test ===~n~n"),

    %% Step 1: Open HCI
    io:format("1. Opening HCI...~n"),
    case hci:open() of
        {ok, Hci} ->
            io:format("   ✓ HCI opened: ~p~n", [Hci]),

            %% Step 2: Get current filter
            io:format("~n2. Checking current filter...~n"),
            {ok, OldFilter} = bt_hci:get_filter(Hci),
            io:format("   Current filter: ~p~n", [OldFilter]),

            %% Step 3: Set new filter
            io:format("~n3. Setting new filter...~n"),
            Filter = bt_hci:make_filter(
                -1,
                [?HCI_EVENT_PKT, ?HCI_ACLDATA_PKT],
                [?EVT_CONN_COMPLETE, ?EVT_DISCONN_COMPLETE]
            ),
            io:format("   New filter: ~p~n", [Filter]),

            case bt_hci:set_filter(Hci, Filter) of
                ok ->
                    io:format("   ✓ Filter set successfully~n"),
                    {ok, NewFilter} = bt_hci:get_filter(Hci),
                    io:format("   Verified filter: ~p~n", [NewFilter]);
                {error, Reason} ->
                    io:format("   ✗ Failed to set filter: ~p~n", [Reason])
            end,

            %% Step 4: Setup advertising
            io:format("~n4. Setting up advertising...~n"),
            test_advertising(Hci),

            %% Step 5: Listen for events
            io:format("~n5. Listening for events (10 seconds)...~n"),
            io:format("   Connect from your phone NOW!~n~n"),

            ok = bt_hci:select(Hci, read),
            listen_loop(Hci, 10),

            %% Cleanup
            hci_le:set_advertise_enable(Hci, false),
            bt_hci:set_filter(Hci, OldFilter),
            hci:close(Hci),

            io:format("~n=== Test Complete ===~n");
        {error, Reason} ->
            io:format("   ✗ Failed to open HCI: ~p~n", [Reason])
    end.

test_advertising(Hci) ->
    %% Set advertising parameters
    AdvOpts = #{
        interval => 500,
        type => connectable,
        own_addr_type => public,
        channel_map => 7
    },
    case hci_le:set_advertising_parameters(Hci, AdvOpts) of
        ok -> io:format("   ✓ Advertising parameters set~n");
        Err -> io:format("   ✗ Failed to set adv params: ~p~n", [Err])
    end,

    %% Set advertising data
    DeviceName = "Debug-Test",
    AdvData = build_adv_data(DeviceName),
    io:format("   Adv data (~w bytes): ~p~n", [byte_size(AdvData), AdvData]),

    case hci_le:set_advertising_data(Hci, AdvData) of
        ok -> io:format("   ✓ Advertising data set~n");
        Err2 -> io:format("   ✗ Failed to set adv data: ~p~n", [Err2])
    end,

    %% Enable advertising
    case hci_le:set_advertise_enable(Hci, true) of
        ok -> io:format("   ✓ Advertising enabled~n");
        Err3 -> io:format("   ✗ Failed to enable advertising: ~p~n", [Err3])
    end.

build_adv_data(Name) ->
    Flags = <<2, 16#01, 16#06>>,
    NameBin = list_to_binary(Name),
    NameLen = byte_size(NameBin),
    NameData = <<(NameLen + 1), 16#09, NameBin/binary>>,
    <<Flags/binary, NameData/binary>>.

listen_loop(_Hci, 0) ->
    io:format("~n   Timeout - no events received~n"),
    ok;
listen_loop(Hci, N) ->
    receive
        {select, Hci, undefined, ready_input} ->
            case bt_hci:read(Hci) of
                {ok, Data} ->
                    io:format("~n   >>> RECEIVED DATA: ~p~n", [Data]),
                    parse_packet(Data),
                    ok = bt_hci:select(Hci, read),
                    listen_loop(Hci, N);
                {error, Reason} ->
                    io:format("   Read error: ~p~n", [Reason]),
                    ok = bt_hci:select(Hci, read),
                    listen_loop(Hci, N)
            end
    after 1000 ->
        io:format("."),
        listen_loop(Hci, N-1)
    end.

parse_packet(<<?HCI_EVENT_PKT, EventType, Len, Data:Len/binary, _/binary>>) ->
    io:format("   Event packet: type=0x~.16B (~s), len=~w~n",
              [EventType, event_name(EventType), Len]),
    io:format("   Data: ~p~n", [Data]);
parse_packet(<<?HCI_ACLDATA_PKT, Handle:12, Flags:4, Len:16, Data:Len/binary, _/binary>>) ->
    io:format("   ACL packet: handle=~w, flags=~w, len=~w~n", [Handle, Flags, Len]),
    io:format("   Data: ~p~n", [Data]);
parse_packet(Other) ->
    io:format("   Unknown packet format: ~p~n", [Other]).

event_name(16#03) -> "CONN_COMPLETE";
event_name(16#05) -> "DISCONN_COMPLETE";
event_name(16#0E) -> "CMD_COMPLETE";
event_name(16#0F) -> "CMD_STATUS";
event_name(16#3E) -> "LE_META_EVENT";
event_name(_) -> "UNKNOWN".

%% Test 2: Raw listen without filter
raw_listen() ->
    io:format("~n=== Raw Listen (no filter) ===~n~n"),
    {ok, Hci} = hci:open(),

    io:format("Listening for ANY HCI packets (5 seconds)...~n"),
    ok = bt_hci:select(Hci, read),

    raw_loop(Hci, 5),

    hci:close(Hci),
    io:format("~n=== Done ===~n").

raw_loop(_Hci, 0) -> ok;
raw_loop(Hci, N) ->
    receive
        {select, Hci, undefined, ready_input} ->
            case bt_hci:read(Hci) of
                {ok, Data} ->
                    io:format("Got: ~p~n", [Data]),
                    ok = bt_hci:select(Hci, read),
                    raw_loop(Hci, N);
                Err ->
                    io:format("Error: ~p~n", [Err]),
                    raw_loop(Hci, N)
            end
    after 1000 ->
        io:format("."),
        raw_loop(Hci, N-1)
    end.

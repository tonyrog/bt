%%% @doc
%%% Test HCI filter for BLE peripheral
%%% This will show all HCI events received
%%% @end

-module(ble_filter_test).
-export([test/0]).

-include("../include/hci_drv.hrl").

test() ->
    io:format("Opening HCI device...~n"),
    {ok, Hci} = hci:open(),

    %% Get current filter
    {ok, OldFilter} = bt_hci:get_filter(Hci),
    io:format("Old filter: ~p~n", [OldFilter]),

    %% Create new filter for all events
    NewFilter = bt_hci:make_filter(
        -1,  % All opcodes
        [?HCI_EVENT_PKT, ?HCI_ACLDATA_PKT],
        [?EVT_CONN_COMPLETE, ?EVT_DISCONN_COMPLETE, ?EVT_LE_META_EVENT]
    ),
    io:format("New filter: ~p~n", [NewFilter]),

    %% Set filter
    case bt_hci:set_filter(Hci, NewFilter) of
        ok ->
            io:format("Filter set successfully!~n");
        {error, Reason} ->
            io:format("Failed to set filter: ~p~n", [Reason])
    end,

    %% Read it back
    {ok, CurrentFilter} = bt_hci:get_filter(Hci),
    io:format("Current filter: ~p~n", [CurrentFilter]),

    %% Restore old filter
    bt_hci:set_filter(Hci, OldFilter),
    hci:close(Hci),

    ok.

%%% @doc
%%% Simple test to verify BLE peripheral connection handling
%%%
%%% Usage:
%%%   1> c(ble_connection_test).
%%%   2> ble_connection_test:start().
%%%   %% Now connect from your phone with nRF Connect or similar
%%%   %% You should see connection messages!
%%% @end

-module(ble_connection_test).
-export([start/0]).

start() ->
    io:format("~n"),
    io:format("==============================================~n"),
    io:format(" BLE Connection Test~n"),
    io:format("==============================================~n"),
    io:format("~n"),

    %% Start as peripheral
    {ok, BLE} = ble:begin_peripheral("Erlang-Test"),

    %% Add a simple test service
    ok = ble:add_service(BLE, "180A"),  % Device Information Service
    ok = ble:add_characteristic(BLE, "2A29",  % Manufacturer Name
                                [read],
                                <<"Erlang BLE">>),

    %% Start advertising
    ok = ble:advertise(BLE),

    io:format("~n"),
    io:format("Advertising started!~n"),
    io:format("~n"),
    io:format("Connect from your phone using:~n"),
    io:format("  - nRF Connect (Android/iOS)~n"),
    io:format("  - LightBlue (iOS)~n"),
    io:format("  - Bluetooth LE Scanner (Android)~n"),
    io:format("~n"),
    io:format("You should see 'Erlang-Test' in the device list.~n"),
    io:format("When you connect, you'll see a connection message!~n"),
    io:format("~n"),

    %% Keep running
    io:format("Press Ctrl+C twice to exit.~n"),
    io:format("~n"),

    {ok, BLE}.

%%% @doc
%%% Simple BLE Test - Verify advertising works
%%%
%%% This creates a minimal BLE peripheral that you can detect with your phone!
%%%
%%% Usage:
%%%   1> c(ble_simple_test).
%%%   2> ble_simple_test:start().
%%%
%%% Then open a BLE scanner app on your phone (like "nRF Connect")
%%% and look for "Erlang-Test" device!
%%%
%%% @end
-module(ble_simple_test).
-export([start/0, stop/0]).

start() ->
    io:format("~n"),
    io:format("===============================================~n"),
    io:format(" BLE Simple Test~n"),
    io:format("===============================================~n"),
    io:format("~n"),

    %% Initialize BLE as Peripheral
    {ok, BLE} = ble:begin_peripheral("Erlang-Test"),
    io:format("✓ BLE initialized~n"),

    %% Add a simple service (Device Information Service)
    ok = ble:add_service(BLE, "180A"),
    io:format("✓ Added Device Information Service (0x180A)~n"),

    %% Add a characteristic (Manufacturer Name)
    ok = ble:add_characteristic(BLE, "2A29",
                                [read],
                                <<"Erlang Systems">>),
    io:format("✓ Added Manufacturer Name characteristic~n"),

    %% Start advertising with 1000ms interval
    ok = ble:advertise(BLE, #{interval => 1000}),

    io:format("~n"),
    io:format("===============================================~n"),
    io:format(" 📡 Now advertising as 'Erlang-Test'!~n"),
    io:format("===============================================~n"),
    io:format("~n"),
    io:format("Open a BLE scanner app on your phone:~n"),
    io:format("  - nRF Connect (Android/iOS)~n"),
    io:format("  - LightBlue (iOS)~n"),
    io:format("  - BLE Scanner (Android)~n"),
    io:format("~n"),
    io:format("Look for device named: Erlang-Test~n"),
    io:format("Service UUID: 0x180A (Device Information)~n"),
    io:format("~n"),
    io:format("To stop: ble_simple_test:stop().~n"),
    io:format("~n"),

    %% Save handle
    put(ble_handle, BLE),

    {ok, BLE}.

stop() ->
    case get(ble_handle) of
        undefined ->
            {error, not_started};
        BLE ->
            io:format("Stopping BLE advertising...~n"),
            ble:stop_advertising(BLE),
            ble:stop(BLE),
            erase(ble_handle),
            io:format("✓ Stopped~n"),
            ok
    end.

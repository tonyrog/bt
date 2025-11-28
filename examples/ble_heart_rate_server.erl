%%% @doc
%%% BLE Heart Rate Monitor - Peripheral Example
%%%
%%% This is like an Arduino BLE heart rate monitor!
%%% Super simple to use.
%%%
%%% Usage:
%%%   1> c(ble_heart_rate_server).
%%%   2> ble_heart_rate_server:start().
%%%
%%% @end
-module(ble_heart_rate_server).
-export([start/0, start/1, stop/0]).
-export([update_heart_rate/1]).

-define(HEART_RATE_SERVICE, "180D").       % Heart Rate Service UUID
-define(HEART_RATE_MEASUREMENT, "2A37").   % Heart Rate Measurement Char
-define(BODY_SENSOR_LOCATION, "2A38").     % Body Sensor Location Char

start() ->
    start("Erlang-HeartRate").

start(DeviceName) ->
    %% Initialize BLE as Peripheral (like Arduino BLE.begin())
    {ok, BLE} = ble:begin_peripheral(DeviceName),

    %% Set device name for advertising
    ble:set_device_name(BLE, DeviceName),

    %% Add Heart Rate Service (like BLE.addService())
    ok = ble:add_service(BLE, ?HEART_RATE_SERVICE),

    %% Add Heart Rate Measurement characteristic
    %% Properties: notify (can send updates to connected devices)
    ok = ble:add_characteristic(BLE, ?HEART_RATE_MEASUREMENT,
                                [read, notify],
                                <<0, 75>>),  % Initial value: 75 bpm

    %% Add Body Sensor Location characteristic
    %% 1 = Chest
    ok = ble:add_characteristic(BLE, ?BODY_SENSOR_LOCATION,
                                [read],
                                <<1>>),

    %% Start advertising (like BLE.advertise())
    ok = ble:advertise(BLE),

    io:format("~n"),
    io:format("==============================================~n"),
    io:format(" Heart Rate Monitor Started!~n"),
    io:format(" Device: ~s~n", [DeviceName]),
    io:format(" Service: Heart Rate (0x~s)~n", [?HEART_RATE_SERVICE]),
    io:format("==============================================~n"),
    io:format("~n"),
    io:format("Advertising... (connect with your phone)~n"),
    io:format("~n"),
    io:format("Try:~n"),
    io:format("  ble_heart_rate_server:update_heart_rate(85).~n"),
    io:format("~n"),

    %% Save handle for later
    put(ble_handle, BLE),

    %% Start simulating heart rate
    spawn_link(fun() -> simulate_heart_rate(BLE) end),

    {ok, BLE}.

stop() ->
    case get(ble_handle) of
        undefined ->
            {error, not_started};
        BLE ->
            ble:stop_advertising(BLE),
            ble:stop(BLE),
            erase(ble_handle),
            ok
    end.

%% Update heart rate value
update_heart_rate(BPM) when is_integer(BPM), BPM > 0, BPM < 256 ->
    case get(ble_handle) of
        undefined ->
            {error, not_started};
        BLE ->
            %% Format: Flags (0 = uint8 format) + BPM value
            Value = <<0, BPM>>,
            ble:write(BLE, ?HEART_RATE_MEASUREMENT, Value),
            io:format("Heart Rate updated: ~w bpm~n", [BPM]),
            ok
    end.

%% Simulate realistic heart rate variations
simulate_heart_rate(BLE) ->
    BaseRate = 75,  % Base heart rate
    simulate_loop(BLE, BaseRate, 0).

simulate_loop(BLE, BaseRate, Counter) ->
    %% Add some variation (sine wave + random)
    Variation = round(10 * math:sin(Counter / 10.0)) + rand:uniform(5) - 3,
    HeartRate = max(60, min(120, BaseRate + Variation)),

    %% Update every 1 second
    timer:sleep(1000),

    %% Write new value
    Value = <<0, HeartRate>>,
    ble:write(BLE, ?HEART_RATE_MEASUREMENT, Value),

    %% Print every 5 seconds
    case Counter rem 5 of
        0 -> io:format("♥ Heart Rate: ~w bpm~n", [HeartRate]);
        _ -> ok
    end,

    simulate_loop(BLE, BaseRate, Counter + 1).

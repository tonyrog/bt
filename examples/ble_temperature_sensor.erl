%%% @doc
%%% BLE Temperature Sensor - Another Peripheral Example
%%%
%%% Environmental Sensing Service with Temperature
%%% Shows how easy it is to create different BLE services!
%%%
%%% Usage:
%%%   1> c(ble_temperature_sensor).
%%%   2> ble_temperature_sensor:start().
%%%   3> ble_temperature_sensor:set_temperature(22.5).
%%%
%%% @end
-module(ble_temperature_sensor).
-export([start/0, start/1, stop/0]).
-export([set_temperature/1]).

-define(ENV_SENSING_SERVICE, "181A").       % Environmental Sensing
-define(TEMPERATURE_CHAR, "2A6E").          % Temperature Characteristic
-define(DEVICE_NAME, "Erlang-TempSensor").

start() ->
    start(?DEVICE_NAME).

start(DeviceName) ->
    %% Initialize BLE Peripheral
    {ok, BLE} = ble:begin_peripheral(DeviceName),

    %% Set device name
    ble:set_device_name(BLE, DeviceName),

    %% Add Environmental Sensing Service
    ok = ble:add_service(BLE, ?ENV_SENSING_SERVICE),

    %% Add Temperature characteristic
    %% Temperature in Celsius * 100 (e.g., 2150 = 21.50°C)
    InitialTemp = encode_temperature(21.0),
    ok = ble:add_characteristic(BLE, ?TEMPERATURE_CHAR,
                                [read, notify],
                                InitialTemp),

    %% Start advertising
    ok = ble:advertise(BLE),

    io:format("~n"),
    io:format("==============================================~n"),
    io:format(" 🌡️  Temperature Sensor Started!~n"),
    io:format(" Device: ~s~n", [DeviceName]),
    io:format(" Service: Environmental Sensing (0x~s)~n", [?ENV_SENSING_SERVICE]),
    io:format("==============================================~n"),
    io:format("~n"),
    io:format("Current temperature: 21.0°C~n"),
    io:format("~n"),
    io:format("Try:~n"),
    io:format("  ble_temperature_sensor:set_temperature(25.5).~n"),
    io:format("~n"),

    %% Save handle
    put(ble_handle, BLE),

    %% Start simulating temperature changes
    spawn_link(fun() -> simulate_temperature(BLE) end),

    {ok, BLE}.

stop() ->
    case get(ble_handle) of
        undefined -> {error, not_started};
        BLE ->
            ble:stop_advertising(BLE),
            ble:stop(BLE),
            erase(ble_handle),
            ok
    end.

%% Set temperature in Celsius
set_temperature(TempC) when is_number(TempC) ->
    case get(ble_handle) of
        undefined -> {error, not_started};
        BLE ->
            Value = encode_temperature(TempC),
            ble:write(BLE, ?TEMPERATURE_CHAR, Value),
            io:format("🌡️  Temperature updated: ~.1f°C~n", [TempC]),
            ok
    end.

%% BLE Temperature format: int16, Celsius * 100
encode_temperature(TempC) ->
    TempInt = round(TempC * 100),
    <<TempInt:16/signed-little>>.

decode_temperature(<<TempInt:16/signed-little>>) ->
    TempInt / 100.0.

%% Simulate temperature variations (like a real sensor)
simulate_temperature(BLE) ->
    BaseTemp = 21.0,
    simulate_temp_loop(BLE, BaseTemp, 0).

simulate_temp_loop(BLE, BaseTemp, Counter) ->
    %% Simulate daily temperature variation
    HourOfDay = (Counter rem 144) / 6.0,  % 0-24 hours (every 10 seconds = 1 hour)
    DailyVariation = 3.0 * math:sin((HourOfDay - 6) * math:pi() / 12),

    %% Add some random noise
    Noise = (rand:uniform() - 0.5) * 0.5,

    Temp = BaseTemp + DailyVariation + Noise,

    %% Update every 10 seconds
    timer:sleep(10000),

    %% Write new value
    Value = encode_temperature(Temp),
    ble:write(BLE, ?TEMPERATURE_CHAR, Value),

    %% Print every update
    io:format("🌡️  Temperature: ~.1f°C~n", [Temp]),

    simulate_temp_loop(BLE, BaseTemp, Counter + 1).

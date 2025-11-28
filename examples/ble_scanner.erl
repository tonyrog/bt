%%% @doc
%%% BLE Scanner - Central Example
%%%
%%% Scan for nearby BLE devices (like Arduino BLE.scan())
%%% Simple and easy!
%%%
%%% Usage:
%%%   1> c(ble_scanner).
%%%   2> ble_scanner:scan().
%%%   3> ble_scanner:scan(10000).  % Scan for 10 seconds
%%%
%%% @end
-module(ble_scanner).
-export([scan/0, scan/1]).
-export([connect_to_heart_rate/1]).

scan() ->
    scan(5000).

scan(Timeout) ->
    io:format("~n"),
    io:format("==============================================~n"),
    io:format(" BLE Scanner~n"),
    io:format(" Scanning for ~w ms...~n", [Timeout]),
    io:format("==============================================~n"),
    io:format("~n"),

    %% Initialize BLE as Central (like Arduino BLE.begin())
    {ok, BLE} = ble:begin_central(),

    %% Scan for devices
    Devices = ble:scan(BLE, Timeout),

    io:format("~n"),
    io:format("Found ~w devices:~n", [length(Devices)]),
    io:format("~n"),

    %% Print each device
    lists:foreach(fun(Device) ->
        ble:print_device(Device)
    end, Devices),

    io:format("~n"),
    io:format("Scan complete!~n"),
    io:format("~n"),

    %% Cleanup
    ble:stop(BLE),

    {ok, Devices}.

%% Example: Connect to a heart rate monitor
connect_to_heart_rate(DeviceAddr) ->
    io:format("~n"),
    io:format("Connecting to Heart Rate Monitor...~n"),
    io:format("Address: ~p~n", [DeviceAddr]),
    io:format("~n"),

    %% Initialize BLE Central
    {ok, BLE} = ble:begin_central(),

    %% Connect to device
    case ble:connect(BLE, DeviceAddr) of
        {ok, Connection} ->
            io:format("Connected!~n"),
            io:format("~n"),

            %% Subscribe to heart rate notifications
            Callback = fun(UUID, Value) ->
                <<_Flags, HeartRate>> = Value,
                io:format("♥ Heart Rate: ~w bpm (UUID: ~p)~n", [HeartRate, UUID])
            end,

            ble:subscribe(BLE, "2A37", Callback),  % Heart Rate Measurement

            io:format("Subscribed to heart rate notifications~n"),
            io:format("Receiving heart rate data...~n"),
            io:format("(Press Ctrl+C to stop)~n"),
            io:format("~n"),

            %% Keep running
            {ok, BLE, Connection};

        Error ->
            io:format("Connection failed: ~p~n", [Error]),
            ble:stop(BLE),
            Error
    end.

%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%    BLE advertising 
%%% @end
%%% Created : 25 Nov 2025 by Tony Rogvall <tony@rogvall.se>

-module(ble_adv).

-export([decode/1]).
-export([get_name/1]).

-define(DATA_TYPE_FLAGS, 16#01).
-define(DATA_TYPE_INCOMP_16BITS_UUIDS, 16#02).
-define(DATA_TYPE_COMP_16BITS_UUIDS, 16#03).
-define(DATA_TYPE_INCOMP_32BITS_UUIDS, 16#04).
-define(DATA_TYPE_COMP_32BITS_UUIDS, 16#05).
-define(DATA_TYPE_INCOMP_128BITS_UUIDS, 16#06).
-define(DATA_TYPE_COMP_128BITS_UUIDS, 16#07).
-define(DATA_TYPE_SHORT_NAME, 16#08).
-define(DATA_TYPE_COMPLETE_NAME, 16#09).
-define(DATA_TYPE_TX_POWER_LEVEL, 16#0A).
-define(DATA_TYPE_DEVICE_CLASS, 16#0B).
-define(DATA_TYPE_SMP_PAIR_HASH_C, 16#0C).
-define(DATA_TYPE_SMP_PAIR_HASH_C192, 16#0D).
-define(DATA_TYPE_SMP_PAIR_RAND_R, 16#0E).
-define(DATA_TYPE_SMP_PAIR_RAND_R192, 16#0F).
-define(DATA_TYPE_DEVICE_ID, 16#10).
-define(DATA_TYPE_SECU_MNGR_TK_VAL, 16#11).
-define(DATA_TYPE_SECU_MNGR_OOB_FLAGS, 16#12).
-define(DATA_TYPE_SLAVE_CONN_INT_RNG, 16#13).
-define(DATA_TYPE_16BITS_SVC_SOL_UUIDS, 16#14).
-define(DATA_TYPE_128BITS_SVC_SOL_UUIDS, 16#15).
-define(DATA_TYPE_SVC_DATA, 16#16).
-define(DATA_TYPE_SVC_DATA_16BITS_UUID, 16#17).
-define(DATA_TYPE_PUB_TARGET_ADDR, 16#18).
-define(DATA_TYPE_RAND_TARGET_ADDR, 16#19).
-define(DATA_TYPE_APPEARANCE, 16#1A).
-define(DATA_TYPE_ADV_INT, 16#1B).
-define(DATA_TYPE_LE_BLT_DEVICE_ADDR, 16#1C).
-define(DATA_TYPE_LE_ROLE, 16#1D).
-define(DATA_TYPE_SMP_PAIR_HASH_C256, 16#1E).
-define(DATA_TYPE_SMP_PAIR_RAND_R256, 16#1F).
-define(DATA_TYPE_32BITS_SVC_SOL_UUIDS, 16#20).
-define(DATA_TYPE_SVC_DATA_32BITS_UUID, 16#21).
-define(DATA_TYPE_SVC_DATA_128BITS_UUID, 16#22).
-define(DATA_TYPE_LE_SECU_CONN_RAND_VAL, 16#23).
-define(DATA_TYPE_URI, 16#24).
-define(DATA_TYPE_INDOOR_POS, 16#25).
-define(DATA_TYPE_TRANS_DISCOV_DATA, 16#26).
-define(DATA_TYPE_LE_SUPPORT_FEAT, 16#27).
-define(DATA_TYPE_CHAN_MAP_UPD_INDIC, 16#28).
-define(DATA_TYPE_PB_ADV, 16#29).
-define(DATA_TYPE_MESH_MSG, 16#2A).
-define(DATA_TYPE_MESH_BEACON, 16#2B).
-define(DATA_TYPE_3D_INFO_DATA, 16#3D).
-define(DATA_TYPE_MANUFACTURER_DATA, 16#FF).

%% AVD Flag 
-define(FLAG_LE_LIMITED_DISC_MODE,       16#01).
-define(FLAG_LE_GENERAL_DISC_MODE,       16#02).
-define(FLAG_BR_EDR_NOT_SUPPORTED,       16#04).
-define(FLAG_LE_BR_EDR_CONTROLLER,       16#08).
-define(FLAG_LE_BR_EDR_HOST,             16#10).

-define(FLAGS_LE_ONLY_LIMITED_DISC_MODE, (16#01 bor 16#04)).
-define(FLAGS_LE_ONLY_GENERAL_DISC_MODE, (16#02 bor 16#04)).

-define(FLAG(Flag, Mask, Name),
	if (((Flag) band (Mask)) =:= (Flag)) -> [Name]; true -> [] end).

%% @doc Parse advertising data to extract device name
%% AD Structure format: Length(1) + Type(1) + Data(Length-1)
decode(Data) ->
    decode(Data, [{raw,Data}]).

decode(<<>>, Acc) ->
    Acc;
decode(<<0, _/binary>>, Acc) ->
    %% Zero length means end of data
    Acc;
decode(<<Length:8, Rest/binary>>, Acc)
  when byte_size(Rest) >= Length ->
    <<AdData:Length/binary, NextRest/binary>> = Rest,
    <<Type,Field/binary>> = AdData,
    {Type1,Field1} = decode_field(Type, Field),
    decode(NextRest, [{Type1,Field1}|Acc]);
decode(_, Acc) ->
    %% Incomplete data
    Acc.

decode_field(?DATA_TYPE_FLAGS, <<Flags>>) ->
    FlagList =
	?FLAG(?FLAG_LE_LIMITED_DISC_MODE, Flags, le_limited_disc_mode) ++
	?FLAG(?FLAG_LE_GENERAL_DISC_MODE, Flags, le_general_disc_mode) ++
	?FLAG(?FLAG_BR_EDR_NOT_SUPPORTED, Flags, br_edr_not_supported) ++
	?FLAG(?FLAG_LE_BR_EDR_CONTROLLER, Flags, le_br_edr_controller) ++
	?FLAG(?FLAG_LE_BR_EDR_HOST, Flags, le_br_edr_host),
    {flags, FlagList};
decode_field(?DATA_TYPE_SHORT_NAME, ShortName) ->
    {short_name, utf8_name(ShortName)};
decode_field(?DATA_TYPE_COMPLETE_NAME, CompleteLocalName) ->
    {name, utf8_name(CompleteLocalName)};
decode_field(?DATA_TYPE_TX_POWER_LEVEL, <<Level>>) ->
    {tx_power_level, Level};
decode_field(?DATA_TYPE_DEVICE_CLASS, Class) ->
    {device_class, Class};
decode_field(?DATA_TYPE_DEVICE_ID, ID) ->
    {device_id, ID};
decode_field(?DATA_TYPE_LE_BLT_DEVICE_ADDR, Addr) ->
    {le_blt_device_addr, Addr};
decode_field(?DATA_TYPE_LE_ROLE,  Role) ->
    {le_role, Role};
decode_field(Type, Field) ->
    {Type, Field}.

get_name(Adv) ->
    case proplists:get_value(name, Adv) of
	undefined ->
	    case proplists:get_value(short_name, Adv) of
		undefined ->
		    "Unknown";
		Name -> Name
	    end;
	Name -> Name
    end.

utf8_name(Data) when is_binary(Data) ->
    unicode:characters_to_list(list_to_binary(name_list(Data))).

%% return list of bytes excluding zero termination
name_list(<<0,_/binary>>) -> [];
name_list(<<C,Cs/binary>>) -> [C|name_list(Cs)];
name_list(<<>>) -> [].

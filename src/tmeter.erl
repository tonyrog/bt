%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    Codec for  HoldPeak HP-90EPD  serial data
%%% @end
%%  Credits: https://alexkaltsas.wordpress.com/2013/04/19/python-script-to-read-data-from-va18b-multimeter/
%%% 
%%% Created : 26 Sep 2024 by Tony Rogvall <tony@rogvall.se>

-module(tmeter).

-export([decode/1]).
-export([encode/2]).
-export([format/1]).
-export([print/1, print/2]).

-export([decode_hp_90epd/1]).

-define(HP_90EPD_0, 2#1111101).
-define(HP_90EPD_1, 2#0000101).
-define(HP_90EPD_2, 2#1011011).
-define(HP_90EPD_3, 2#0011111).
-define(HP_90EPD_4, 2#0100111).
-define(HP_90EPD_5, 2#0111110).
-define(HP_90EPD_6, 2#1111110).
-define(HP_90EPD_7, 2#0010101).
-define(HP_90EPD_8, 2#1111111).
-define(HP_90EPD_9, 2#0111111).
-define(HP_90EPD_L, 2#1101000).
-define(HP_90EPD_N, 2#0000000).
-define(HP_90EPD_NONE, 0).
-define(HP_90EPD_AC,      55).
-define(HP_90EPD_DC,      54).
-define(HP_90EPD_AUTO,    53).
-define(HP_90EPD_PCLINK,  52).
-define(HP_90EPD_MINUS,   51).
-define(HP_90EPD_DIGIT1,  44).
-define(HP_90EPD_DOT1,    43).
-define(HP_90EPD_DIGIT2,  36).
-define(HP_90EPD_DOT2,    35).
-define(HP_90EPD_DIGIT3,  28).
-define(HP_90EPD_DOT3,    27).
-define(HP_90EPD_DIGIT4,  20).
-define(HP_90EPD_MICRO,   19).
-define(HP_90EPD_NANO,    18).
-define(HP_90EPD_KILO,    17).
-define(HP_90EPD_DIOTST,  16).
-define(HP_90EPD_MILLI,   15).
-define(HP_90EPD_PERCENT, 14).
-define(HP_90EPD_MEGA,    13).
-define(HP_90EPD_CONTST,  12).
-define(HP_90EPD_FARAD,   11).
-define(HP_90EPD_OHM,     10).
-define(HP_90EPD_REL,     9).
-define(HP_90EPD_HOLD,    8).
-define(HP_90EPD_AMP,     7).
-define(HP_90EPD_VOLT,    6).
-define(HP_90EPD_HZ,      5).
-define(HP_90EPD_LOWBAT,  4).
-define(HP_90EPD_MINM,    3).
-define(HP_90EPD_TEMP,    2).
-define(HP_90EPD_CELCIUS, 1).
-define(HP_90EPD_MAXM,    0).
-define(HP_90EPD_DIGIT(I,D),
    case (I) of
	1 -> (D) bsl ?HP_90EPD_DIGIT1;
	2 -> (D) bsl ?HP_90EPD_DIGIT2;
	3 -> (D) bsl ?HP_90EPD_DIGIT3;
	4 -> (D) bsl ?HP_90EPD_DIGIT4
    end).
-define(HP_90EPD_DOT(I),
    case (I) of
	1 -> (1 bsl ?HP_90EPD_DOT1);
	2 -> (1 bsl ?HP_90EPD_DOT2);
	3 -> (1 bsl ?HP_90EPD_DOT3)
    end).

encode(Value, Opts) when is_number(Value), is_map(Opts) ->
    F = encode_flags(maps:get(flags, Opts,[])),
    Ubit = case maps:get(unit, Opts, none) of
	       volt    -> ?HP_90EPD_VOLT;
	       amp     -> ?HP_90EPD_AMP;
	       ohm     -> ?HP_90EPD_OHM;
	       farad   -> ?HP_90EPD_FARAD;
	       hertz   -> ?HP_90EPD_HZ;
	       temp    -> ?HP_90EPD_TEMP;
	       none    -> ?HP_90EPD_NONE
	   end,
    Sbit = case maps:get(scale, Opts, none) of
	       nano    -> ?HP_90EPD_NANO;
	       micro   -> ?HP_90EPD_MICRO;
	       milli   -> ?HP_90EPD_MILLI;
	       kilo    -> ?HP_90EPD_KILO;
	       mega    -> ?HP_90EPD_MEGA;
	       percent -> ?HP_90EPD_PERCENT;
	       none    -> ?HP_90EPD_NONE
	   end,
    Sign = if Value < 0 -> $-; true -> $+ end,
    Value1 = abs(Value),
    Int = trunc(Value1),                   %% integer part
    Frac = trunc((Value1 - Int) * 10000),  %% 4 digits
    %% fixme round?
    Num = case {integer_to_list(Int),integer_to_list(10000+Frac)} of
	      {[D1,D2,D3,D4|_],_} -> [Sign,D1,D2,D3,D4];  %% too large
	      {[D1,D2,D3],[_,F1|_]} -> [Sign,D1,D2,D3,$.,F1];
	      {[D1,D2],[_,F1,F2|_]} -> [Sign,D1,D2,$.,F1,F2];
	      {[D1],[_,F1,F2,F3|_]} -> [Sign,D1,$.,F1,F2,F3];
	      {[],[_,F1,F2,F3,F4|_]}  -> [Sign,$.,F1,F2,F3,F4]
	  end,
    io:format("Num: ~s~n",[Num]),
    << <<0:4, B:4>> || <<B:4>> <= << (F + (1 bsl Ubit) + (1 bsl Sbit) + encode_hp_90epd_num(Num,1)):56>> >>.



encode_flags([]) -> 0;
encode_flags([F|Fs]) ->
    Fbit = case F of
	       ac -> ?HP_90EPD_AC;
	       dc -> ?HP_90EPD_DC;
	       auto ->  ?HP_90EPD_AUTO;
	       diotst -> ?HP_90EPD_DIOTST;
	       contst -> ?HP_90EPD_CONTST;
	       rel -> ?HP_90EPD_REL;
	       hold -> ?HP_90EPD_HOLD;
	       lowbat -> ?HP_90EPD_LOWBAT
	   end,
    (1 bsl Fbit) + encode_flags(Fs).

encode_hp_90epd_num([$+|Ds],I) ->
    encode_hp_90epd_num(Ds,I);
encode_hp_90epd_num([$-|Ds],I) ->
    (1 bsl ?HP_90EPD_MINUS) + encode_hp_90epd_num(Ds,I);
encode_hp_90epd_num([D|Ds],I) when D >= $0, D =< $9 ->
    Di = case D of
	     $0 -> ?HP_90EPD_0;
	     $1 -> ?HP_90EPD_1;
	     $2 -> ?HP_90EPD_2;
	     $3 -> ?HP_90EPD_3;
	     $4 -> ?HP_90EPD_4;
	     $5 -> ?HP_90EPD_5;
	     $6 -> ?HP_90EPD_6;
	     $7 -> ?HP_90EPD_7;
	     $8 -> ?HP_90EPD_8;
	     $9 -> ?HP_90EPD_9
	 end,
    ?HP_90EPD_DIGIT(I,Di) + encode_hp_90epd_num(Ds,I+1);
encode_hp_90epd_num([$.|Ds],I) -> 
    ?HP_90EPD_DOT(I) + encode_hp_90epd_num(Ds,I);
encode_hp_90epd_num([],_) -> 0.

    
%% Each data byte is <<seq:4,nybble:4>> where
%% seq=1..14 and nybble=0..15
decode(Data) when is_binary(Data) ->
    case Data of
	<<Data1:14/binary, _Data2/binary>> ->
	    try decode_hp_90epd(<< << <<B:4>> || 
				       <<_I:4,B:4>> <= Data1 >>/binary >>) of
		Value ->
		    {ok, Value}
	    catch 
		error:_ -> {error, {bad_data, Data}}
	    end;
	_ ->
	    more
    end.

digit(?HP_90EPD_0) -> "0";
digit(?HP_90EPD_1) -> "1";
digit(?HP_90EPD_2) -> "2";
digit(?HP_90EPD_3) -> "3";
digit(?HP_90EPD_4) -> "4";
digit(?HP_90EPD_5) -> "5";
digit(?HP_90EPD_6) -> "6";
digit(?HP_90EPD_7) -> "7";
digit(?HP_90EPD_8) -> "8";
digit(?HP_90EPD_9) -> "9";
digit(?HP_90EPD_N) -> "";
digit(?HP_90EPD_L) -> "L";
digit(_) -> "".

dot(1) -> ".";
dot(_) -> "".

flag(1,Flag) -> [Flag];
flag(0,_) -> "".
    
%% HoldPeak HP-90EPD
decode_hp_90epd(<<Ac:1, Dc:1, Auto:1, _Pclink:1, Minus:1,
		  Digit1:7,Dot1:1,Digit2:7,Dot2:1,Digit3:7,Dot3:1,Digit4:7,
		  Micro:1, Nano:1, Kilo:1, Diotst:1, Milli:1, Percent:1, 
		  Mega:1, Contst:1, Farad:1, Ohm:1, Rel:1, Hold:1, Amp:1,
		  Volt:1, Hertz:1, Lowbat:1, _Minm:1, Temp:1,
		  Celcius:1, _Maxm:1>>) ->
    Value = 
	flag(Minus, $-) ++
	digit(Digit1) ++ dot(Dot1) ++
	digit(Digit2) ++ dot(Dot2) ++
	digit(Digit3) ++ dot(Dot3) ++
	digit(Digit4),
    Flags = lists:append(
	      [flag(Ac, ac),
	       flag(Dc, dc),
	       flag(Auto, auto),
	       flag(Diotst, diotst),
	       flag(Contst, contst),
	       flag(Rel, rel),
	       flag(Hold, hold),
	       flag(Celcius, celcius),
	       %% flag(Minm, "Min"),
	       %% flag(Maxm, "Max")
	       flag(Lowbat, lowbat)]),
    Scale = case lists:append(
		   [flag(Nano, nano),
		    flag(Micro, micro), 
		    flag(Kilo, kilo),
		    flag(Milli,milli),
		    flag(Mega,mega),
		    flag(Percent,percent)
		   ]) of
		[S] -> S;
		[] -> none
	    end,
    Unit = case lists:append(
		  [flag(Ohm,ohm),flag(Amp,amp),flag(Volt,volt),
		   flag(Farad,farad),flag(Hertz,hertz),flag(Temp,temp)
		  ]) of
	       [U] -> U;
	       [] -> none
	   end,
    {Value, #{ flags => Flags, scale => Scale, unit => Unit}}.


format({Value, Opts}) ->
    Fs = maps:get(flags, Opts, []),
    io_lib:format("~s ~s~s~s",[Value,
			     format_scale(maps:get(scale, Opts, none)),
			     format_unit(maps:get(unit, Opts, none),Fs),
			     format_flags(Fs)]).

print(Data) -> print(user, Data).
print(Fd,Data) -> io:format(Fd,"~s\n",[format(Data)]).
			 
format_scale(nano) -> "n";
format_scale(micro) -> "u";
format_scale(milli) -> "m";
format_scale(kilo) -> "k";
format_scale(mega) -> "M";
format_scale(percent) -> "%";
format_scale(none) -> "".

format_unit(ohm,_) -> "Ohm";
format_unit(amp,_) -> "A";
format_unit(volt,_) -> "V";
format_unit(farad,_) -> "F";
format_unit(hertz,_) -> "Hz";
format_unit(temp,Fs) ->
    case lists:member(celcius, Fs) of
	true -> "(C)";
	false -> "(F)"
    end;
format_unit(none,_) -> "".

format_flags([]) -> "";
format_flags([F|Fs]) ->
    case F of
	ac -> " AC";
	dc -> " DC";
	auto -> " Auto";
	diotst -> " Diotst";
	contst -> " Contst";
	rel -> " Rel";
	hold -> " Hold";
	lowbat -> " Lowbat";
	celcius -> ""
    end ++ format_flags(Fs).

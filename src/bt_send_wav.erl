%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Send wav file to bluetooth headset
%%% @end
%%% Created : 20 Dec 2021 by Tony Rogvall <tony@rogvall.se>

-module(bt_send_wav).

-export([send/2]).

send(Address, WavFile) ->
    case file:open(WavFile, [read, raw, binary]) of
	{ok,Fd} ->
	    case alsa_wav:read_header(Fd) of
		{ok, Header} ->
		    io:format("wav header = ~p\n", [Header]),
		    {ok,PortList} = bt_sdp:protocol_port(Address, "AudioSink"),
		    {value,{_,L2Port}} = lists:keysearch("L2CAP", 1, PortList),
		    {value,{_,AVPort}} = lists:keysearch("AVDTP", 1, PortList),
		    {ok, L2CAP} = l2cap:open(Address, L2Port),
		    {ok, AVDTP} = l2cap:open(Address, AVPort),
		    send_(Fd, AVDTP, L2CAP),
		    file:close(Fd),
		    l2cap:close(L2CAP),
		    l2cap:close(AVDTP),
		    ok;
		Error ->
		    file:close(Fd),
		    Error
	    end;
	Error ->
	    Error
    end.

send_(Fd, AVDTP, L2CAP) ->
    Enc = alsa_sbc:new(sbc),
    {ok, _FrameLength} = alsa_sbc:get_frame_length(Enc),
    {ok, FrameDuration} = alsa_sbc:get_frame_length(Enc),
    {ok, CodeSize} = alsa_sbc:get_codesize(Enc),
    send_(Fd, AVDTP, L2CAP, Enc, CodeSize, FrameDuration, <<>>).

send_(Fd, AVDTP, L2CAP, Enc, CodeSize, FrameDuration, Data0) ->
    case file:read(Fd, CodeSize) of
	{ok, Data1} ->
	    {ok,Fs,Data2} = alsa_sbc:encode(Enc, <<Data0/binary,Data1/binary>>),
	    FRAG = 2#00000000, 
	    NFrames = length(Fs),
	    io:format("NFrames = ~w, duration = ~f ms\n", 
		      [NFrames, (NFrames*FrameDuration)/1000]),
	    l2cap:send(L2CAP, [(FRAG+NFrames), lists:reverse(Fs)]),
	    timer:sleep(trunc((NFrames*FrameDuration)/1000)),
	    send_(Fd, AVDTP, L2CAP, Enc, CodeSize, FrameDuration, Data2);
	eof ->
	    ok
    end.
    

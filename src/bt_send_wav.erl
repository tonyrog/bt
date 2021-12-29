%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Send wav file to bluetooth headset
%%% @end
%%% Created : 20 Dec 2021 by Tony Rogvall <tony@rogvall.se>

-module(bt_send_wav).

-export([send/2]).

-define(PAYLOAD_TYPE, 96). %% SBC payload? what number 96????
-define(SOURCE_NUM, 1).

send(Address, WavFile) ->
    case file:open(WavFile, [read, raw, binary]) of
	{ok,Fd} ->
	    case alsa_wav:read_header(Fd) of
		{ok, Wav} ->
		    io:format("wav header = ~p\n", [Wav]),
		    {ok,PortList} = bt_sdp:protocol_port(Address, "AudioSink"),
		    {value,{_,L2Port}} = lists:keysearch("L2CAP", 1, PortList),
		    {value,{_,AVPort}} = lists:keysearch("AVDTP", 1, PortList),
		    io:format("L2Port=~w, AVPort=~w\n", [L2Port,AVPort]),
		    %% {ok, AVDTP} = l2cap:open(Address, AVPort),
		    {ok, L2CAP} = l2cap:open(Address, L2Port),
		    AVDTP = false,
		    send_(Fd, Wav, AVDTP, L2CAP),
		    file:close(Fd),
		    l2cap:close(L2CAP),
		    %%l2cap:close(AVDTP)
		    ok;
		Error ->
		    file:close(Fd),
		    Error
	    end;
	Error ->
	    Error
    end.

send_(Fd, Wav, AVDTP, L2CAP) ->
    Rate = maps:get(rate, Wav),
    Channels = maps:get(channels, Wav),
    s16_le = maps:get(format, Wav), %% assert!
    A2DP_Conf = [{rate, Rate},
		 {channel_mode, 
		  case Channels of
		      2 -> stereo;
		      1 -> mono
		  end}],
    io:format("a2dp conf = ~w\n", [A2DP_Conf]),
    %% {ok,Enc} = alsa_sbc:new(msbc),
    {ok,Enc} = alsa_sbc:new(a2dp, A2DP_Conf),
    RtpSource = rtp:init_source(?PAYLOAD_TYPE, ?SOURCE_NUM, []),
    {ok, SBCFrameLength} = alsa_sbc:get_frame_length(Enc),
    {ok, FrameDurationUS} = alsa_sbc:get_frame_duration(Enc),
    {ok, CodeSize} = alsa_sbc:get_codesize(Enc),
    {ok, Mtu} = bt_l2cap:get_mtu(L2CAP),
    FramesPerSBCFrame = CodeSize div (Channels*2),
    DurationPerSBCFrameUS = trunc((FramesPerSBCFrame*1000000)/Rate),
    io:format("FrameLength ~w\n", [SBCFrameLength]),
    io:format("FrameDuration ~wus\n", [FrameDurationUS]),
    io:format("CodeSize ~w\n", [CodeSize]),
    io:format("Mtu ~w\n", [Mtu]),
    io:format("FramesPerSBCFrame=~w\n", [FramesPerSBCFrame]),
    io:format("DurationPerSBCFrame=~wus\n", [DurationPerSBCFrameUS]),
    send_(Fd, AVDTP, L2CAP, Enc, CodeSize, FramesPerSBCFrame, 
	  DurationPerSBCFrameUS, RtpSource, <<>>).

send_(Fd, AVDTP, L2CAP, Enc, CodeSize, FramesPerSBCFrame, 
      DurationPerSBCFrameUS, RtpSource, Data0) ->
    case file:read(Fd, CodeSize) of
	{ok, Data1} ->
	    {ok,Fs,Data2} = alsa_sbc:encode(Enc, <<Data0/binary,Data1/binary>>),
	    FRAG = 2#00000000,
	    NSBCFrames = length(Fs),
	    NFrames = FramesPerSBCFrame*NSBCFrames,
	    SBCPacket = iolist_to_binary([(FRAG+NSBCFrames),
					  lists:reverse(Fs)]),
	    {L2CapPacket,RtpSource1} = rtp:next(RtpSource,NFrames,SBCPacket),
	    io:format("#frames = ~w, packet size = ~p\n", 
		      [NFrames, byte_size(L2CapPacket)]),
	    l2cap:send(L2CAP, L2CapPacket),
	    timer:sleep(DurationPerSBCFrameUS div 1000),
	    send_(Fd,AVDTP,L2CAP,Enc,CodeSize,FramesPerSBCFrame,
		  DurationPerSBCFrameUS,RtpSource1,Data2);
	eof ->
	    ok
    end.

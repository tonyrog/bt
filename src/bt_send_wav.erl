%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Send wav file to bluetooth headset
%%% @end
%%% Created : 20 Dec 2021 by Tony Rogvall <tony@rogvall.se>

-module(bt_send_wav).

-export([file/2]).
-export([test_jbl/0, test_jabra/0]).

%% -define(PAYLOAD_TYPE, 96). %% SBC payload? what number 96????
%% -define(SOURCE_NUM, 1).

-include("../include/avdtp.hrl").

test_jbl() ->
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    File   = filename:join(Sounds, "Front_Left.wav"),
    bt_send_wav:file("FC:A8:9A:A9:10:30", File).

test_jabra() ->
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    File   = filename:join(Sounds, "Front_Left.wav"),
    bt_send_wav:file("50:C2:ED:5A:06:ED", File).

file(Address, WavFile) ->
    case file:open(WavFile, [read, raw, binary]) of
	{ok,Fd} ->
	    case alsa_wav:read_header(Fd) of
		{ok, Wav} ->
		    io:format("wav header = ~p\n", [Wav]),
		    {ok,PortList} = bt_sdp:protocol_port(Address, "AudioSink"),
		    {value,{_,L2Port}} = lists:keysearch("L2CAP", 1, PortList),
		    {value,{_,AVDTP}} = lists:keysearch("AVDTP", 1, PortList),
		    io:format("L2Port=~w, AVDTP=~w\n", [L2Port,AVDTP]),
		    {ok, S} = l2cap:connect(Address, L2Port),
		    %% Get protocol info
		    Trans = 13,
		    {ok,[Info|_]} = bt_avdtp:discover(S, Trans),
		    ACP = Info#seid_info.seid,
		    {ok,Caps} = bt_avdtp:get_capabilities(S, Trans, ACP),
		    io:format("Caps = ~w\n", [Caps]),
		    {ok,_Open} = bt_avdtp:open(S,Trans,ACP),
		    {ok,_Start} = bt_avdtp:start(S,Trans,ACP),
		    send_(Fd, Wav, S, Info),
		    file:close(Fd),
		    l2cap:close(S),
		    ok;
		Error ->
		    file:close(Fd),
		    Error
	    end;
	Error ->
	    Error
    end.

send_(Fd, Wav, L2CAP, Info) ->
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
    PayloadType = Info#seid_info.media_type,
    SSRC = Info#seid_info.seid, %% ?? 
    RtpSource = rtp:init_source(PayloadType, SSRC, []),
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
    send_(Fd, L2CAP, Enc, CodeSize, FramesPerSBCFrame, 
	  DurationPerSBCFrameUS, RtpSource, <<>>).

send_(Fd, L2CAP, Enc, CodeSize, FramesPerSBCFrame, 
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
	    send_(Fd,L2CAP,Enc,CodeSize,FramesPerSBCFrame,
		  DurationPerSBCFrameUS,RtpSource1,Data2);
	eof ->
	    ok
    end.

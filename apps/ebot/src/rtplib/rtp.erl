%%%----------------------------------------------------------------------
%%% Copyright (c) 2008-2012 Peter Lemenkov <lemenkov@gmail.com>
%%%
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification,
%%% are permitted provided that the following conditions are met:
%%%
%%% * Redistributions of source code must retain the above copyright notice, this
%%% list of conditions and the following disclaimer.
%%% * Redistributions in binary form must reproduce the above copyright notice,
%%% this list of conditions and the following disclaimer in the documentation
%%% and/or other materials provided with the distribution.
%%% * Neither the name of the authors nor the names of its contributors
%%% may be used to endorse or promote products derived from this software
%%% without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ''AS IS'' AND ANY
%%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%%----------------------------------------------------------------------

-module(rtp).
-author('lemenkov@gmail.com').

-include("../../include/rtplib/rtcp.hrl").
-include("../../include/rtplib/rtp.hrl").

% FIXME move to the header?
-define(MBZ, 0).

-export([encode/1]).
-export([decode/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Decoding functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(<<?RTP_VERSION:2, Padding:1, ExtensionFlag:1, CC:4, Marker:1, PayloadType:7, SequenceNumber:16, Timestamp:32, SSRC:32, Rest/binary>>) when PayloadType =< 34 ->
    Size = CC * 4,
    <<CSRCs:Size/binary, Data/binary>> = Rest,
    {ok, Payload, Extension} = decode_extension(Data, ExtensionFlag),
    {ok, #rtp{
        padding = Padding,
        marker = Marker,
        payload_type = PayloadType,
        sequence_number = SequenceNumber,
        timestamp = Timestamp,
        ssrc = SSRC,
        csrcs = [CSRC || <<CSRC:32>> <= CSRCs],
        extension = Extension,
        payload = Payload
    }};
decode(<<?RTP_VERSION:2, Padding:1, ExtensionFlag:1, CC:4, Marker:1, PayloadType:7, SequenceNumber:16, Timestamp:32, SSRC:32, Rest/binary>>) when 96 =< PayloadType ->
    Size = CC * 4,
    <<CSRCs:Size/binary, Data/binary>> = Rest,
    {ok, Payload, Extension} = decode_extension(Data, ExtensionFlag),
    {ok, #rtp{
        padding = Padding,
        marker = Marker,
        payload_type = PayloadType,
        sequence_number = SequenceNumber,
        timestamp = Timestamp,
        ssrc = SSRC,
        csrcs = [CSRC || <<CSRC:32>> <= CSRCs],
        extension = Extension,
        payload = case get(PayloadType) of undefined -> Payload; dtmf -> {ok, Dtmf} = decode_dtmf(Payload), Dtmf; _ ->
            Payload end
    }};

decode(<<?RTCP_VERSION:2, _:7, PayloadType:7, _Rest/binary>> = Binary) when 64 =< PayloadType, PayloadType =< 82 ->
    rtcp:decode(Binary);

decode(_) ->
    {error, badarg}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Decoding helpers
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_extension(Data, 0) ->
    {ok, Data, null};
decode_extension(<<Type:16, Length:16, Payload:Length/binary, Data/binary>>, 1) ->
    {ok, Data, #extension{type = Type, payload = Payload}}.

%%
%% RFC 2198, 2833, and 4733 decoding helpers
%%

% DTMF with zero duration is possible. Im teams that this events lasts forever.
decode_dtmf(<<Event:8, 0:1, _Mbz:1, Volume:6, Duration:16>>) ->
    {ok, #dtmf{event = Event, eof = false, volume = Volume, duration = Duration}};
decode_dtmf(<<Event:8, 1:1, _Mbz:1, Volume:6, Duration:16>>) ->
    {ok, #dtmf{event = Event, eof = true, volume = Volume, duration = Duration}};
decode_dtmf(<<Dtmf:4/binary, _Rest/binary>>) ->
    error_logger:warning_msg("Broken DTMF generator (Jitsi?)~n"),
    decode_dtmf(Dtmf).

% FIXME Tone with zero duration SHOULD be ignored (just drop it?)
decode_tone(<<Modulation:9, Divider:1, Volume:6, Duration:16, Rest/binary>>) ->
    Frequencies = [Frequency || <<?MBZ:4, Frequency:12>> <= Rest],
    {ok, #tone{modulation = Modulation, divider = Divider, volume = Volume, duration = Duration, frequencies = Frequencies}}.

decode_red(RedundantPayload) ->
    decode_red_headers(RedundantPayload, []).

decode_red_headers(<<0:1, PayloadType:7, Data/binary>>, Headers) ->
    decode_red_payload(Headers ++ [{PayloadType, 0, 0}], Data);
decode_red_headers(<<1:1, PayloadType:7, TimeStampOffset:14, BlockLength:10, Data/binary>>, Headers) ->
    decode_red_headers(Data, Headers ++ [{PayloadType, TimeStampOffset, BlockLength}]).

decode_red_payload(Headers, Payload) ->
    decode_red_payload(Headers, Payload, []).
decode_red_payload([{PayloadType, 0, 0}], <<Payload/binary>>, Payloads) ->
    {ok, Payloads ++ [{PayloadType, 0, Payload}]};
decode_red_payload([{PayloadType, TimeStampOffset, BlockLength} | Headers], Data, Payloads) ->
    <<Payload:BlockLength/binary, Rest/binary>> = Data,
    decode_red_payload(Headers, Rest, Payloads ++ [{PayloadType, TimeStampOffset, Payload}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Encoding functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode(#rtp{padding = P, marker = M, payload_type = PT, sequence_number = SN, timestamp = TS, ssrc = SSRC, csrcs = CSRCs, extension = X, payload = Payload}) when is_binary(Payload) ->
    CC = length(CSRCs),
    CSRC_Data = <<<<CSRC:32>> || CSRC <- CSRCs>>,
    {ExtensionFlag, ExtensionData} = encode_extension(X),
    <<?RTP_VERSION:2, P:1, ExtensionFlag:1, CC:4, M:1, PT:7, SN:16, TS:32, SSRC:32, CSRC_Data/binary, ExtensionData/binary, Payload/binary>>;
encode(#rtp{payload = #dtmf{} = Payload} = Rtp) ->
    encode(Rtp#rtp{payload = encode_dtmf(Payload)});
encode(#rtp{payload = #tone{} = Payload} = Rtp) ->
    encode(Rtp#rtp{payload = encode_tone(Payload)});

encode(#rtcp{} = Rtcp) ->
    rtcp:encode(Rtcp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Encoding helpers
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_extension(null) ->
    {0, <<>>};
encode_extension(#extension{type = Type, payload = Payload}) ->
    Length = size(Payload),
    {1, <<Type:16, Length:16, Payload:Length/binary>>}.

%%
%% RFC 2198, 2833, and 4733 encoding helpers
%%

encode_dtmf(#dtmf{event = Event, eof = false, volume = Volume, duration = Duration}) ->
    <<Event:8, 0:1, 0:1, Volume:6, Duration:16>>;
encode_dtmf(#dtmf{event = Event, eof = true, volume = Volume, duration = Duration}) ->
    <<Event:8, 1:1, 0:1, Volume:6, Duration:16>>.

encode_tone(#tone{modulation = Modulation, divider = Divider, volume = Volume, duration = Duration, frequencies = Frequencies}) ->
    FrequenciesBin = <<<<0:4, Frequency:12>> || Frequency <- Frequencies>>,
    <<Modulation:9, Divider:1, Volume:6, Duration:16, FrequenciesBin/binary>>.

encode_red(RedundantPayloads) ->
    encode_red(RedundantPayloads, <<>>, <<>>).
encode_red([{PayloadType, _, Payload}], HeadersBinary, PayloadBinary) ->
    <<HeadersBinary/binary, 0:1, PayloadType:7, PayloadBinary/binary, Payload/binary>>;
encode_red([{PayloadType, TimeStampOffset, Payload} | RedundantPayloads], HeadersBinary, PayloadBinary) ->
    BlockLength = size(Payload),
    encode_red(RedundantPayloads, <<HeadersBinary/binary, 1:1, PayloadType:7, TimeStampOffset:14, BlockLength:10>>, <<PayloadBinary/binary, Payload/binary>>).

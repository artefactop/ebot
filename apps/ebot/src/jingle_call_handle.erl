%%%-------------------------------------------------------------------
%%% @author pepe
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Feb 2014 1:18 PM
%%%-------------------------------------------------------------------
-module(jingle_call_handle).
-author("pepe").

-behaviour(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include("../include/rtplib/rtcp.hrl").
-include("../include/rtplib/rtp.hrl").

-define(NS_JINGLE_1, 'urn:xmpp:jingle:1').

-define(LISTEN_IP, {0, 0, 0, 0}).
-define(SOCKOPTS, [binary, {active, once}]).
-define(FRECUENCY, 1 / 8000).     %%TODO calculate Frecuency codec dependant
-define(TMIN, 5000).     %% 5 seconds

%% GEN SERVER
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% API
-export([
    start_link/1
]).

-record(qos, {
    mos :: float(),
    round_trip_delay = 0.0 :: float(),
    jitter = 0.0 :: float(),
    packet_loss = 0.0 :: float()
}).

-type qos() :: #qos{}.

%% TODO save state in ets one for each ssrc

-record(state, {
    sid :: binary(),
    ssrc :: non_neg_integer(),
    origin_jid :: exmpp_jid:jid(),
    destination_jid :: exmpp_jid:jid(),
    rtp :: inet:posix(),
    rtcp :: inet:posix(),
    destination_addr :: {inet:ip_address(), inet:port_number()},
    last_timestamp :: erlang:timestamp(),
    ts_offset = 0 :: non_neg_integer(),
    last_rtp_packet_timestamp = 0 :: non_neg_integer(),
    last_sequence_number = 0 :: non_neg_integer(),
    lost_packets = 0 :: non_neg_integer(),
    fraction_lost_packets = 0 :: non_neg_integer(),
    dropped_packets = 0 :: non_neg_integer(),
    npackets = 0 :: non_neg_integer(),
    noctets = 0 :: non_neg_integer(),
    destination_ssrc :: non_neg_integer(),
    key :: binary(),
    mask :: binary(),
    qos :: qos(),
    sr_ntp = 0 :: non_neg_integer(),
    timeout = 2500 :: non_neg_integer()
}).

start_link(_Key) ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({Session, Packet}, State) ->
    Result =
        case jingle:get_action(Packet) of
            undefined -> State;
            'session-initiate' ->
                {noreply, session_initiate(Session, Packet, State)};
            'session-terminate' ->
                session_terminate(Session, Packet, State),
                {stop, normal, State};
            _ -> {noreply, State}
        end,
    Result;

handle_info(send_sr, State) ->
    % Do the action
    NTP = rtp_utils:ntp_timestamp(),
    Rblocks = [
        #rblock{
            ssrc = State#state.destination_ssrc,
            fraction = State#state.fraction_lost_packets,
            lost = State#state.lost_packets,
            last_seq = State#state.last_sequence_number,
            jitter = trunc(State#state.qos#qos.jitter),
            lsr = State#state.sr_ntp bsr 32,
            dlsr = trunc((NTP - State#state.sr_ntp) / 65536)  %%FIXME negative values ?
        }
    ],    %%TODO fix
    SR = #sr{
        ssrc = State#state.ssrc,
        ntp = NTP,
        timestamp = trunc((NTP bsr 32) * (1 / ?FRECUENCY)) + State#state.ts_offset, %%  sec * (1/ ?FRECUENCY) = timestamp
        packets = State#state.npackets,
        octets = State#state.noctets,
        rblocks = Rblocks},
    lager:info("SID:~p, SR: ~p",
        [State#state.sid, SR]),
    SRData = rtcp:encode(SR),
    {IP, Port} = State#state.destination_addr,
    send(State#state.rtcp, IP, Port + 1, SRData),
    C = 1, %% priori calculated interval according 5% target for the control bandwidth
    Timeout = trunc(State#state.timeout + random:uniform() * max(?TMIN, C * 2)),
    % Start new timer
    erlang:send_after(Timeout, self(), send_sr),
    {noreply, State#state{timeout = Timeout}};

handle_info({udp, Sock, SrcIP, SrcPort, Data},
    #state{sid = Sid, rtp = Sock, npackets = NPackets,
        last_sequence_number = LastSequenceNumber, qos = QOS} = State) ->
    lager:debug("RTP data received from: ~p:~p ", [SrcIP, SrcPort]),
    inet:setopts(Sock, [{active, once}]),
    lager:debug("destination addr: ~p ", [State#state.destination_addr]),

    NewState =
        case rtp:decode(Data) of
            {ok, RTP} ->
                lager:debug("SID: ~p, SSRC: ~p, CSRCS:~p, PayloadType: ~p, SequenceNumber: ~p, Timestamp: ~p",
                    [Sid, RTP#rtp.ssrc, RTP#rtp.csrcs, RTP#rtp.payload_type, RTP#rtp.sequence_number, RTP#rtp.timestamp]),

                Octets = State#state.noctets + byte_size(RTP#rtp.payload),
                LostPackets = RTP#rtp.sequence_number - (case NPackets of 0 ->
                    RTP#rtp.sequence_number; _ ->
                    LastSequenceNumber + 1 end) + State#state.lost_packets,
                FractionLostPackets = RTP#rtp.sequence_number - (case NPackets of 0 ->
                    RTP#rtp.sequence_number; _ ->
                    LastSequenceNumber + 1 end) + State#state.fraction_lost_packets,
                Loss = loss_packets(LostPackets, NPackets + 1),

                case RTP#rtp.sequence_number < LastSequenceNumber of
                    true ->
                        State#state{
                            last_timestamp = os:timestamp(),
                            npackets = NPackets + 1,
                            dropped_packets = State#state.dropped_packets + 1,
                            lost_packets = LostPackets,
                            fraction_lost_packets = FractionLostPackets,
                            noctets = Octets,
                            qos = QOS#qos{packet_loss = Loss}
                        };
                    _ ->
                        Redirect = RTP#rtp{ssrc = State#state.ssrc},
                        RedirectData = rtp:encode(Redirect),
                        case State#state.destination_addr of
                            {DstIP, DstPort} ->
                                send(State#state.rtp, DstIP, DstPort, RedirectData);
                            _ ->
                                ok
                        end,

                        Jitter =
                            case NPackets == 0 of
                                true -> 0;
                                _ ->
                                    {_, RecivedSecondsOld, _} = State#state.last_timestamp,
                                    {_, RecivedSecondsNew, _} = os:timestamp(),

                                    jitter(QOS#qos.jitter,
                                        RecivedSecondsOld,
                                        RecivedSecondsNew,
                                        State#state.last_rtp_packet_timestamp,
                                        RTP#rtp.timestamp,
                                        ?FRECUENCY
                                    )
                            end,

                        State#state{
                            last_timestamp = os:timestamp(),
                            last_rtp_packet_timestamp = RTP#rtp.timestamp,
                            destination_ssrc = RTP#rtp.ssrc,
                            npackets = NPackets + 1,
                            last_sequence_number = RTP#rtp.sequence_number,
                            lost_packets = LostPackets,
                            fraction_lost_packets = FractionLostPackets,
                            noctets = Octets,
                            qos = QOS#qos{jitter = Jitter, packet_loss = Loss}
                        }
                end;
            Error ->
                lager:debug("RTP decode error: ~p", [Error]),
                State#state{last_timestamp = os:timestamp(), npackets = NPackets + 1}
        end,
    {noreply, NewState};

handle_info({udp, Sock, SrcIP, SrcPort, Data},
    #state{rtcp = Sock, qos = QOS} = State) ->
    NTP = rtp_utils:ntp_timestamp(),
    lager:debug("RTCP data received from: ~p:~p", [SrcIP, SrcPort]),
    inet:setopts(Sock, [{active, once}]),
    NewState =
        case rtcp:decode(Data) of
            {ok, RTCP} ->
                case rtp_utils:get_rtcp_report(RTCP) of
                    undefined -> State;
                    #sr{} = SR ->
                        lager:info("RECEIVED SR sid:~p ssrc:~p, ntp:~p timestamp:~p packets:~p octets:~p",
                            [State#state.sid, SR#sr.ssrc, SR#sr.ntp, SR#sr.timestamp, SR#sr.packets, SR#sr.octets]),
                        RBlock = #rblock{
                            ssrc = SR#sr.ssrc,
                            fraction = State#state.fraction_lost_packets,
                            lost = State#state.lost_packets,
                            last_seq = State#state.last_sequence_number,
                            jitter = trunc(QOS#qos.jitter),
                            lsr = SR#sr.ntp bsr 32,
                            dlsr = trunc((NTP - State#state.sr_ntp) / 65536)  %%FIXME negative values ?
                        },
                        {IP, Port} = State#state.destination_addr,
                        RR = #rr{ssrc = State#state.ssrc, rblocks = [RBlock]},
                        lager:info("SID:~p, RR: ~p",
                            [State#state.sid, RR]),
                        RRData = rtcp:encode(RR),
                        send(State#state.rtcp, IP, Port + 1, RRData),
                        State#state{fraction_lost_packets = 0, sr_ntp = SR#sr.ntp};
                    #rr{} = RR ->
                        lager:info("RECEIVED RR sid:~p ssrc:~p, rblocks:~p ijs:~p ",
                            [State#state.sid, RR#rr.ssrc, RR#rr.rblocks, RR#rr.ijs]),

                        lists:foreach(fun(Block) ->
                            RTD = round_trip_delay(NTP, Block#rblock.lsr, Block#rblock.dlsr),
                            lager:info("sid:~p ssrc:~p round_trip_delay:~p ",
                                [State#state.sid, RR#rr.ssrc, RTD])
                        end, RR#rr.rblocks),
                        State#state{fraction_lost_packets = 0};
                    _ ->
                        lager:info("sid:~p, RTCP: ~p",
                            [State#state.sid, RTCP])
                end;
            Error ->
                lager:debug("RTCP decode error: ~p", [Error]),
                State
        end,
    {noreply, NewState};

handle_info(Request, State) ->
    lager:debug("Unknown request: ~p ~nState:~p ", [Request, State]),
    {noreply, State}.

terminate(Reason, State) ->
    lager:info("Terminating call process, reason ~p", [Reason]),
    lager:info("Call stats caller:~p, sid:~p, total_packets:~p, lost_packets:~p",
        [exmpp_jid:to_binary(State#state.origin_jid), State#state.sid, State#state.npackets + State#state.lost_packets, State#state.lost_packets]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
session_initiate(Session, Packet, _State) ->
    %%MAKE session-accept
    Result = exmpp_iq:result(Packet),
    exmpp_session:send_packet(Session, Result),

    Sid = jingle:get_sid(Packet),
    Jingle = exmpp_xml:get_element(Packet, jingle),
    lager:debug("Jingle:~p", [Jingle]),
    Content = jingle:get_content(Packet),
    lager:debug("Content:~p", [Content]),

    Transport = exmpp_xml:get_element(Content, transport),
    lager:debug("Transport:~p", [Transport]),

    [Candidate | _] = exmpp_xml:get_elements(Transport, candidate),
    lager:debug("Candidate:~p", [Candidate]),
    Sender = exmpp_jid:parse(exmpp_stanza:get_sender(Packet)),
    Recipient = exmpp_jid:parse(exmpp_stanza:get_recipient(Packet)),
    lager:debug("Recipient:~p", [Recipient]),

    ResponderReplaced = jingle:replace_responder(Packet, exmpp_jid:node(Recipient), exmpp_jid:domain(Recipient), exmpp_jid:resource(Recipient)),

    lager:debug("Replace responder:~p", [ResponderReplaced]),
    J2 = jingle:replace_action(ResponderReplaced, <<"session-accept">>),
    J3 = exmpp_stanza:reply(J2),
    lager:debug("Session-accept:~p", [J3]),
    %%lager:debug("Session-accept XML:~n~p~n", [exmpp_xml:document_to_binary(J3)]),

    {ok, DestinationIp} = inet_parse:address(erlang:binary_to_list(exmpp_xml:get_attribute_as_binary(Candidate, <<"ip">>, undefined))),
    Port = utils:binary_to_integer(exmpp_xml:get_attribute_as_binary(Candidate, <<"port">>, undefined)),

    {ok, RtpSock} = gen_udp:open(Port, ?SOCKOPTS), %% OPEN A SOCKET TO TRANSMIT/LISTEN
    {ok, RtcpSock} = gen_udp:open(Port + 1, ?SOCKOPTS), %% OPEN A SOCKET TO TRANSMIT/LISTEN

    exmpp_session:send_packet(Session, J3),

    lager:debug("Session-accept sent:~p", [J3]),

    lager:info("Socket local for listen opened ip:~p port:~p", [?LISTEN_IP, Port]),
    lager:info("Addres to send ip:~p port:~p", [DestinationIp, Port]),

    SSRC = rtp_utils:generate_ssrc(),

    send(RtpSock, DestinationIp, Port, <<0>>), %%Send data to notify relay

    RTCP = #rtcp{payloads = [
        #rr{
            ssrc = SSRC,
            rblocks = [#rblock{
                ssrc = 0,
                fraction = 0,
                lost = 0,
                last_seq = 0,
                jitter = 0,
                lsr = 0,
                dlsr = 0
            }]
        },
        #sdes{list = [
            [{ssrc, SSRC},
                {cname, exmpp_jid:bare_to_list(Recipient)},
                {eof, true}]
        ]}
    ]},
    RTCPData = rtcp:encode(RTCP),

    send(RtcpSock, DestinationIp, Port + 1, RTCPData),

    erlang:send_after(2500, self(), send_sr), %% FIRST SR AT 2,5 seconds

    #state{
        sid = Sid,
        ssrc = SSRC,
        origin_jid = Recipient,
        destination_jid = Sender,
        rtp = RtpSock,
        rtcp = RtcpSock,
        destination_addr = {DestinationIp, Port},
        ts_offset = random:uniform(681), %% calculate random
        qos = #qos{}
    }.

session_terminate(Session, Packet, #state{rtp = RtpSock, rtcp = RtcpSock} = State) ->
    {IP, Port} = State#state.destination_addr,
    BYE = #bye{ssrc = [State#state.ssrc], message = "session terminate"},
    BYEData = rtcp:encode(BYE),
    send(RtcpSock, IP, Port, BYEData),
    lager:info("Closing rtp socket ~p", [RtpSock]),
    Crtp = gen_udp:close(RtpSock),
    lager:info("Closed ~p", [Crtp]),
    lager:info("Closing rtcp socket ~p", [RtcpSock]),
    Crtcp = gen_udp:close(RtcpSock),
    lager:info("Closed ~p", [Crtcp]),
    Result = exmpp_iq:result(Packet),
    exmpp_session:send_packet(Session, Result),
    send_stats_message(Session, State),
    ok.

send(Sock, Addr, Port, Data) ->
    case gen_udp:send(Sock, Addr, Port, Data) of
        ok ->
            lager:debug("Data sent to: ~p:~p ", [Addr, Port]),
            ok;
        Err ->
            lager:error("unable to send data: ~p", [Err]),
            exit({shutdown, Err})
    end.

send_stats_message(Session, State) ->
    {IP, Port} = State#state.destination_addr,
    Text = io_lib:format("Call stats~n"
    "~nHost: ~s"
    "~nPort: ~p"
    "~nTotal packets: ~p"
    "~nLost packets: ~p"
    "~nDropped packets: ~p"
    "~nJitter: ~.3f"
    "~nAvg. Round trip delay: ~.3f"
    "~nPacket loss: ~.3f"
    "~nMean Opinion Score: ~p", [
        inet_parse:ntoa(IP), Port,
        State#state.npackets,
        State#state.lost_packets,
        State#state.dropped_packets,
        State#state.qos#qos.jitter,
        State#state.qos#qos.round_trip_delay, %%TODO fix me
        State#state.qos#qos.packet_loss,
        State#state.qos#qos.mos
    ]),
    TmpEcho = exmpp_message:chat(Text),
    Echo = exmpp_stanza:set_jids(TmpEcho, State#state.origin_jid, State#state.destination_jid),
    lager:info("Echo message ~n~p~n", [Echo]),
    exmpp_session:send_packet(Session, Echo),
    ok.

loss_packets(LostPackets, ReceivedPacktes) ->
    Expected = ReceivedPacktes + LostPackets,
    LostPackets / Expected.

-spec jitter(Jitter :: float(),
    RecOld :: non_neg_integer(), RecNew :: non_neg_integer(),
    SentOld :: non_neg_integer(), SentNew :: non_neg_integer(), Frecuency :: float()) -> float().

jitter(JitterOld, RecOld, RecNew, SentOld, SentNew, Frecuency) ->
    D = abs((RecNew - RecOld) - (SentNew * Frecuency - SentOld * Frecuency)),
    JitterOld + (1 / 16 * (D - JitterOld)).

round_trip_delay(A, LSR, DLSR) ->
    A - LSR - DLSR.
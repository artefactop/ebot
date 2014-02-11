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


-define(TCP, 6).
-define(UDP, 17).
-define(IPVER4, 4).
-define(IPVER6, 6).

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
    low_throughput,
    dropped_pakets,
    errors,
    latency,
    jitter,
    out_of_order_delivery
}).

-type qos() :: #qos{}.

-record(state, {
    sid :: binary(),
    origin_jid :: exmpp_jid:jid(),
    destination_jid :: exmpp_jid:jid(),
    rtp :: inet:posix(),
    rtcp :: inet:posix(),
    destination_addr :: {inet:ip_address(), inet:port_number()},
    last_timestamp :: erlang:timestamp(),
    last_sequence_number :: non_neg_integer(),
    loss_packets :: non_neg_integer(),
    npackets :: non_neg_integer(),
    qos :: qos()
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

handle_info({udp, Sock, SrcIP, SrcPort, Data},
    #state{sid = Sid, rtp = Sock, npackets = NPackets,
        last_sequence_number = LSN,
        loss_packets = LossP} = State) ->
    lager:debug("RTP data received from: ~p:~p ", [SrcIP, SrcPort]),
    inet:setopts(Sock, [{active, once}]),
    lager:debug("destination addr: ~p ", [State#state.destination_addr]),
    case State#state.destination_addr of
        {DstIP, DstPort} ->
            send(State#state.rtp, DstIP, DstPort, Data);
        _ ->
            ok
    end,

    NewState =
        case rtp:decode(Data) of
            {ok, RTP} ->
                lager:debug("SID: ~p, PayloadType: ~p, SequenceNumber: ~p, Timestamp: ~p, SSRC: ~p",
                    [Sid, RTP#rtp.payload_type, RTP#rtp.sequence_number, RTP#rtp.timestamp, RTP#rtp.ssrc]),
                if
                    NPackets == 0 ->
                        SR = rtcp:encode_sr(RTP#rtp.ssrc, 0, 0, NPackets + 1, NPackets + 1, []), %%TODO fix
                        {IP, Port} = State#state.destination_addr,
                        send(State#state.rtcp, IP, Port + 1, SR);
                    true -> ok
                end,

                lager:debug("LSN: ~p", [LSN]),
                lager:debug("Sequence number: ~p", [RTP#rtp.sequence_number]),
                lager:debug("Loss: ~p", [LossP]),
                lager:debug("LOST: ~p", [RTP#rtp.sequence_number - (case NPackets of 0 -> RTP#rtp.sequence_number; _ ->
                    LSN + 1 end) + LossP]),

                State#state{
                    last_timestamp = now(),
                    npackets = NPackets + 1,
                    last_sequence_number = RTP#rtp.sequence_number,
                    loss_packets = RTP#rtp.sequence_number - (case NPackets of 0 -> RTP#rtp.sequence_number; _ ->
                        LSN + 1 end) + LossP
                };
            Error ->
                lager:debug("RTP decode error: ~p", [Error]),
                State#state{last_timestamp = now(), npackets = NPackets + 1}
        end,
    {noreply, NewState};

handle_info({udp, Sock, SrcIP, SrcPort, Data},
    #state{rtcp = Sock} = State) ->
    lager:debug("RTCP data received from: ~p:~p", [SrcIP, SrcPort]),
    inet:setopts(Sock, [{active, once}]),
    case rtcp:decode(Data) of
        {ok, RTCP} ->
            lager:debug("SID:~p, RTCP: ~p",
                [State#state.sid, RTCP]);
        Error ->
            lager:debug("RTCP decode error: ~p", [Error])
    end,
    %% TODO write stats
    {noreply, State};

handle_info(Request, State) ->
    lager:debug("Unknown request: ~p ~nState:~p ", [Request, State]),
    {noreply, State}.

terminate(Reason, State) ->
    lager:info("Terminating call process, reason ~p", [Reason]),
    lager:info("Call stats caller:~p, sid:~p, total_packets:~p, loss_packets:~p",
        [exmpp_jid:to_binary(State#state.origin_jid), State#state.sid, State#state.npackets + State#state.loss_packets, State#state.loss_packets]),
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

    send(RtpSock, DestinationIp, Port, <<>>),
    send(RtcpSock, DestinationIp, Port + 1, <<>>),

    #state{sid = Sid,
    origin_jid = Sender,
    destination_jid = Recipient,
    rtp = RtpSock,
    rtcp = RtcpSock,
    destination_addr = {DestinationIp, Port},
    loss_packets = 0,
    npackets = 0,
    qos = #qos{}
    }.


session_terminate(Session, Packet, #state{rtp = RtpSock, rtcp = RtcpSock}) ->
    lager:info("Closing rtp socket ~p", [RtpSock]),
    Crtp = gen_udp:close(RtpSock),
    lager:info("Closed ~p", [Crtp]),
    lager:info("Closing rtcp socket ~p", [RtcpSock]),
    Crtcp = gen_udp:close(RtcpSock),
    lager:info("Closed ~p", [Crtcp]),
    Result = exmpp_iq:result(Packet),
    exmpp_session:send_packet(Session, Result),
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


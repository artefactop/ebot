%%%-------------------------------------------------------------------
%%% @author pepe
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Feb 2014 1:18 PM
%%%-------------------------------------------------------------------
-module(handle_call).
-author("pepe").

-behaviour(gen_server).

-define(NS_JINGLE_1, 'urn:xmpp:jingle:1').

-define(LISTEN_IP, {0, 0, 0, 0}).
-define(SOCKOPTS, [binary, {active, once}]).

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

-record(state, {sid, destination_jid, local_sock,
    last_recv_remote,
    remote_sock,
    last_recv_local,
    lastTimestamp_local,
    lastTimestamp_remote,
    npackets,
    listen_ip, listen_port, sent_ip, sent_port}).

start_link(_Key) ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    {ok, #state{}}.

handle_call(Request, From, State) ->
    {reply, ok, State}.

handle_cast(Request, State) ->
    {noreply, State}.

handle_info({Session, Packet}, State) ->
    NewState =
        case jingle:get_action(Packet) of
            undefined -> State;
            'session-initiate' ->
                session_initiate(Session, Packet, State);
            'session-terminate' ->
                session_terminate(Session, Packet, State),
                %%TODO terminate gen server
                State;
            _ -> State
        end,
    {noreply, NewState};

handle_info({udp, Sock, SrcIP, SrcPort, Data},
    #state{local_sock = Sock, npackets = NPackets} = State) ->
    lager:debug("Data received from: ~p:~p ",[SrcIP, SrcPort]),
    inet:setopts(Sock, [{active, once}]),
    case State#state.last_recv_remote of
        {DstIP, DstPort} ->
            send(State#state.remote_sock, DstIP, DstPort, Data);
        _ ->
            ok
    end,
    {noreply, State#state{last_recv_local = {SrcIP, SrcPort}, lastTimestamp_local = now(), npackets = NPackets + 1}};

handle_info({udp, Sock, SrcIP, SrcPort, Data},
    #state{remote_sock = Sock, npackets = NPackets} = State) ->
    lager:debug("Data received from: ~p:~p ",[SrcIP, SrcPort]),
    inet:setopts(Sock, [{active, once}]),
    case State#state.last_recv_local of
        {DstIP, DstPort} ->
            send(State#state.local_sock, DstIP, DstPort, Data);
        _ ->
            ok
    end,
    {noreply, State#state{last_recv_remote = {SrcIP, SrcPort}, lastTimestamp_remote = now(), npackets = NPackets + 1}};

handle_info({Session, Packet}, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    erlang:error(not_implemented).

code_change(OldVsn, State, Extra) ->
    erlang:error(not_implemented).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
session_initiate(Session, Packet, State) ->
    %%TODO get candidate
    %%MAKE session-accept
    Result = exmpp_iq:result(Packet),
    exmpp_session:send_packet(Session, Result),

    Sid = jingle:get_sid(Packet),
    Jingle = exmpp_xml:get_element(Packet, jingle),
    lager:debug("Jingle:~p", [Jingle]),
    Content = jingle:get_content(Packet),
    lager:debug("Content:~p", [Content]),
    %%Description = exmpp_xml:get_element(Content, description),
    Transport = exmpp_xml:get_element(Content, transport),
    lager:debug("Transport:~p", [Transport]),

    %%[PreferredPayload | _] = exmpp_xml:get_elements(Description, 'payload-type'),
    [Candidate | _] = exmpp_xml:get_elements(Transport, candidate),
    lager:debug("Candidate:~p", [Candidate]),
    Recipient = exmpp_jid:parse(exmpp_stanza:get_recipient(Packet)),
    lager:debug("Recipient:~p", [Recipient]),
    %%Sender = exmpp_jid:parse(exmpp_stanza:get_sender(Packet)),

    ResponderReplaced = jingle:replace_responder(Packet, exmpp_jid:node(Recipient), exmpp_jid:domain(Recipient), exmpp_jid:resource(Recipient)),

    lager:debug("Replace responder:~p", [ResponderReplaced]),
    J2 = jingle:replace_action(ResponderReplaced, <<"session-accept">>),
    J3 = exmpp_stanza:reply(J2),
    lager:debug("Session-accept:~p", [J3]),
    lager:debug("Session-accept XML:~n~p~n", [exmpp_xml:document_to_binary(J3)]),

    {ok, SentIp} = inet_parse:address(exmpp_xml:get_attribute_as_binary(Candidate, <<"ip">>, undefined)),

    Port = erlang:binary_to_integer(exmpp_xml:get_attribute_as_binary(Candidate, <<"port">>, undefined)),

    IQ = exmpp_iq:set(?NS_JINGLE_1, J3),
    exmpp_session:send_packet(Session, IQ),
    #state{sid = Sid, destination_jid = Recipient,
    local_sock = gen_udp:open(Port + 1, ?SOCKOPTS),
         remote_sock = gen_udp:open(Port, ?SOCKOPTS),
    listen_ip = ?LISTEN_IP, listen_port = Port,
    sent_ip = SentIp, sent_port = Port}.


session_terminate(Session, Packet, #state{listen_port = Port} = State) ->
    gen_udp:close(Port),
    Result = exmpp_iq:result(Packet),
    exmpp_session:send_packet(Session, Result),
    ok.


send(Sock, Addr, Port, Data) ->
    case gen_udp:send(Sock, Addr, Port, Data) of
        ok ->
            lager:debug("Data sent to: ~p:~p ",[Addr, Port]),
            ok;
        Err ->
            lager:error("unable to send data: ~p", [Err]),
            exit(normal)
    end.



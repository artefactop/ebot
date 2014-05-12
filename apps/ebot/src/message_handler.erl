-module(message_handler).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([process_message/2]).

process_message(#received_packet{from = From, raw_packet = Packet}, Session) ->
    echo_packet(Session, From, Packet).

%% Send the same packet back for each message received
echo_packet(Session, _From, Packet) ->
    lager:info("Received message ~n~p~n", [Packet]),
    case exmpp_xml:has_element(Packet, 'body') of
        true ->
            To = exmpp_stanza:get_sender(Packet),
            From_Binary = exmpp_stanza:get_recipient(Packet),
            FromJidTmp = exmpp_jid:parse(From_Binary),
            ToJidTmp = exmpp_jid:parse(To),
            lager:info("ToJidTmp ~n~p~n", [ToJidTmp]),
            ToJid = exmpp_jid:make(exmpp_jid:node(ToJidTmp), exmpp_jid:domain(ToJidTmp)),

            lager:info("ToJid ~n~p~n", [ToJid]),
            FromJid = exmpp_jid:make(exmpp_jid:node(FromJidTmp), exmpp_jid:domain(FromJidTmp)),
            handle_message_receipts(Session, FromJid, ToJid, Packet),
            lager:info("From ~n~p~n", [FromJid]),
            TmpEcho = exmpp_message:chat(exmpp_message:get_body(Packet)),
            Echo = exmpp_stanza:set_jids(TmpEcho, FromJid, ToJid),
            lager:info("Echo message ~n~p~n", [Echo]),
            GCMyself = exmpp_jid:resource(ToJidTmp) == exmpp_jid:node(FromJidTmp),
            case exmpp_stanza:get_type(Packet) of
                <<"groupchat">> when GCMyself == true ->
                    lager:debug("GroupChat message from myself:~p", [Packet]),
                    ok;
                <<"groupchat">> ->
                    exmpp_session:send_packet(Session, exmpp_message:set_type(Echo, <<"groupchat">>));
                _ ->
                    exmpp_session:send_packet(Session, Echo)
            end;
        _ -> ok
    end.

handle_message_receipts(MySession, From, To, Packet) ->
    case exmpp_xml:get_element_by_ns(Packet, ?NS_RECEIPTS) of
        undefined ->
            ok;
        {xmlel, _, _, request, _, _} ->
            Id = exmpp_xml:get_attribute(Packet, <<"id">>, <<"unknown">>),
            Rcv = exmpp_xml:element(?NS_RECEIPTS, read),
            Received = exmpp_xml:set_attribute(Rcv, <<"id">>, Id),

            MessageTmp = exmpp_message:chat(),
            MessageTmp1 = exmpp_xml:append_child(MessageTmp, Received),

            TmpPacket = exmpp_stanza:set_sender(MessageTmp1, From),
            Message = exmpp_stanza:set_recipient(TmpPacket, To),

            lager:info("Response receipt ~n~p~n", [Message]),
            exmpp_session:send_packet(MySession, Message);
        _ ->
            ok
    end.
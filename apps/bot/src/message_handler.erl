-module(message_handler).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([process_message/2]).

process_message(#received_packet{from=From, raw_packet=Packet}, Session) ->
    echo_packet(Session, From, Packet).

%% Send the same packet back for each message received
echo_packet(Session, From, Packet) ->
    case exmpp_xml:has_element(Packet, 'body') of
        true ->
            To = exmpp_stanza:get_sender(Packet),
            ToJidTmp = exmpp_jid:parse(To),
            ToJid = exmpp_jid:make(exmpp_jid:node(ToJidTmp), exmpp_jid:domain(ToJidTmp)),
            handle_message_receipts(Session, From, ToJid, Packet),

            TmpPacket = exmpp_stanza:set_sender(Packet, From),
            TmpPacket2 = exmpp_stanza:set_recipient(TmpPacket, ToJid),
            NewPacket = exmpp_xml:remove_attribute(TmpPacket2, <<"id">>),
            io:format("Echo message ~n~p~n", [NewPacket]),
            exmpp_session:send_packet(Session, NewPacket);
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
            
            io:format("Response receipt ~n~p~n", [Message]),
            exmpp_session:send_packet(MySession, Message);
        _ ->
            ok
    end.
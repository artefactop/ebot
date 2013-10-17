-module(presence_handler).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([process_presence/2]).

process_presence(#received_packet{}=Rcv_Packet, Session) ->
    handle_presence(Session, Rcv_Packet).

handle_presence(Session, Packet) ->
    case exmpp_jid:make(_From = Packet#received_packet.from) of
    JID ->
        case _Type = Packet#received_packet.type_attr of
        "available" ->
            %% handle presence available
            ok;
        "unavailable" ->
            %% handle presence unavailable
            ok;
        "subscribe" ->
            presence_subscribed(Session, JID),
            presence_subscribe(Session, JID);
        "subscribed" ->
            presence_subscribed(Session, JID),
            presence_subscribe(Session, JID);
        _ -> 
            ok
        end
    end.

presence_subscribed(Session, Recipient) ->
    Presence_Subscribed = exmpp_presence:subscribed(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribed, Recipient),
    exmpp_session:send_packet(Session, Presence).

presence_subscribe(Session, Recipient) ->
    Presence_Subscribe = exmpp_presence:subscribe(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribe, Recipient),
    exmpp_session:send_packet(Session, Presence).
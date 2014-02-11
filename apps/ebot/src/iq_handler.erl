-module(iq_handler).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-define(NS_JINGLE_1, 'urn:xmpp:jingle:1').

-export([process_iq/2]).

process_iq(#received_packet{queryns = ?NS_JINGLE_1, raw_packet = Packet}, Session) ->
    handle_jingle(Session, Packet);
process_iq(#received_packet{type_attr = Type, queryns = ?NS_PING, raw_packet = Packet}, Session) ->
    handle_ping(Session, Type, Packet);
process_iq(#received_packet{raw_packet = Packet, queryns = NS}, _Session) ->
    lager:debug("Namespace ~p", [NS]),
    lager:info("RECEIVED:~n~p~n", [Packet]),
    ebot:syslog(info, io_lib:format("RECEIVED: ~p", [exmpp_xml:document_to_binary(Packet)])),
    ok.

handle_ping(Session, "get", Packet) ->
    Result = exmpp_iq:result(Packet),
    exmpp_session:send_packet(Session, Result);
handle_ping(_Session, "result", Packet) ->
    lager:debug("Received result ping IQ:~n~p~n", [Packet]),
    Id = exmpp_xml:get_attribute(Packet, <<"id">>, <<"unknown">>),
    case timem:remove(Id) of
        {_, {Service, Tm}} ->
            ebot:syslog(notice, io_lib:format("Service ~p responding in ~p milliseconds", [
                erlang:binary_to_list(Service),
                diff_now_ms(Tm)])),
            ok;
        _ ->
            ebot:syslog(warning, io_lib:format("Unknown :~p", [Packet])),
            ok
    end;
handle_ping(_Session, "error", Packet) ->
    lager:debug("Received error ping IQ:~n~p~n", [Packet]),
    Id = exmpp_xml:get_attribute(Packet, <<"id">>, <<"unknown">>),
    Condition = exmpp_stanza:get_condition(Packet),
    case timem:remove(Id) of
        {_, {Service, Tm}} ->
            ebot:syslog(emerg, io_lib:format("Service ~p responding error ~p in ~p milliseconds", [
                erlang:binary_to_list(Service),
                erlang:atom_to_list(Condition),
                diff_now_ms(Tm)])),
            ok;
        _ ->
            ebot:syslog(warning, io_lib:format("Unknown :~p", [Packet])),
            ok
    end.

handle_jingle(Session, Packet) ->
    Sid = jingle:get_sid(Packet),
    {_Node,PID} = forseti:get_key(Sid),
       PID ! {Session, Packet},
    ok.

diff_now_ms(T) ->
    (timem:tm(os:timestamp()) - timem:tm(T)) / 1000.
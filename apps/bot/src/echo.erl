%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Mickael Remond <mickael.remond@process-one.net>

%% @doc
%% The module <strong>{@module}</strong> implements a simple XMPP echo client.
%%
%% <p>
%% This is a example use of the exmpp framework.
%% </p>
%%
%% <p>
%% Usage:
%% </p>
%% <pre>{ok, session} = echo_client:start().
%% echo_client:stop(Session).</pre>

-module(echo).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([start/3, start/4, start/5, stop/1]).
-export([init/6]).

%% State
-record(state, {session, jid}).

start(User, Domain, Password) ->
    start(User, Domain, Domain, Password).

start(User, Domain, Server, Password) ->
    start(User, Domain, random, Server, Password).

start(User, Domain, Resource, Server, Password) ->
    start(User, Domain, Resource, Server, 5222, Password).

start(User, Domain, Resource, Server, Port, Password) ->
    spawn(?MODULE, init, [User, Domain, Resource, Server, Port, Password]).

stop(EchoClientPid) ->
    EchoClientPid ! stop.

init(User, Domain, Resource, Server, Port, Password) ->
    ping_timer:init(),
    init_syslog(local7, "ping_bot@"++Server),
    %% Start XMPP session: Needed to start service (Like
    %% exmpp_stringprep):
    MySession = exmpp_session:start(),
    MyJID = exmpp_jid:make(User, Domain, Resource),
    %% Create a new session with basic (digest) authentication:
    exmpp_session:auth_basic_digest(MySession, MyJID, Password),
    %% Connect in standard TCP:
    {ok, _StreamId} = exmpp_session:connect_TCP(MySession, Server, Port),
    session(MySession, MyJID, Password).

-spec init_syslog(Facility::(atom() | integer()), Name::string()) -> ok | {error, Reason::any()}.

init_syslog(Facility, Name) ->
    lager:info("Syslog configured: facility=~p, name=~p",[Facility, Name]),
    syslog:open(Name, [cons, perror, pid], Facility).

%% We are connected. We now log in (and try registering if authentication fails)
session(MySession, MyJID, Password) ->
    %% Login with defined JID / Authentication:
    try exmpp_session:login(MySession)
    catch
    throw:{auth_error, 'not-authorized'} ->
        %% Try creating a new user:
        io:format("Register~n",[]),
        %% In a real life client, we should trap error case here
        %% and print the correct message.
        exmpp_session:register_account(MySession, Password),
        %% After registration, retry to login:
        exmpp_session:login(MySession)
    end,
    %% We explicitely send presence:
    exmpp_session:send_packet(MySession,
                  exmpp_presence:set_status(
                exmpp_presence:available(), "Echo Ready")),
    loop(#state{session=MySession, jid=MyJID}).

%% Process exmpp packet:
loop(#state{session=MySession, jid=MyJID}) ->
    receive
        stop ->
            exmpp_session:stop(MySession);
        %% If we receive a message, we reply with the same message
        Record = #received_packet{packet_type=message,
                  raw_packet=Packet,
                  type_attr=Type} when Type =/= "error" ->
            io:format("Received Message stanza:~n~p~n~n", [Record]),
            echo_packet(MySession, MyJID, Packet),
            loop(#state{session=MySession, jid=MyJID});
        %% If we receive a presence stanza, handle it
        Record when Record#received_packet.packet_type == 'presence' ->
            io:format("Received Presence stanza:~n~p~n~n", [Record]),
            handle_presence(MySession, Record, Record#received_packet.raw_packet),
            loop(#state{session=MySession, jid=MyJID});
        %% If we receive a iq stanza, handle it
        Record = #received_packet{packet_type=iq,
                  raw_packet=Packet,
                  type_attr=Type} ->
            io:format("Received IQ stanza:~n~p~n~n", [Record]),
            handle_ping(MySession, Type, Packet),
            loop(#state{session=MySession, jid=MyJID});
        Record ->
            io:format("Received a stanza:~n~p~n~n", [Record]),
            loop(#state{session=MySession, jid=MyJID})
    after
        10000 ->
            Exp_Time = 10000,
            io:format("Sending ping ~n~n", []),
            List = [<<"pushservice.ym.ms">>, <<"userservice.ym.ms">>,
                <<"energy.ym.ms">>,<<"sms.ym.ms">>,<<"relay.ym.ms">>,
                <<"sip.ym.ms">>,<<"siprelay.ym.ms">>,<<"vgoods.ym.ms">>,
                <<"achievement.ym.ms">>,<<"sms-route-selector.ym.ms">>,
                <<"sipoutctr.ym.ms">>,<<"sipoute.ym.ms">>,<<"sipine.ym.ms">>,
                <<"pushservice.ym.ms">>, <<"ym.ms">>], %%TODO: hardcoded
            Expired = ping_timer:remove_expired(Exp_Time), 
            lists:foreach(fun({_, {Service, _}}) -> 
                syslog(emerg, io_lib:format("Service ~p NOT responding during ~p milliseconds", [erlang:binary_to_list(Service), Exp_Time]))
             end, Expired),
            lists:foreach(fun(X) -> send_ping(MySession, MyJID, X) end, List),
            loop(#state{session=MySession, jid=MyJID})
    end.

%% Send the same packet back for each message received
echo_packet(MySession, From, Packet) ->
    case exmpp_xml:has_element(Packet, 'body') of
        true ->
            To = exmpp_stanza:get_sender(Packet),
            ToJidTmp = exmpp_jid:parse(To),
            ToJid = exmpp_jid:make(exmpp_jid:node(ToJidTmp), exmpp_jid:domain(ToJidTmp)),
            handle_message_receipts(MySession, From, ToJid, Packet),

            TmpPacket = exmpp_stanza:set_sender(Packet, From),
            TmpPacket2 = exmpp_stanza:set_recipient(TmpPacket, ToJid),
            NewPacket = exmpp_xml:remove_attribute(TmpPacket2, <<"id">>),
            io:format("Echo message ~n~p~n", [NewPacket]),
            exmpp_session:send_packet(MySession, NewPacket);
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

handle_presence(Session, Packet, _Presence) ->
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

handle_ping(Session, "get", Packet) ->
    Result = exmpp_iq:result(Packet),
    exmpp_session:send_packet(Session, Result);
handle_ping(_Session, "result", Packet) ->
    io:format("Received result ping IQ:~n~p~n~n", [Packet]),
    Id = exmpp_xml:get_attribute(Packet, <<"id">>, <<"unknown">>),
    case ping_timer:remove(Id) of 
        {_, {Service, Tm}} ->
            syslog(notice, io_lib:format("Service ~p responding in ~p milliseconds", [erlang:binary_to_list(Service), diff_now_ms(Tm)]));
        _ ->
            syslog(warning, io_lib:format("Unknown :~p", [Packet]))
    end;
handle_ping(_Session, "error", Packet) ->
    io:format("Received error ping IQ:~n~p~n~n", [Packet]),
    Id = exmpp_xml:get_attribute(Packet, <<"id">>, <<"unknown">>),
    Condition = exmpp_stanza:get_condition(Packet),
    case ping_timer:remove(Id) of 
        {_, {Service, Tm}} ->
            syslog(emerg, io_lib:format("Service ~p responding error ~p in ~p milliseconds", [erlang:binary_to_list(Service), erlang:atom_to_list(Condition), diff_now_ms(Tm)]));
        _ ->
            syslog(warning, io_lib:format("Unknown :~p", [Packet]))
    end.

diff_now_ms(T) ->
    (ping_timer:tm(os:timestamp()) - ping_timer:tm(T))/1000.

send_ping(Session, From, To) ->
    Ping = exmpp_xml:element(?NS_PING, 'ping'), 
    Id = gen_id(),
    Stanza = exmpp_iq:get(?NS_COMPONENT_ACCEPT, Ping, Id),
    Packet = exmpp_stanza:set_jids(Stanza, From, To),
    R = ping_timer:insert(Id, {To, os:timestamp()}),
    io:format("ping IQ:~n~p~n~p~n~n", [Packet, R]),
    exmpp_session:send_packet(Session, Packet).

-spec gen_id() -> binary().

gen_id() ->
    list_to_binary(uuid:to_string(uuid:uuid4())).

-type levels() :: emerg | alert | crit | err | warning | notice | info | debug.

-spec syslog(Level::levels(), Message::string()) -> ok.

%% Level: emerg, alert, crit, err, warning, notice, info, debug
syslog(Level, Message) when is_binary(Message) ->
    syslog(Level, erlang:binary_to_list(Message));
syslog(Level, Message) when is_list(Message) ->
    Priority = case Level of
        emerg -> "EMERG ";
        alert -> "ALERT ";
        crit -> "CRIT ";
        err -> "ERR ";
        warning -> "WARNING ";
        notice -> "NOTICE ";
        info -> "INFO ";
        debug -> "DEBUG ";
        _ -> ""
    end,
    syslog:log(Level, Priority ++ Message).


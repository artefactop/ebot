-module(ebot).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-record(state, {
    ref :: reference(),
    session :: pid(),
    jid :: exmpp_jid:jid(),
    pass :: string(),
    server :: string(),
    port :: non_neg_integer(),
    services :: list(),
    timeout = 10000 :: timeout(),
    timer :: reference()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, send_ping/1, syslog/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} | {error, Reason :: any()}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init(Args :: []) ->
    {ok, State :: #state{}} |
    {ok, State :: #state{}, hibernate | infinity | non_neg_integer()} |
    ignore | {stop, Reason :: string()}.

init([]) ->
    lager:info("Loading Application ebot", []),
    {ok, User} = application:get_env(user),
    {ok, Domain} = application:get_env(domain),
    {ok, Resource} = application:get_env(resource),
    {ok, Password} = application:get_env(password),
    {ok, Server} = application:get_env(server),
    {ok, Port} = application:get_env(port),
    {ok, RequestPing} = application:get_env(request_ping),
    {ok, Services} = application:get_env(services),
    {ok, Timeout} = application:get_env(timeout),

    timem:init(),
    init_syslog(local7, "ebot@" ++ Server),

    Jid = exmpp_jid:make(User, Domain, Resource),

    {_, Session} = make_connection(Jid, Password, Server, Port),
    Ref = erlang:monitor(process, Session),
    Timer =
        case RequestPing of
            false -> undefined;
            true -> erlang:send_after(Timeout, self(), trigger)
        end,

    {ok, #state{
        ref = Ref,
        session = Session,
        jid = Jid,
        pass = Password,
        server = Server,
        port = Port,
        services = Services,
        timeout = Timeout,
        timer = Timer}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(trigger, #state{timeout = Timeout} = State) ->
    % Do the action
    spawn(?MODULE, send_ping, [State]),
    % Start new timer
    erlang:send_after(Timeout, self(), trigger),
    {noreply, State};

handle_info(#received_packet{packet_type = iq} = Rcv_Packet, #state{session = Session} = State) ->
    spawn(iq_handler, process_iq, [Rcv_Packet, Session]),
    {noreply, State};

handle_info(#received_packet{packet_type = presence} = Rcv_Packet, #state{session = Session} = State) ->
    spawn(presence_handler, process_presence, [Rcv_Packet, Session]),
    {noreply, State};

handle_info(#received_packet{packet_type = message} = Rcv_Packet, #state{session = Session} = State) ->
    spawn(message_handler, process_message, [Rcv_Packet, Session]),
    {noreply, State};

handle_info({'DOWN', _Ref, process, _Pid2, _Reason}, State) ->
    {noreply, try_reconnect(State)};

handle_info(stop, #state{ref = Ref, session = Session, timer = Timer} = State) ->
    lager:info("Component Stopped.~n", []),
    exmpp_session:stop(Session),
    erlang:cancel_timer(Timer),
    erlang:demonitor(Ref),
    {stop, normal, State};

handle_info(Info, State) ->
    lager:info("Unknown Info Request: ~p~n", [Info]), %%TODO {stream_error,'system-shutdown'}
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec try_reconnect(State :: record()) -> record().

try_reconnect(#state{ref = Ref, jid = Jid, server = Server, pass = Pass, port = Port, timeout = Timeout, timer = Timer} = State) ->
    lager:info("Connection Closed. Trying to Reconnect...~n", []),
    erlang:demonitor(Ref),
    NewTimer =
        case Timer of
            undefined -> undefined;
            _ ->
                erlang:cancel_timer(Timer),
                erlang:send_after(Timeout, self(), trigger)
        end,
    {_, Session} = make_connection(Jid, Pass, Server, Port),
    NewRef = erlang:monitor(process, Session),
    lager:info("Session ~p", [Session]),
    lager:info("Reconnected.~n", []),
    State#state{ref = NewRef, session = Session, timer = NewTimer}.

-spec make_connection(Jid :: exmpp_jid:jid(), Password :: string(), Server :: string(), Port :: integer()) -> {R :: string(), Session :: pid()}.

make_connection(Jid, Password, Server, Port) ->
    Session = exmpp_session:start(),
    make_connection(Session, Jid, Password, Server, Port, 20).

-spec make_connection(Session :: pid(), JID :: exmpp_jid:jid(), Password :: string(), Server :: string(), Port :: integer(), Tries :: integer()) -> {string(), pid()}.

make_connection(Session, JID, Password, Server, Port, 0) ->
    exmpp_session:stop(Session),
    make_connection(JID, Password, Server, Port);
make_connection(Session, Jid, Password, Server, Port, Tries) ->
    lager:info("Connecting: ~p Tries Left~n", [Tries]),
    %% Create a new session with basic (digest) authentication:
    exmpp_session:auth_basic_digest(Session, Jid, Password),
    %% Connect in standard TCP:
    try exmpp_session:connect_TCP(Session, Server, Port) of
        R ->
            lager:info("Connected.~n", []),
            %% We are connected. We now log in
            try exmpp_session:login(Session)
            catch
                throw:{auth_error, 'not-authorized'} ->
                    syslog(emerg, "Error when login to server, not-authorized")
            end,
            %% We explicitely send presence:
            exmpp_session:send_packet(Session,
                exmpp_presence:set_status(
                    exmpp_presence:available(), "Echo Ready")),
            {R, Session}
    catch
        Exception ->
            syslog(emerg, io_lib:format("Can not connect to server, Tries Left: ~p, Exception: ~p~n", [Tries, Exception])),
            lager:warning("Exception: ~p~n", [Exception]),
            timer:sleep((20 - Tries) * 200),
            make_connection(Session, Jid, Password, Server, Port, Tries - 1)
    end.

send_ping(#state{session = Session, jid = Jid, timeout = Timeout, services = Services}) ->
    lager:debug("Sending ping"),
    Expired = timem:remove_expired(Timeout),
    lager:debug("Expired ids: ~p~n", [Expired]),
    lists:foreach(fun({_, {Service, _}}) ->
        lager:info("Service ~s NOT responding during ~p milliseconds", [erlang:binary_to_list(Service), Timeout]),
        syslog(emerg, io_lib:format("Service ~s NOT responding during ~p milliseconds", [erlang:binary_to_list(Service), Timeout]))
    end, Expired),
    case erlang:length(Services) of
        0 -> ok;
        _ ->
            Sleep = Timeout div length(Services),
            lager:debug("Session: ~p~n", [Session]),
            lager:debug("Services list: ~p~n", [Services]),
            lists:foreach(fun(X) ->
                send_ping(Session, Jid, X),
                timer:sleep(Sleep)
            end, Services)
    end.

send_ping(Session, From, To) ->
    Ping = exmpp_xml:element(?NS_PING, 'ping'),
    Id = gen_id(),
    Stanza = exmpp_iq:get(?NS_COMPONENT_ACCEPT, Ping, Id),
    Packet = exmpp_stanza:set_jids(Stanza, From, To),
    R = timem:insert(Id, {To, os:timestamp()}),
    lager:debug("ping IQ:~n~p~n~p~n~n", [Packet, R]),
    exmpp_session:send_packet(Session, Packet).

%% Utils

-spec init_syslog(Facility :: (atom() | integer()), Name :: string()) -> ok | {error, Reason :: any()}.

init_syslog(Facility, Name) ->
    lager:info("Syslog configured: facility=~p, name=~p", [Facility, Name]),
    syslog:open(Name, [cons, perror, pid], Facility).

-spec gen_id() -> binary().

gen_id() ->
    list_to_binary(uuid:to_string(uuid:uuid4())).

-type levels() :: emerg | alert | crit | err | warning | notice | info | debug.

-spec syslog(Level :: levels(), Message :: string()) -> ok.

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
    syslog:log(Level, Priority ++ Message),
    ok.
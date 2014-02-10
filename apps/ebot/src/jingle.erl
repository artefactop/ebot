%%%-------------------------------------------------------------------
%%% @author pepe
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Feb 2014 5:33 PM
%%%-------------------------------------------------------------------
-module(jingle).
-author("pepe").

-include_lib("exmpp/include/exmpp.hrl").

%% API
-export([
    redirect/2,
    replace_responder/4,
    replace_action/2,
    get_initiator/1,
    get_responder/1,
    get_sid/1,
    get_action/1,
    get_content/1,
    get_reason/1
]).

-type action() ::
'content-accept' |
'content-add' |
'content-modify' |
'content-reject' |
'content-remove' |
'description-info' |
'security-info' |
'session-accept' |
'session-info' |
'session-initiate' |
'session-terminate' |
'transport-accept' |
'transport-info' |
'transport-reject' |
'transport-replace'.

-type sid() :: binary().

-type jingle() :: exmpp_iq:iq().

-define(Redirect(Xmpp),
    (
        exmpp_xml:element(undefined, error,
            [exmpp_xml:attribute(<<"type">>, <<"modify">>)],
            [exmpp_xml:element('urn:ietf:params:xml:ns:xmpp-stanzas', redirect, [], [{xmlcdata, <<"xmpp:", Xmpp/binary>>}]),
                exmpp_xml:element('urn:ietf:params:xml:ns:xmpp-stanzas', text, [exmpp_xml:attribute(<<"lang">>, <<"en">>)], [{xmlcdata, <<"xmpp:", Xmpp/binary>>}])] %% CORE-1727
        )
    )).

-spec redirect(JingleIQ :: jingle(), Xmpp :: binary()) -> exmpp_iq:iq().

redirect(JingleIQ, Xmpp) when is_list(Xmpp) ->
    redirect(JingleIQ, list_to_binary(Xmpp));
redirect(JingleIQ, Xmpp) when is_binary(Xmpp) ->
    CleanedIQ = exmpp_xml:remove_element(JingleIQ, jingle),
    exmpp_iq:error(CleanedIQ, ?Redirect(Xmpp)).

-spec replace_responder(Xmlel :: exmpp_xml:xmlel(), Node :: binary(), XmppDomain :: binary(), Resource :: binary()) -> exmpp_xml:xmlel().

replace_responder(Xmlel, Node, XmppDomain, Resource) ->
    case exmpp_xml:get_element(Xmlel, jingle) of
        undefined ->
            Xmlel;
        Jingle ->
            NewJingle = exmpp_xml:set_attribute(Jingle, <<"responder">>, exmpp_jid:to_binary(Node, XmppDomain, Resource)),
            exmpp_xml:replace_child(Xmlel, Jingle, NewJingle)
    end.

-spec replace_action(Xmlel :: exmpp_xml:xmlel(), binary()) -> exmpp_xml:xmlel().

replace_action(Xmlel, Action) ->
    case exmpp_xml:get_element(Xmlel, jingle) of
        undefined ->
            Xmlel;
        Jingle ->
            NewJingle = exmpp_xml:set_attribute(Jingle, <<"action">>, Action),
            exmpp_xml:replace_child(Xmlel, Jingle, NewJingle)
    end.


-spec get_responder(JingleIQ :: jingle()) -> exmpp_jid:jid() | undefined.

get_responder(JingleIQ) ->
    case exmpp_xml:get_element(JingleIQ, jingle) of
        undefined ->
            undefined;
        Jingle ->
            case exmpp_xml:get_attribute_as_binary(Jingle, <<"responder">>, undefined) of
                undefined ->
                    undefined;
                Responder ->
                    exmpp_jid:parse(Responder)
            end
    end.

-spec get_initiator(JingleIQ :: jingle()) -> exmpp_jid:jid() | undefined.

get_initiator(JingleIQ) ->
    case exmpp_xml:get_element(JingleIQ, jingle) of
        undefined ->
            undefined;
        Jingle ->
            case exmpp_xml:get_attribute_as_binary(Jingle, <<"initiator">>, undefined) of
                undefined ->
                    undefined;
                Initiator ->
                    exmpp_jid:parse(Initiator)
            end
    end.

-spec get_sid(JingleIQ :: jingle()) -> jingle:sid() | undefined.

get_sid(JingleIQ) ->
    case exmpp_xml:get_element(JingleIQ, jingle) of
        undefined -> undefined;
        Jingle ->
            case exmpp_xml:get_attribute_as_binary(Jingle, <<"sid">>, undefined) of
                undefined -> undefined;
                Sid -> Sid
            end
    end.

-spec get_action(JingleIQ :: jingle()) -> action() | undefined.

get_action(JingleIQ) ->
    case exmpp_xml:get_element(JingleIQ, jingle) of
        undefined ->
            undefined;
        Jingle ->
            erlang:binary_to_atom(
                exmpp_xml:get_attribute_as_binary(Jingle, <<"action">>, <<"undefined">>),
                utf8)
    end.

-spec get_content(JingleIQ :: jingle()) -> exmpp:exmpp_xml() | undefined.

get_content(JingleIQ) ->
    case exmpp_xml:get_element(JingleIQ, jingle) of
        undefined ->
            undefined;
        Jingle ->
            exmpp_xml:get_element(Jingle, content)
    end.

-spec get_reason(JingleIQ :: jingle()) -> exmpp:exmpp_xml() | undefined.

get_reason(JingleIQ) ->
    case exmpp_xml:get_element(JingleIQ, jingle) of
        undefined ->
            undefined;
        Jingle ->
            exmpp_xml:get_element(Jingle, reason)
    end.
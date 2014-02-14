%%%-------------------------------------------------------------------
%%% @author pepe
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Feb 2014 12:22 PM
%%%-------------------------------------------------------------------
-module(rtp_utils).
-author("pepe").

-define(MAX_SSRC, 4294967295).
-define(YEARS_70, 2208988800).

-include("../../include/rtplib/rtcp.hrl").

%% API
-export([
    generate_ssrc/0,
    ntp_timestamp/0,
    get_rtcp_report/1
]).

-spec generate_ssrc() -> non_neg_integer().

generate_ssrc() ->
    {A, B, C} = os:timestamp(),
    random:seed(A, B, C),
    random:uniform(?MAX_SSRC).

-spec ntp_timestamp() -> binary().

ntp_timestamp() ->
    {Mega, Sec, Micro} = erlang:now(),
    MSW = (Mega * 1000000 + Sec + ?YEARS_70) band 16#FFFFFFFF,
    LSW = Micro * 1000,
    <<NTP:64>> =  <<MSW:32, LSW:32>>,
    NTP.

get_rtcp_report(#rtcp{payloads = Payloads}) ->
    get_report_from_payloads(Payloads).

get_report_from_payloads([]) ->
    undefined;
get_report_from_payloads([H | T]) when is_tuple(H) ->
    case H of
        {sr, _, _, _, _, _, _} = SR -> SR;
        {rr, _, _, _} = SR -> SR;
        _ -> get_report_from_payloads(T)
    end.



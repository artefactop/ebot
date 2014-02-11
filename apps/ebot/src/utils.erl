%%%-------------------------------------------------------------------
%%% @author pepe
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Feb 2014 2:00 PM
%%%-------------------------------------------------------------------
-module(utils).
-author("pepe").

%% API
-export([
    binary_to_integer/1
]).


-spec binary_to_integer(Bin :: binary()) -> integer()| {error, term()}.

binary_to_integer(Bin) when is_binary(Bin) ->
    erlang:list_to_integer(erlang:binary_to_list(Bin)).
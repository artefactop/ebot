-module(bot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-spec start(
        StartType :: application:start_type(), 
        StartArgs :: application:start_args() 
    ) ->
        {ok, Pid::pid()} | {error, Reason::any()}.

start(_StartType, _StartArgs) ->
    bot_sup:start_link().

-spec stop( State::any() ) -> ok.

stop(_State) ->
    ok.
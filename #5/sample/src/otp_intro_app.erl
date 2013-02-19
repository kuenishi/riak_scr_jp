-module(otp_intro_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    otp_intro_sup:start_link().

stop(_State) ->
    ok.

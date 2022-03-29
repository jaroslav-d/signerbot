%%%-------------------------------------------------------------------
%% @doc signerbot public API
%% @end
%%%-------------------------------------------------------------------

-module(signerbot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    signerbot_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

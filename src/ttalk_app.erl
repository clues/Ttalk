-module(ttalk_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% -ifdef(TEST).
%% -include_lib("eunit/include/eunit.hrl).
%% 
%% -endif.

start(_StartType, _StartArgs) ->
    ttalk_sup:start_link().

stop(_State) ->
    ok.

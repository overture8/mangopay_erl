%%%-------------------------------------------------------------------
%% @doc mangopay public API
%% @end
%%%-------------------------------------------------------------------

-module(mangopay_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  mangopay_sup:start_link(),
  mangopay_api_sup:start_link().

stop(_State) ->
  ok.

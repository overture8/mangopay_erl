-module(mangopay).

-export([
  auth/2,
  list_users/0
]).

%% mangopay library's entry point.

auth(ClientId, ClientPassword) ->
  mangopay_api:auth(ClientId, ClientPassword).

list_users() ->
  mangopay_api:list_users().

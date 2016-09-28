%%%-------------------------------------------------------------------
%%% @author Phil McClure
%%% @copyright (C) MIT license
%%% @doc
%%%
%%% @end
%%% Created : 2016-09-25 22:26:49.532436
%%%-------------------------------------------------------------------
-module(mangopay_api).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([
  auth/2,
  list_users/0
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {auth_token    :: string(), 
                client_id     :: string()}).

%%%===================================================================
%%% API
%%%===================================================================

auth(ClientId, ClientPassword) ->
  gen_server:call(?MODULE, {auth, ClientId, ClientPassword}).

list_users() ->
  gen_server:call(?MODULE, {list_users}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_call({list_users}, _From, State) ->
  AuthToken = State#state.auth_token,
  ClientId = State#state.client_id,
  Path = "/" ++ ClientId ++ "/users/",
	Headers = [{"Authorization", "Bearer " ++ binary:bin_to_list(AuthToken)}],
  Response = request(get, Path, Headers),
  case Response of
    {ok, Payload} ->
      {{_, StatusCode, _}, _, BodyData} = Payload,

      case StatusCode of
        200 ->
          ParsedJson = jsx:decode(list_to_binary(BodyData)),
          {reply, ParsedJson, State};
        Other when Other > 200 -> 
          {reply, Payload, State}
      end;
    {error, Error} -> 
      {reply, Error, State}
  end;

handle_call({auth, ClientId, ClientPassword}, _From, State) ->
  Response = authenticate(ClientId, ClientPassword),
	case Response of
    {ok, Payload} ->
      {{_, StatusCode, _}, _, BodyData} = Payload,

      case StatusCode of
        200 ->
          ParsedJson = jsx:decode(list_to_binary(BodyData)),
          [{_, Token}|_] = ParsedJson,
          NewState = State#state{auth_token = Token, client_id = ClientId},
          {reply, ParsedJson, NewState};
        Other when Other > 200 -> 
          {reply, Payload, State}
      end;
    {error, Error} -> 
      {reply, Error, State}
	end.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

authenticate(ClientId, ClientPassword) ->
  AuthStr = base64:encode_to_string(ClientId ++ ":" ++ ClientPassword),
	Path = "/oauth/token",
	Headers = [{"Authorization", "Basic " ++ AuthStr}],
	Body = "{\"grant_type\":\"client_credentials\"}",
  request(post, Path, Headers, Body).

request(post, Path, Headers, Body) ->
  URL = "https://api.sandbox.mangopay.com/v2.01" ++ Path,
	Type = "application/json",
	HTTPOptions = [{ssl,[{versions, ['tlsv1.2']}]}],
	Options = [],
  httpc:request(post, {URL, Headers, Type, Body}, HTTPOptions, Options).

request(get, Path, Headers) ->
  URL = "https://api.sandbox.mangopay.com/v2.01" ++ Path,
	HTTPOptions = [{ssl,[{versions, ['tlsv1.2']}]}],
	Options = [],
  httpc:request(get, {URL, Headers}, HTTPOptions, Options).

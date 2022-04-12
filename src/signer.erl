%%%-------------------------------------------------------------------
%%% @author jaroslav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(signer).
-author("jaroslav").

-behaviour(gen_server).



-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {offset=0}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  ok = gen_server:cast(?SERVER, launch),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(launch, State) ->
  timer:sleep(1000),
  gen_server:cast(?SERVER, read_chat),
  {noreply, State};
handle_cast(read_chat, State) ->
  io:format("all good ~n"),
  Request = utils:build_request(getUpdates, {offset, State#state.offset}),
  Respose = httpc:request(post, Request, [], [{body_format, binary}]),
  case utils:parse_response(getUpdates, Respose) of
    empty -> {noreply, State};
    [HeadData|TailData] ->
      [gen_server:cast(?SERVER, {sign_apk, X}) || X <- [HeadData|TailData]],
      gen_server:cast(?SERVER, read_chat),
      {noreply, State}
  end;
handle_cast({sign_apk, Data}, State) ->
  {Uid, _, _, _} = Data,
%%  UrlFile = "http://example.com/app-release.apk",
%%  io:format(getNameFile(UrlFile)),
%%  File = getNameFile(UrlFile),
%%  os:cmd("mv resource/" ++ File ++ " resource/sign-app-release.apk"),
%%  gen_server:cast(?SERVER, read_chat),
  {noreply, State#state{offset = Uid + 1}}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

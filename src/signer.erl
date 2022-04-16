%%%-------------------------------------------------------------------
%%% @author jaroslav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(signer).
-author("jaroslav").

-behaviour(gen_server).

-include("sign_data.hrl").

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
  Request = utils:build_post_request(getUpdates, {offset, State#state.offset}),
  Response = httpc:request(post, Request, [], [{body_format, binary}]),
  case utils:parse_response(getUpdates, Response) of
    [HeadData|TailData] -> [gen_server:cast(?SERVER, {handle_message, X}) || X <- [HeadData|TailData]];
    _Else -> io:format("repeat after 1 second ~n"), timer:sleep(1000)
  end,
  gen_server:cast(?SERVER, read_chat),
  {noreply, State};
handle_cast({handle_message, {Uid, ChatId, Text}}, State) ->
  httpc:request(utils:build_url(sendMessage, {chat_id, ChatId, text, Text})),
  {noreply, State#state{offset = Uid + 1}};
handle_cast({handle_message, Data = {Uid, _ChatId, _FileName, _FileId}}, State) ->
  handle_cast({download_apk, Data}, State),
  handle_cast({sign_apk, Data}, State),
  handle_cast({push_apk, Data}, State),
  {noreply, State#state{offset = Uid + 1}};
handle_cast({download_apk, Data}, State) ->
  {Uid, ChatId, FileName, FileId} = Data,
  Request = utils:build_url(getFile, FileId),
  Response = httpc:request(get, {Request, []}, [], [{body_format, binary}]),
  FilePath = utils:parse_response(getFile, Response),
  UrlDownload = utils:build_url(file_download, FilePath),
  httpc:request(get, {UrlDownload, []}, [], [{stream, binary:bin_to_list(FileName)}]),
  httpc:request(utils:build_url(sendMessage, {chat_id, ChatId, text, <<"Downloaded ", FileName/binary>>})),
  {noreply, State#state{offset = Uid + 1}};
handle_cast({sign_apk, Data}, State) ->
  {Uid, ChatId, FileNameBin, FileId} = Data,
  FileName = binary:bin_to_list(FileNameBin),
  os:cmd("cd resources"),
%%  {ok, IODevice} = file:open("sign_data.txt", [read]),
%%  {ok, "key-alias: " ++ KeyAliasWith} = file:read_line(IODevice),
  os:cmd("mkdir folder_" ++ Uid),
  FolderFile = "folder_" ++ Uid,
  os:cmd("mv ../" ++ FileName ++ " " ++ FolderFile),
  SignFileName = "sign-" ++ FileName,
  os:cmd("./apksigner sign --ks " ++ ?KEYSTORE ++ " --ks-key-alias " ++ ?KEY_ALIAS ++ " --ks-pass " ++ ?PASSWORD ++ " --out " ++ SignFileName ++ " " ++ FileName),
  os:cmd("mv " ++ SignFileName ++ " " ++ FolderFile),
  os:cmd("cd ../"),
  httpc:request(utils:build_url(sendMessage, {chat_id, ChatId, text, <<"File processed ", FileNameBin/binary>>})),
  {noreply, State#state{offset = Uid + 1}};
handle_cast({push_apk, Data}, State) ->
  {Uid, ChatId, FileNameBin, FileId} = Data,
  FileName = binary:bin_to_list(FileNameBin),
  {ok, Dir} = file:get_cwd(),
  PathFile = Dir ++ "/resources/folder_" ++ Uid ++ "/sign" ++ FileName,
  utils:sendDocument(ChatId, PathFile),
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

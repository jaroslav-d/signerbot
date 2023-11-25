%%%-------------------------------------------------------------------
%%% @author jaroslav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(signer).
-author("jaroslav").

-behaviour(gen_server).

-include("params_bot.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {offset=0, auth_chats=[], key_alias, sign_password, keystore}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  io:setopts([{encoding, unicode}]),
  {ok, BinaryAuthChats} = file:read_file("auth_chats.json"),
  [{<<"chats">>, AuthChats}] = jsx:decode(BinaryAuthChats),
  c:cd("resources"),
  {ok, BinarySignData} = file:read_file("sign_data.json"),
  [{_, BinKeyAlias}, {_, BinPassword}, {_, BinKeyStore}] = jsx:decode(BinarySignData),
  {KeyAlias, Password, KeyStore} =
    {
      binary_to_list(BinKeyAlias),
      binary_to_list(BinPassword),
      binary_to_list(BinKeyStore)
    },
  c:cd("../"),
  ok = gen_server:cast(?SERVER, launch),
  {ok, #state{
    auth_chats = AuthChats,
    key_alias = KeyAlias,
    sign_password = Password,
    keystore = KeyStore}
  }.

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
handle_cast({handle_message, {Uid, ChatId, ?PASSWORD}}, State) ->
  AuthChats = State#state.auth_chats,
  NewBinaryAuthChats = jsx:encode([{<<"chats">>, AuthChats ++ [ChatId]}]),
  ok = file:write_file("auth_chats.json", NewBinaryAuthChats),
  Message = {chat_id, ChatId, text, <<"вы прошли аутентификацию"/utf8>>},
  Request = utils:build_post_request(sendMessage, Message),
  httpc:request(post, Request, [], [{body_format, binary}]),
  {noreply, State#state{offset = Uid + 1, auth_chats = AuthChats ++ [ChatId]}};
handle_cast({handle_message, {Uid, ChatId, FileLinkBin = <<"http"/utf8, _/binary>>}}, State) ->
  handle_cast({sign_process, {FileLinkBin}, {Uid, ChatId}}, State);
handle_cast({handle_message, {Uid, ChatId, Text}}, State) ->
  Message = {chat_id, ChatId, text, Text},
  Request = utils:build_post_request(sendMessage, Message),
  httpc:request(post, Request, [], [{body_format, binary}]),
  {noreply, State#state{offset = Uid + 1}};
handle_cast({handle_message, {Uid, ChatId, FileNameBin, FileId}}, State) ->
  handle_cast({sign_process, {FileNameBin, FileId}, {Uid, ChatId}}, State);
handle_cast({sign_process, Data, {Uid, ChatId} }, State) ->
  AuthChats = State#state.auth_chats,
  AuthenticatedData = [ X || X <- AuthChats, X =:= ChatId],
  handle_cast({sign_process, Data, {Uid, ChatId, AuthenticatedData} }, State);
handle_cast({sign_process, _, {Uid, ChatId, []}}, State) ->
  Message = {chat_id, ChatId, text, <<"отказано в доступе"/utf8>>},
  Request = utils:build_post_request(sendMessage, Message),
  httpc:request(post, Request, [], [{body_format, binary}]),
  {noreply, State#state{offset = Uid + 1}};
handle_cast({sign_process, {FileLinkBin}, {Uid, ChatId, _AuthenticatedData}}, State) ->
  handle_cast({download_link_apk, {Uid, ChatId, FileLinkBin}}, State);
handle_cast({download_link_apk, {Uid, ChatId, FileLinkBin}}, State) ->
  FileLink = unicode:characters_to_list(FileLinkBin),
  FileName = utils:get_name_file(FileLink),
  FileNameBin = unicode:characters_to_binary(FileName),
  Query = "curl -v -u " ++ ?REPO_LOGIN ++ ":" ++ ?REPO_PASSWORD ++ " -X GET "
    ++ FileLink ++ " --output " ++ FileName,
  os:cmd(Query),
  Message = {chat_id, ChatId, text, <<"Downloaded ", FileNameBin/binary>>},
  RequestSendMessage = utils:build_post_request(sendMessage, Message),
  httpc:request(post, RequestSendMessage, [], [{body_format, binary}]),
  handle_cast({sign_apk, {Uid, ChatId, FileNameBin}}, State),
  handle_cast({push_apk, {Uid, ChatId, FileNameBin}}, State);
handle_cast({sign_process, {FileNameBin, FileId}, {Uid, ChatId, _AuthenticatedData}}, State) ->
  handle_cast({download_apk, {Uid, ChatId, FileNameBin, FileId}}, State);
handle_cast({download_apk, {Uid, ChatId, FileNameBin, FileId} }, State) ->
  RequestGetFile = utils:build_url(getFile, FileId),
  ResponseGetFile = httpc:request(get, {RequestGetFile, []}, [], [{body_format, binary}]),
  FilePath = utils:parse_response(getFile, ResponseGetFile),
  handle_cast({download_apk, {Uid, ChatId, FileNameBin}, FilePath}, State);
handle_cast({download_apk, {Uid, ChatId, _FileNameBin}, empty}, State) ->
  Message = {chat_id, ChatId, text, <<"размер файла не должен превышать 20 мб"/utf8>>},
  Request = utils:build_post_request(sendMessage, Message),
  httpc:request(post, Request, [], [{body_format, binary}]),
  {noreply, State#state{offset = Uid + 1}};
handle_cast({download_apk, Data = {_Uid, ChatId, FileNameBin}, FilePath}, State) ->
  UrlDownload = utils:build_url(file_download, FilePath),
  FileName = unicode:characters_to_list(FileNameBin),
  httpc:request(get, {UrlDownload, []}, [], [{stream, FileName}]),
  Message = {chat_id, ChatId, text, <<"Downloaded ", FileNameBin/binary>>},
  RequestSendMessage = utils:build_post_request(sendMessage, Message),
  httpc:request(post, RequestSendMessage, [], [{body_format, binary}]),
  handle_cast({sign_apk, Data}, State),
  handle_cast({push_apk, Data}, State);
handle_cast({sign_apk, Data}, State) ->
  {Uid, ChatId, FileNameBin} = Data,
  FileName = "'" ++ unicode:characters_to_list(FileNameBin) ++ "'",
  c:cd("resources"),
  {KeyAlias, Password, KeyStore} = {State#state.key_alias, State#state.sign_password, State#state.keystore},
  io:format("~p ~p ~p ~n", [KeyAlias, Password, KeyStore]),
  FolderFile = "folder_" ++ integer_to_list(Uid),
  os:cmd("mkdir " ++ FolderFile),
  os:cmd("mv ../" ++ FileName ++ " ."),
  SignFileName = "'sign-" ++ unicode:characters_to_list(FileNameBin) ++ "'",
  os:cmd("./apksigner sign --ks " ++ KeyStore ++ " --ks-key-alias " ++ KeyAlias ++ " --ks-pass pass:" ++ Password ++ " --out " ++ SignFileName ++ " " ++ FileName),
  os:cmd("mv " ++ FileName ++ " " ++ FolderFile),
  os:cmd("mv " ++ SignFileName ++ " " ++ FolderFile),
  os:cmd("rm " ++ SignFileName ++ ".idsig"),
  c:cd("../"),
  Message = {chat_id, ChatId, text, <<"File processed ", FileNameBin/binary>>},
  Request = utils:build_post_request(sendMessage, Message),
  httpc:request(post, Request, [], [{body_format, binary}]),
  {noreply, State#state{offset = Uid + 1}};
handle_cast({push_apk, Data}, State) ->
  {Uid, ChatId, FileNameBin} = Data,
  SignFileName = "/'sign-" ++ unicode:characters_to_list(FileNameBin) ++ "'",
  {ok, Dir} = file:get_cwd(),
  PathFile = Dir ++ "/resources/folder_" ++ integer_to_list(Uid) ++ SignFileName,
  utils:send_document(ChatId, PathFile),
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

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
  FileLink = unicode:characters_to_list(FileLinkBin),
  FileName = utils:get_name_file(FileLink),
  FileNameBin = unicode:characters_to_binary(FileName),
  SignFileName = "'sign-" ++ unicode:characters_to_list(FileNameBin) ++ "'",
  FolderFile = "folder_" ++ integer_to_list(Uid),
  ForSigner = {State#state.key_alias, State#state.sign_password, State#state.keystore},
  Handlers = [
    fun() -> access_check(ChatId, State#state.auth_chats) end,
    fun() -> {text, <<"Downloading ... "/utf8>>} end,
    fun() -> download_link_apk(FileLink, FileName, FileNameBin) end,
    fun() -> sign_apk(FileName, FileNameBin, SignFileName, FolderFile, ForSigner) end,
    fun() -> push_apk(ChatId, FolderFile, SignFileName) end
  ],
  Messenger = fun(Text) ->
    Message = {chat_id, ChatId, text, Text},
    Request = utils:build_post_request(sendMessage, Message),
    httpc:request(post, Request, [], [{body_format, binary}])
              end,
  ok = messenger_chain(Messenger, chain_of_responsibility(Handlers)),
  {noreply, State#state{offset = Uid + 1}};
handle_cast({handle_message, {Uid, ChatId, Text}}, State) ->
  Message = {chat_id, ChatId, text, Text},
  Request = utils:build_post_request(sendMessage, Message),
  httpc:request(post, Request, [], [{body_format, binary}]),
  {noreply, State#state{offset = Uid + 1}};
handle_cast({handle_message, {Uid, ChatId, FileNameBin, FileId}}, State) ->
  FileName = unicode:characters_to_list(FileNameBin),
  FolderFile = "folder_" ++ integer_to_list(Uid),
  SignFileName = "'sign-" ++ unicode:characters_to_list(FileNameBin) ++ "'",
  ForSigner = {State#state.key_alias, State#state.sign_password, State#state.keystore},
  Handlers = [
    fun() -> access_check(ChatId, State#state.auth_chats) end,
    fun() -> {text, <<"Downloading ... "/utf8>>} end,
    fun() -> download_apk(FileName, FileNameBin, FileId) end,
    fun() -> sign_apk(FileName, FileNameBin, SignFileName, FolderFile, ForSigner) end,
    fun() -> push_apk(ChatId, FolderFile, SignFileName) end
  ],
  Messenger = fun(Text) ->
    Message = {chat_id, ChatId, text, Text},
    Request = utils:build_post_request(sendMessage, Message),
    httpc:request(post, Request, [], [{body_format, binary}])
              end,
  ok = messenger_chain(Messenger, chain_of_responsibility(Handlers)),
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

messenger_chain(Messenger, {Message, finish}) ->
  Messenger(Message),
  ok;
messenger_chain(Messenger, {Message, ChainHandlers}) ->
  Messenger(Message),
  messenger_chain(Messenger, chain_of_responsibility(ChainHandlers));
messenger_chain(_, _) -> ok.


chain_of_responsibility([Handler | Others]) ->
  Result = Handler(),
  chain_of_responsibility(Result, Others).
chain_of_responsibility({exception, Message}, _) -> {Message, finish};
chain_of_responsibility({text, Message}, []) -> {Message, finish};
chain_of_responsibility({text, Message}, Handlers) -> {Message, Handlers};
chain_of_responsibility(_, []) -> ok;
chain_of_responsibility(_, Handlers) -> chain_of_responsibility(Handlers).

access_check(ChatId, AuthChats) ->
  AuthenticatedData = [ X || X <- AuthChats, X =:= ChatId],
  access_check(AuthenticatedData).
access_check([]) -> {exception, <<"отказано в доступе"/utf8>>};
access_check(_AuthenticatedData) -> ok.

download_link_apk(FileLink, FileName, FileNameBin) ->
  Query = "curl -v -u " ++ ?REPO_LOGIN ++ ":" ++ ?REPO_PASSWORD ++ " -X GET "
    ++ FileLink ++ " --output " ++ FileName,
  os:cmd(Query),
  {text, <<"Downloaded ", FileNameBin/binary>>}.

download_apk(FileName, FileNameBin, FileId) ->
  RequestGetFile = utils:build_url(getFile, FileId),
  ResponseGetFile = httpc:request(get, {RequestGetFile, []}, [], [{body_format, binary}]),
  FilePath = utils:parse_response(getFile, ResponseGetFile),
  download_apk(FileName, FileNameBin, file_path, FilePath).
download_apk(_, _, file_path, empty) ->
  {exception, <<"размер файла не должен превышать 20 мб"/utf8>>};
download_apk(FileName, FileNameBin, file_path, FilePath) ->
  UrlDownload = utils:build_url(file_download, FilePath),
  httpc:request(get, {UrlDownload, []}, [], [{stream, FileName}]),
  {text, <<"Downloaded ", FileNameBin/binary>>}.

sign_apk(FileName, FileNameBin, SignFileName, FolderFile, {KeyAlias, Password, KeyStore}) ->
  c:cd("resources"),
  os:cmd("mkdir " ++ FolderFile),
  os:cmd("mv ../" ++ FileName ++ " ."),
  os:cmd("./apksigner sign --ks " ++ KeyStore ++ " --ks-key-alias "
    ++ KeyAlias ++ " --ks-pass pass:" ++ Password ++ " --out " ++ SignFileName ++ " " ++ FileName),
  os:cmd("mv " ++ FileName ++ " " ++ FolderFile),
  os:cmd("mv " ++ SignFileName ++ " " ++ FolderFile),
  os:cmd("rm " ++ SignFileName ++ ".idsig"),
  c:cd("../"),
  {text, <<"File processed ", FileNameBin/binary>>}.

push_apk(ChatId, FolderFile, SignFileName) ->
  {ok, Dir} = file:get_cwd(),
  PathFile = Dir ++ "/resources/" ++ FolderFile ++ "/" ++ SignFileName,
  utils:send_document(ChatId, PathFile),
  ok.
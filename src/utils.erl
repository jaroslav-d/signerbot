%%%-------------------------------------------------------------------
%%% @author jaroslav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Apr 2022 23:20
%%%-------------------------------------------------------------------
-module(utils).
-author("jaroslav").
-include("params_bot.hrl").

%% API
-export([build_url/2, build_post_request/2, parse_response/2]).
-export([sendDocument/2]).

-define(URL, "https://api.telegram.org/bot").
-define(URL_FILE, "https://api.telegram.org/file/bot").

sendDocument(ChatId, PathFile) ->
  os:cmd("curl -v -F \"chat_id=" ++ integer_to_list(ChatId) ++ "\" -F document=@" ++ PathFile ++ " "
    ++ ?URL ++ ?TOKEN ++ "/sendDocument").

build_url(file_download, FilePath) ->
  ?URL_FILE ++ ?TOKEN ++ "/" ++ binary:bin_to_list(FilePath);
build_url(sendMessage, {chat_id, ChatId, text, Text}) ->
%%  todo get not working
  ?URL ++ ?TOKEN ++ "/sendMessage?chat_id=" ++ integer_to_list(ChatId) ++ "&text=" ++ binary:bin_to_list(Text);
build_url(getFile, FileId) ->
  ?URL ++ ?TOKEN ++ "/getFile?file_id=" ++ binary:bin_to_list(FileId);
build_url(getUpdates, {offset, Offset}) ->
  ?URL ++ ?TOKEN ++ "/getUpdates?offset=" ++ integer_to_list(Offset).


build_post_request(sendMessage, {chat_id, ChatId, text, Text}) ->
  Url = ?URL ++ ?TOKEN ++ "/sendMessage",
  build_post_request({url, Url}, [{chat_id, ChatId}, {text, Text}]);
build_post_request(getFile, FileId) ->
  Url = ?URL ++ ?TOKEN ++ "/getFile",
  build_post_request({url, Url}, [{file_id, FileId}]);
build_post_request(getUpdates, {offset, Offset}) ->
  Url = ?URL ++ ?TOKEN ++ "/getUpdates",
  build_post_request({url, Url}, [{offset, Offset}]);
build_post_request({url, Url}, [HParams|TParams]) ->
  Headers = [{"Content-Type","application/json"}],
  ContentType = "application/json",
  {Url, Headers, ContentType, jsx:encode([HParams|TParams])}.


parse_response(getUpdates, {ok, {_, _, Body}}) ->
  [{<<"ok">>,true}, {<<"result">>, Updates}] = jsx:decode(Body),
  parse_response(getUpdates, {updates, Updates, data, []});
parse_response(getUpdates, {updates, [], data, []}) -> empty;
parse_response(getUpdates, {updates, [], data, Data}) -> Data;
parse_response(getUpdates, {updates, [HeadU|TailU], data, Data}) ->
  Element = parse_response(update, HeadU),
  parse_response(getUpdates, {updates, TailU, data, Data ++ Element});
parse_response(getUpdates, _Else) -> empty;

parse_response(getFile, {ok, {_, _, Body}}) ->
  [{<<"ok">>,true}, {<<"result">>, Updates}] = jsx:decode(Body),
  {_, FilePath} = find_rec(Updates, <<"file_path">>),
  FilePath;


parse_response(update, Update) ->
  Message = find_rec(Update, <<"message">>),
  parse_response(message, {Message, Update});
%% if not found record
parse_response(message, {not_found, _Update}) -> [];
parse_response(text, {not_found, Update}) ->
  Document = find_rec(Update, <<"document">>),
  parse_response(document, {Document, Update});
parse_response(document, {not_found, _Update}) -> [];
%% if found record
parse_response(message, {_, Update}) ->
  Text = find_rec(Update, <<"text">>),
  parse_response(text, {Text, Update});
parse_response(text, {_, Update}) ->
  {_, Uid} = find_rec(Update, <<"update_id">>),
  Chat = find_rec(Update, <<"chat">>),
  {_, ChatId} = find_rec(Chat, <<"id">>),
  {_, Text} = find_rec(Update, <<"text">>),
  [{Uid, ChatId, Text}];
parse_response(document, {_, Update}) ->
  {_, Uid} = find_rec(Update, <<"update_id">>),
  Chat = find_rec(Update, <<"chat">>),
  {_, ChatId} = find_rec(Chat, <<"id">>),
  {_, FileName} = find_rec(Update, <<"file_name">>),
  {_, FileId} = find_rec(Update, <<"file_id">>),
  [{Uid, ChatId, FileName, FileId}];
parse_response(_Key, _Update) -> [].


find_rec({Key, Value}, Key) -> {Key, Value};
find_rec([{Key, Value}|_Tail], Key) -> {Key, Value};
find_rec({_, [Head|Tail]}, Key) -> find_rec([Head|Tail], Key);
find_rec([not_found|Tail], Key) -> find_rec(Tail, Key);
find_rec([Head|Tail], Key) ->
  ResultHead = find_rec(Head, Key),
  find_rec([ResultHead|Tail], Key);
find_rec(_Arg, _Key) -> not_found.


getNameFile([H|T]) -> getNameFile([H],T).
getNameFile(_H, [47|T]) -> getNameFile([],T);
getNameFile(H, [H1|[]]) -> H ++ [H1];
getNameFile(H, [H1|T]) -> getNameFile(H ++ [H1], T).
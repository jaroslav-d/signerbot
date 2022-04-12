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
-include("token.hrl").

%% API
-export([build_request/2, parse_response/2]).

-define(URL, "https://api.telegram.org/bot").

build_request(getUpdates, Params) ->
  Url = ?URL ++ ?TOKEN ++ "/" ++ "getUpdates",
  Headers = [{"Content-Type","application/json"}],
  ContentType = "application/json",
  {Url, Headers, ContentType, jsx:encode([Params])}.

parse_response(getUpdates, {ok, {_, _, Body}}) ->
  [{<<"ok">>,true}, {<<"result">>, Updates}] = jsx:decode(Body),
  parse_response(getUpdates, {updates, Updates, data, []});
parse_response(getUpdates, {updates, [], data, []}) -> empty;
parse_response(getUpdates, {updates, [], data, Data}) -> Data;
parse_response(getUpdates, {updates, [HeadU|TailU], data, Data}) ->
  Element = parse_response(update, HeadU),
  parse_response(getUpdates, {updates, TailU, data, Data ++ Element});
parse_response(getUpdates, _Else) -> empty;

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
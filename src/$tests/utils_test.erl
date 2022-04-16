%%%-------------------------------------------------------------------
%%% @author jaroslav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2022 22:38
%%%-------------------------------------------------------------------
-module(utils_test).
-author("jaroslav").

-include_lib("eunit/include/eunit.hrl").

response(Body) -> {ok, {ok, ok, Body}}.

build_request_test() ->
  ?assertMatch({_Url, _Headers, _ContentType, _Params}, utils:build_post_request(getUpdates, {offset, 0})),
  ?assertMatch({_Url, _Headers, _ContentType, _Params}, utils:build_post_request(getUpdates, [{offset, 0}])),
  ?assertMatch({_Url, _Headers, _ContentType, <<"{\"offset\":0}">>}, utils:build_post_request(getUpdates, {offset, 0})).

parse_response_test() ->
  BodyOne = <<"{\"ok\": true, \"result\": []}">>,
  ?assertEqual(empty, utils:parse_response(getUpdates, response(BodyOne))),
  BodyTwo = <<"{
    \"ok\": true,
    \"result\": [
      {
        \"update_id\": 1234,
        \"message\": {
          \"message_id\": 8989,
          \"chat\": {
            \"id\": 3
          }
        }
      }
    ]
  }">>,
  ?assertEqual(empty, utils:parse_response(getUpdates, response(BodyTwo))),
  BodyThree = <<"{
    \"ok\": true,
    \"result\": [
      {
        \"update_id\": 629258088,
        \"message\": {
          \"message_id\": 705,
          \"from\": {
            \"id\": 530167187,
            \"is_bot\": false,
            \"first_name\": \"Yaroslav\",
            \"last_name\": \"Drozdov\",
            \"language_code\": \"en\"
          },
          \"chat\": {
            \"id\": 530167187,
            \"first_name\": \"Yaroslav\",
            \"last_name\": \"Drozdov\",
            \"type\": \"private\"
          },
          \"date\": 1649712850,
          \"text\": \"у сука\"
        }
      }
    ]
  }">>,
  CheckThree = [{629258088, 530167187, <<"у сука">>}],
  DecodedThreeBody = jsx:decode(BodyThree),
  ?assertEqual(CheckThree, utils:parse_response(getUpdates, response(BodyThree))),
  ?assertEqual(CheckThree, utils:parse_response(update, DecodedThreeBody)),
  ?assertEqual([], utils:parse_response(text, {not_found, DecodedThreeBody})),
  ?assertEqual(CheckThree, utils:parse_response(text, {ok, DecodedThreeBody})).
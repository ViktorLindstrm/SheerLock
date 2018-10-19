%% Feel free to use, reuse and abuse the code in this file.

%% @doc GET echo handler.
-module(userinfo_handler).

-export([init/2]).
-record(user, {id,
               username         :: atom(),
               name             :: list(),
               password         :: list(),
               consents         :: list(),
               token = undefined,
               code = undefined :: 'undefined' | list()
              }).

init(Req0, Opts) ->
    Req = case cowboy_req:parse_header(<<"authorization">>, Req0) of
              {bearer, Token} ->
                  case idp_mng:get_userinfo(binary_to_list(Token)) of
                      {ok,User} -> 
                          UserId = User#user.username,
                          Response = atom_to_binary(UserId,utf8),
                          cowboy_req:reply(200, #{
                            <<"content-type">> => <<"application/json; charset=utf-8">>
                           },Response, Req0);

                      {error,_} ->
                          cowboy_req:reply(405, #{}, <<>>, Req0)
                  end;
              E ->
                  cowboy_req:reply(405, #{}, <<>>, Req0)
          end,
    {ok, Req, Opts}.


